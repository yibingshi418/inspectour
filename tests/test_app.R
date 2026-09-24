# Run from the repository root:  Rscript tests/test_app.R
#
# Two parts:
#   1. the audio pipeline on the bundled sample recording (TextGrid reader,
#      token grouping, matching to the dataset, clip extraction)
#   2. the server, driven with shiny::testServer: sample data, column
#      auto-detection, every plotting path, and the sample-audio button
APP_DIR <- if (file.exists("app.R")) normalizePath(".") else
           if (file.exists(file.path("..", "app.R"))) normalizePath("..") else
           normalizePath(".")
stopifnot(file.exists(file.path(APP_DIR, "app.R")))
setwd(APP_DIR)
suppressPackageStartupMessages(library(shiny))

fails <- 0
ok <- function(label, cond) {
  pass <- isTRUE(cond)
  if (!pass) fails <<- fails + 1
  cat(if (pass) "PASS  " else "FAIL  ", label, "\n")
}

# the same files Shiny sources before running the app
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE)))
  suppressPackageStartupMessages(source(f))

# ---- 1. audio pipeline ------------------------------------------------------
sd  <- as.data.frame(readr::read_csv(sample_data_path(), show_col_types = FALSE))
ok("sample data loads", nrow(sd) > 0)
ok("sample data has a token id column", nzchar(find_token_id_col(names(sd))))

adir <- sample_audio_dir()
ok("sample audio folder found", nzchar(adir))
wav <- list.files(adir, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
tg  <- list.files(adir, pattern = "\\.TextGrid$", full.names = TRUE, ignore.case = TRUE)
ok("one recording and one TextGrid bundled", length(wav) == 1 && length(tg) == 1)

inf <- wav_info(wav)
ok("wav header parsed", inf$format == 1 && inf$sample_rate > 0 && inf$duration > 0)

iv <- textgrid_read_dir(tg)
ok("TextGrid read", is.data.frame(iv) && nrow(iv) > 0)
ok("TextGrid fits inside the recording", max(iv$xmax) <= inf$duration + 1e-6)

sep <- am_guess_label_sep(iv$label)
tk  <- audio_group_tokens(iv[iv$tier == iv$tier[1], ], key_field = 1,
                          label_sep = sep, chunk = as.integer(SAMPLE_AUDIO_NPARTS))
tk  <- tk[tk$n_parts == as.integer(SAMPLE_AUDIO_NPARTS), , drop = FALSE]
ok("disyllabic tokens grouped", nrow(tk) > 0)

tok_col <- find_token_id_col(names(sd))
tt <- unique(data.frame(tok_key = sd[[tok_col]], sd[, SAMPLE_AUDIO_MATCH]))
tt <- tt[!duplicated(tt$tok_key), ]
res <- audio_build_index(tk, tt, SAMPLE_AUDIO_MATCH,
                         list(speaker = "file:1", item_id = "label:1"),
                         label_sep = sep, loose = TRUE)
n_spk <- length(unique(tt$tok_key[tt$speaker == "S2"]))
n_hit <- length(unique(res$index$tok_key))
ok(sprintf("every S2 token matched to audio (%d/%d)", n_hit, n_spk),
   n_hit == n_spk && n_spk > 0)

out <- tempfile(fileext = ".wav")
r1 <- res$index[1, ]
wav_clip(wav, out, r1$xmin, r1$xmax)
ci <- wav_info(out)
ok("clip written and playable", file.exists(out) && ci$format == 1)
ok("clip has the interval's length",
   abs(ci$duration - (r1$xmax - r1$xmin)) < 2 / inf$sample_rate)

# ---- 2. server ----------------------------------------------------------------
testServer(server, {

  ok("no data before any action", isFALSE(data_loaded()))

  session$setInputs(load_sample = 1)
  ok("data_loaded after sample click", isTRUE(data_loaded()))
  ok("sample rows loaded", nrow(raw()) == nrow(sd))

  cols <- col_info()$cols
  ok("time col guessed",    identical(pick(cols, DEFAULTS$time), "time"))
  ok("f0 col guessed",      identical(pick(cols, DEFAULTS$f0), "norm_f0"))
  ok("speaker col guessed", identical(pick(cols, DEFAULTS$speaker), "speaker"))
  ok("token id col found",  identical(find_token_id_col(cols), "token_id"))

  session$setInputs(col_time = "time", col_f0 = "norm_f0",
                    col_token = "token_id", col_speaker = "speaker",
                    col_colour = "citation_tone", col_break = "(none)",
                    col_hover = c("speaker", "citation_tone", "token"),
                    ylim = c(-4, 4), lw_token = 0.4, alpha_token = 0.75,
                    lw_mean = 1.1, x_breaks = 4, font_size = 13,
                    axis_size = 9, show_legend = TRUE, plot_height = 620,
                    facet_x = "(none)", facet_y = "(none)", facet_ncol = 2,
                    view = "tokens", break_style = "boundary",
                    highlight = character(0), filter_vars = character(0))
  ok("token id accepted", isTRUE(token_id_ready()))
  ok("every token keyed",
     length(unique(keyed()$tok_key)) == length(unique(sd$token_id)))

  draw <- function(label) {
    p <- tryCatch(output$plot, error = function(e) e)
    ok(label, !inherits(p, "error") && is.character(p) && nchar(p) > 0)
    if (inherits(p, "error")) cat("      ", conditionMessage(p), "\n")
  }
  draw("plot: tokens")
  for (v in c("means", "both")) {
    session$setInputs(view = v); draw(paste("plot: view =", v))
  }
  session$setInputs(view = "tokens", facet_x = "speaker");  draw("plot: facet by speaker")
  session$setInputs(facet_y = "syntax");                    draw("plot: facet grid")
  session$setInputs(facet_x = "(none)", facet_y = "(none)",
                    col_break = "syllable_no");             draw("plot: syllable boundary")
  session$setInputs(break_style = "disconnect");            draw("plot: disconnected syllables")

  session$setInputs(load_sample_audio = 1)
  ok("sample audio switched on", isTRUE(use_sample_audio()))
  ok("sample recording listed", !is.null(audio_files_tbl()) && nrow(audio_files_tbl()) == 1)
  ok("sample TextGrid read", !is.null(tg_intervals()) && nrow(tg_intervals()) > 0)
  ok("sample matches on speaker + item_id", identical(match_cols(), SAMPLE_AUDIO_MATCH))
})

cat("\n", if (fails == 0) "All tests passed." else sprintf("%d test(s) FAILED.", fails), "\n")
quit(status = if (fails == 0) 0 else 1)
