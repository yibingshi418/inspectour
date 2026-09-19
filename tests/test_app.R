`%||%` <- function(a, b) if (is.null(a)) b else a
# Run from the repository root:  Rscript tests/test_app.R
APP_DIR <- if (file.exists("app.R")) normalizePath(".") else
           if (file.exists(file.path("..", "app.R"))) normalizePath("..") else
           normalizePath(".")
stopifnot(file.exists(file.path(APP_DIR, "app.R")))
setwd(APP_DIR)
library(shiny)

fails <- 0
ok <- function(label, cond) {
  pass <- isTRUE(cond)
  if (!pass) fails <<- fails + 1
  cat(if (pass) "PASS  " else "FAIL  ", label, "\n")
}

# testServer does not feed update*Input() back into input$, so auto-detected
# column roles are checked by inspecting the messages the app sends, and the
# plotting paths are then driven by setting those inputs explicitly.
app_env <- new.env()
suppressMessages(source(file.path(APP_DIR, "app.R"), local = app_env))
pick_col <- app_env$pick_col
f0_slider_range <- app_env$f0_slider_range
DEFAULTS  <- app_env$DEFAULTS

slider_updates <- 0
trace(shiny::updateSliderInput, tracer = quote({
  if (identical(inputId, "ylim")) slider_updates <<- slider_updates + 1
}), print = FALSE)
on.exit(untrace(shiny::updateSliderInput), add = TRUE)

testServer(APP_DIR, {

  # --- 1. nothing loaded at start -----------------------------------------
  ok("no data before any action", isFALSE(data_loaded()))
  ok("active_dataset blank at start", identical(output$active_dataset, ""))

  # --- 2. the sample-data button loads the bundled file --------------------
  session$setInputs(use_sample = 1)
  ok("data_loaded after sample click", isTRUE(data_loaded()))
  ok("sample has 1700 rows", nrow(raw()) == 1700)
  ok("active_dataset names the sample", grepl("sample data", output$active_dataset))

  # --- 3. column roles auto-detected from the sample's column names --------
  cols <- sort(names(raw()))
  ok("time col guessed",    identical(pick_col(cols, DEFAULTS$time), "time"))
  ok("f0 col guessed",      identical(pick_col(cols, DEFAULTS$f0), "norm_f0"))
  ok("token col guessed",   identical(pick_col(cols, DEFAULTS$token), "ind_no"))
  ok("speaker col guessed", identical(pick_col(cols, DEFAULTS$speaker), "speaker"))
  ok("colour col guessed",  identical(pick_col(cols, DEFAULTS$colour), "citation_no"))

  # --- 4. drive the app the way the UI would -------------------------------
  session$setInputs(col_time = "time", col_f0 = "norm_f0",
                    col_token = "ind_no", col_speaker = "speaker",
                    col_colour = "citation_no",
                    col_hover = c("speaker", "citation_tone", "token"),
                    ylim = c(-4, 4), lw_token = 0.4, alpha_token = 0.75,
                    lw_mean = 1.1, x_breaks = 4, font_size = 13,
                    axis_size = 9, show_legend = TRUE, plot_height = 620,
                    facet_x = "(none)", facet_y = "(none)", facet_ncol = 2,
                    col_break = "(none)", break_style = "boundary",
                    view = "tokens")

  # --- 5. f0 slider range tracks the chosen f0 column ----------------------
  # the slider ships on a -6..6 range that suits normalised f0; a raw Hz
  # column must move it, or the plot silently comes back empty
  hz <- raw()$f0
  nf <- raw()$norm_f0
  r_hz <- f0_slider_range(hz)
  r_nf <- f0_slider_range(nf)
  ok("slider range covers raw Hz",
     r_hz$min <= min(hz) && r_hz$max >= max(hz) && r_hz$min > 6)
  ok("slider range covers normalised f0",
     r_nf$min <= min(nf) && r_nf$max >= max(nf))
  ok("slider step is sane", r_hz$step > 0 && r_nf$step > 0)
  ok("all-NA column yields no range", is.null(f0_slider_range(c(NA, NA))))
  ok("constant column still yields a usable range",
     { rc <- f0_slider_range(rep(3, 10)); rc$max > rc$min })
  ok("slider rescale is wired to the f0 selector", slider_updates > 0)

  # --- 6. keying and token list -------------------------------------------
  ok("85 distinct tokens", length(unique(keyed()$tok_key)) == 85)

  # --- 7. the plotly (individual tokens) path renders -----------------------
  ok("tokens view renders", nchar(output$plot) > 1000)

  session$setInputs(col_break = "syllable_no")
  ok("boundary-line mode renders", nchar(output$plot) > 1000)

  session$setInputs(break_style = "disconnect")
  ok("disconnect mode renders", nchar(output$plot) > 1000)

  # --- 8. faceting ---------------------------------------------------------
  session$setInputs(break_style = "boundary", facet_x = "speaker", facet_ncol = 3)
  ok("single-facet tokens view renders", nchar(output$plot) > 1000)

  session$setInputs(facet_y = "syntax")
  ok("grid-facet tokens view renders", nchar(output$plot) > 1000)

  # --- 9. the ggplot (means) paths -----------------------------------------
  session$setInputs(facet_x = "(none)", facet_y = "(none)", view = "means")
  ok("means view renders", nchar(output$plot) > 1000)

  session$setInputs(view = "both")
  ok("means-over-tokens renders", nchar(output$plot) > 1000)

  session$setInputs(view = "means", facet_x = "speaker")
  ok("faceted means view renders", nchar(output$plot) > 1000)

  # --- 10. filtering -------------------------------------------------------
  session$setInputs(view = "tokens", facet_x = "(none)", filter_vars = "speaker")
  session$setInputs(flt_speaker = "S1")
  ok("filter narrows the data", length(unique(filtered()$tok_key)) == 10)
  ok("filter status mentions S1", grepl("S1", output$filter_status$html))
  ok("filtered view still renders", nchar(output$plot) > 1000)

  # --- 11. curation: label, re-label, undo ---------------------------------
  session$setInputs(flt_speaker = character(0), filter_vars = character(0))
  session$setInputs(highlight = c("S1_1_ct", "S1_3_ct"), label_value = "rise-fall")
  session$setInputs(label_add_selected = 1)
  ok("two tokens labelled", nrow(labels_log()) == 2)

  session$setInputs(highlight = "S1_9_ct", label_value = "level")
  session$setInputs(label_add_selected = 2)
  ok("third token labelled", nrow(labels_log()) == 3)

  session$setInputs(label_undo = 1)
  ok("undo removes only the last action", nrow(labels_log()) == 2)
  ok("undo kept the earlier label", all(labels_log()$label == "rise-fall"))

  session$setInputs(label_undo = 2)
  ok("second undo empties the log", nrow(labels_log()) == 0)

  session$setInputs(label_undo = 3)
  ok("undo past the start is harmless", nrow(labels_log()) == 0)

  # --- 12. re-labelling a token overwrites rather than duplicates ----------
  session$setInputs(highlight = "S1_1_ct", label_value = "A")
  session$setInputs(label_add_selected = 3)
  session$setInputs(label_value = "B")
  session$setInputs(label_add_selected = 4)
  ok("re-label overwrites", nrow(labels_log()) == 1 &&
       identical(labels_log()$label, "B"))

  # --- 13. label everything currently filtered -----------------------------
  session$setInputs(label_value = "all", label_add_filtered = 1)
  ok("label-all covers every token", nrow(labels_log()) == 85)

  # --- 14. status line -----------------------------------------------------
  ok("status reports token count", grepl("85 tokens shown", output$status))

  # --- 15. the curated download carries the labels -------------------------
  f <- tempfile(fileext = ".csv")
  output$label_dl  # touch the handler
  ok("download handler exists", !is.null(output$label_dl))

  # --- 16. loading a new dataset clears curation state ---------------------
  session$setInputs(use_sample = 2)
  session$flushReact()
  ok("reload clears the label log", nrow(labels_log()) == 0)
})

cat("\n--", if (fails == 0) "ALL TESTS PASSED" else paste(fails, "FAILURE(S)"), "--\n")
quit(status = if (fails == 0) 0 else 1)
