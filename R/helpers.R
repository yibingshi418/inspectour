# =============================================================================
#  Packages, app-wide constants, and small stateless helpers shared by
#  ui.R and server.R -- nothing in here depends on the other R/ files.
#  Sourced automatically by Shiny before ui.R and server.R (see app.R);
#  alphabetically first among files that matter, so its functions (e.g.
#  field_block(), role_row()) are already defined by the time ui.R calls
#  them while building the page.
# =============================================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# Shiny caps uploads at 5 MB by default, which is too small for audio.
# Note that large recordings are better pointed at as a FOLDER than uploaded:
# nothing is copied, and only small extracted clips are ever served.
options(shiny.maxRequestSize = 200 * 1024^2)

AUDIO_EXT <- "\\.(wav|mp3|ogg|flac|m4a)$"
TG_EXT    <- "\\.(TextGrid|textgrid)$"

# how much audio to keep either side of a token's interval, so that onsets
# and offsets are not cut abruptly -- tone onsets sit right at the boundary
CLIP_PAD <- 0.05

# Where the bundled "Try it with sample data" file lives, relative to the
# app's own root. Shiny sets the working directory to the app root for the
# life of the app, so the plain path is what actually runs in production.
# The second candidate exists only so this repo's own test scripts (which
# run from one level up, alongside the app folder) can exercise the sample
# data without needing to launch the app itself; it never matters when the
# app is actually run.
sample_data_path <- function() {
  if (file.exists("data/sample_data.csv.gz")) "data/sample_data.csv.gz"
  else "inspectour/data/sample_data.csv.gz"
}

# Bundled recordings (and any TextGrids) for "Try it with sample audio".
# They match the sample dataset's token ids. Same two-candidate lookup as
# sample_data_path(); "" when the folder is absent, so the button can say so.
sample_audio_dir <- function() {
  for (d in c("data/sample_audio", "inspectour/data/sample_audio"))
    if (dir.exists(d)) return(normalizePath(d))
  ""
}
# How the bundled recording names a token: speaker from the file name
# (S2_...), item number from the first part of the TextGrid label, and only
# the two-interval (disyllabic) tokens. Preset when the sample audio loads.
SAMPLE_AUDIO_MATCH  <- c("speaker", "item_id")
SAMPLE_AUDIO_NPARTS <- "2"

# ---- defaults matching your existing data --------------------------------
DEFAULTS <- list(
  time    = "time",
  f0      = "norm_f0",
  # no token default: the column is found by name (see find_token_id_col)
  # and left empty when the dataset has none
  speaker = "speaker",
  colour  = "item_id",
  hover   = c("speaker", "citation_tone", "item_id", "token")
)

# fixed qualitative palette for native-plotly colour-by (ColorBrewer Dark2)
QUAL_PALETTE <- c("#4E79A7", "#F28E2B", "#E15759", "#59A14F", "#B07AA1",
                  "#76B7B2", "#EDC948", "#FF9DA7", "#9C755F", "#BAB0AC")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

pick <- function(choices, preferred, fallback = choices[1]) {
  for (p in preferred) {
    if (p %in% choices) return(p)
  }
  for (p in preferred) {
    idx <- match(tolower(p), tolower(choices))
    if (!is.na(idx)) return(choices[idx])
  }
  for (p in preferred) {
    hit <- choices[grepl(p, choices, ignore.case = TRUE, fixed = TRUE) |
                     sapply(choices, function(ch) grepl(ch, p, ignore.case = TRUE, fixed = TRUE))]
    if (length(hit)) return(hit[1])
  }
  fallback
}

# ---- finding, and naming, the token id column -----------------------------
# Datasets spell the same idea differently: token_id, Token ID, token-id,
# tokenID. Compare on letters and digits alone so all of them land on the
# same string. Nothing matching returns "", which leaves the control empty:
# selecting an arbitrary column would look like a considered choice and be
# wrong silently, whereas an empty control asks the question out loud.
squash_name <- function(x) tolower(gsub("[^A-Za-z0-9]+", "", x))

find_token_id_col <- function(cols, want = "tokenid") {
  if (!length(cols)) return("")
  sq <- squash_name(cols)
  hit <- which(sq == want)
  if (length(hit)) return(cols[hit[1]])
  hit <- which(grepl(want, sq, fixed = TRUE))
  if (length(hit)) return(cols[hit[1]])
  ""
}

# A generated column must not quietly overwrite one the dataset already has.
free_name <- function(base, taken) {
  if (!(base %in% taken)) return(base)
  i <- 2L
  while (paste0(base, "_", i) %in% taken) i <- i + 1L
  paste0(base, "_", i)
}

field_block <- function(label, help = NULL, widget, extra_class = NULL) {
  tags$div(class = paste(c("field-block", extra_class), collapse = " "),
           tags$label(class = "control-label", label),
           if (!is.null(help)) tags$div(class = "field-help", help),
           widget
  )
}

sub_label <- function(text) tags$div(class = "subsection-label", text)

# One role per row: name and a short hint on the left, the control on the
# right. Laying these out side by side in three columns made the controls sit
# at different heights whenever one had help text and another did not, which
# read as clutter rather than structure.
# ---- time: continuous is fine, but averaging needs a shared grid ----------
# Time is read as a number and plotted continuously, so any numeric column
# works for looking at individual contours. Group means are different: they
# average across tokens AT THE SAME x, so tokens must land on the same time
# values. A time-normalised grid does; raw tracker frames, which start at
# their own offsets, do not -- there every point averages a single token and
# the mean line is just the tokens redrawn, with no ribbon and no warning.
time_grid_is_shared <- function(t, keys) {
  ok <- !is.na(t)
  if (!any(ok)) return(TRUE)
  n_pts <- as.integer(table(keys[ok]))
  length(unique(t[ok])) <= max(n_pts)
}

# Put every token on 0..1 of its own span, keeping its own sample points, so
# contours are comparable without pretending to a precision they lack.
time_to_relative <- function(t, keys) {
  spl <- split(seq_along(t), keys)
  out <- t
  for (ix in spl) {
    r <- range(t[ix], na.rm = TRUE)
    out[ix] <- if (diff(r) > 0) (t[ix] - r[1]) / diff(r) else 0
  }
  out
}

# For means only: resample each token onto one common grid by interpolation.
resample_tokens <- function(df, k) {
  parts <- lapply(split(seq_len(nrow(df)), df$tok_key), function(ix) {
    g <- df[ix, , drop = FALSE]
    g <- g[order(g$t_num), , drop = FALSE]
    keep <- !is.na(g$t_num) & !is.na(g$f0_num)
    g <- g[keep, , drop = FALSE]
    if (nrow(g) < 2) return(NULL)
    xx <- seq(0, 1, length.out = k)
    out <- g[rep(1L, k), , drop = FALSE]      # carries the token's metadata
    out$t_num  <- xx
    out$f0_num <- stats::approx(g$t_num, g$f0_num, xout = xx, ties = mean)$y
    out
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(df[0, , drop = FALSE])
  do.call(rbind, parts)
}

role_row <- function(name, hint, widget) {
  tags$div(class = "role-row",
           tags$div(class = "role-key",
                    tags$div(class = "role-name", name),
                    if (!is.null(hint)) tags$div(class = "role-hint", hint)),
           tags$div(class = "role-val", widget))
}

brand_name <- function() {
  HTML("<span class='brand-name'><span class='word1'>Inspec</span><em>tour</em></span>")
}

numbered_card <- function(intro, items) {
  tags$div(class = "numbered-card",
           tags$p(class = "numbered-card-intro", intro),
           lapply(seq_along(items), function(i) {
             tags$div(class = "numbered-item",
                      tags$div(class = "numbered-badge", i),
                      tags$div(class = "numbered-content", items[[i]])
             )
           })
  )
}
