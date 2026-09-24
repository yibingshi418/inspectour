# =============================================================================
#  Self-contained Praat TextGrid reader -- no package dependencies.
#
#  Praat writes two on-disk formats, and the naive approach is to write a
#  separate parser for each. Instead both are reduced to the SAME ordered
#  stream of values, so there is only one piece of structural logic to get
#  right:
#
#    long format   xmin = 0            ->  take whatever follows the FIRST "="
#    short format  0                   ->  take the line as-is
#
#  After that reduction the streams are byte-for-byte equivalent:
#
#    xmin, xmax, size, then per tier: class, name, xmin, xmax, n,
#    then per interval: xmin, xmax, text      (or per point: number, mark)
#
#  Taking the FIRST "=" rather than the last matters: a label may itself
#  contain "=", and `text = "a = b"` must yield `"a = b"`, not `b"`.
# =============================================================================

# a Praat quoted string is "..." with any internal " doubled. It is complete
# once the quote characters balance AND it closes on a quote -- used to detect
# a label that Praat wrapped across several physical lines.
tg_quote_complete <- function(s) {
  nq <- nchar(s) - nchar(gsub('"', '', s, fixed = TRUE))
  nchar(s) >= 2 && endsWith(s, '"') && nq %% 2 == 0
}

tg_unquote <- function(s) {
  if (nchar(s) >= 2 && startsWith(s, '"') && endsWith(s, '"')) {
    s <- substr(s, 2, nchar(s) - 1)
  }
  gsub('""', '"', s, fixed = TRUE)
}

# ---- read the file as text, whatever Praat encoded it as -------------------
# Windows Praat commonly writes UTF-16LE with a BOM; some editors add a UTF-8
# BOM, which would otherwise stay glued to the first line and break the
# "File type" check. Falls back to latin1 for old files that are neither.
tg_read_lines <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size == 0) stop("TextGrid file is empty: ", basename(path))
  bytes <- readBin(path, what = "raw", n = size)

  b <- as.integer(head(bytes, 3))
  txt <- NULL
  if (length(b) >= 2 && b[1] == 0xff && b[2] == 0xfe) {
    txt <- iconv(list(bytes[-(1:2)]), from = "UTF-16LE", to = "UTF-8")
  } else if (length(b) >= 2 && b[1] == 0xfe && b[2] == 0xff) {
    txt <- iconv(list(bytes[-(1:2)]), from = "UTF-16BE", to = "UTF-8")
  } else {
    if (length(b) >= 3 && b[1] == 0xef && b[2] == 0xbb && b[3] == 0xbf) {
      bytes <- bytes[-(1:3)]
    }
    txt <- iconv(list(bytes), from = "UTF-8", to = "UTF-8")
    if (is.na(txt)) txt <- iconv(list(bytes), from = "latin1", to = "UTF-8")
  }
  if (is.na(txt)) stop("Could not decode TextGrid: ", basename(path))

  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  Encoding(lines) <- "UTF-8"
  lines
}

# ---- reduce either format to one ordered stream of values ------------------
tg_values <- function(lines, is_long) {
  n <- length(lines)
  out <- character(n)
  k <- 0L
  i <- 3L                                   # skip the two header lines
  while (i <= n) {
    ln <- lines[i]
    if (is_long) {
      eq <- regexpr("=", ln, fixed = TRUE)
      if (eq < 0L) { i <- i + 1L; next }     # `item [1]:`, `tiers? <exists>`
      v <- substring(ln, eq + 1L)
    } else {
      v <- ln
    }
    v <- trimws(v)
    if (!nzchar(v)) { i <- i + 1L; next }

    # a label Praat wrapped across lines: keep pulling until the quotes close
    if (startsWith(v, '"')) {
      while (!tg_quote_complete(v) && i < n) {
        i <- i + 1L
        v <- paste(v, lines[i], sep = "\n")
      }
    }
    k <- k + 1L
    out[k] <- v
    i <- i + 1L
  }
  out[seq_len(k)]
}

#' Read a Praat TextGrid.
#'
#' @return data.frame with one row per INTERVAL (point tiers are consumed so
#'   the stream stays aligned, but are not returned): file, tier, tier_index,
#'   index, label, xmin, xmax. `file` is the filename stem, which is what the
#'   audio matching keys off. Empty labels are kept -- the caller decides.
textgrid_read <- function(path) {
  lines <- tg_read_lines(path)
  if (length(lines) < 4) stop("TextGrid is too short to be valid: ", basename(path))

  body <- lines[-(1:2)]
  # long format always carries a top-level `xmin = `. A short-format label
  # containing "xmin =" would be quoted, so it cannot match at line start.
  is_long <- any(grepl('^\\s*xmin\\s*=', body))

  vals <- tg_values(lines, is_long)
  p <- 0L
  take <- function() {
    p <<- p + 1L
    if (p > length(vals))
      stop("TextGrid ended unexpectedly -- file may be truncated: ", basename(path))
    vals[p]
  }
  take_num <- function() suppressWarnings(as.numeric(tg_unquote(take())))

  take_num(); take_num()                     # file xmin, xmax
  v <- take()
  if (grepl("exists", v, fixed = TRUE)) v <- take()   # short format's <exists>
  n_tiers <- suppressWarnings(as.integer(tg_unquote(v)))
  if (is.na(n_tiers) || n_tiers < 0)
    stop("Could not read the tier count from: ", basename(path))

  stem <- tools::file_path_sans_ext(basename(path))

  # preallocate generously, then trim -- growing vectors per interval makes
  # a grid with thousands of intervals quadratic
  cap <- max(16L, length(vals) %/% 3L + 8L)
  f_tier <- character(cap); f_ti <- integer(cap); f_ix <- integer(cap)
  f_lab <- character(cap);  f_a  <- numeric(cap); f_b  <- numeric(cap)
  k <- 0L

  for (t in seq_len(n_tiers)) {
    cls  <- tg_unquote(take())
    name <- tg_unquote(take())
    take_num(); take_num()                   # tier xmin, xmax
    n_items <- suppressWarnings(as.integer(tg_unquote(take())))
    if (is.na(n_items) || n_items < 0)
      stop("Could not read the item count for tier '", name, "' in: ", basename(path))

    if (identical(cls, "IntervalTier")) {
      for (j in seq_len(n_items)) {
        a <- take_num(); b <- take_num(); lab <- tg_unquote(take())
        k <- k + 1L
        f_tier[k] <- name; f_ti[k] <- t; f_ix[k] <- j
        f_lab[k] <- lab;   f_a[k]  <- a; f_b[k]  <- b
      }
    } else {
      # point tier: consume number+mark so the stream stays aligned. A parser
      # that assumes every tier is an interval tier desynchronises here and
      # silently corrupts every tier that follows.
      for (j in seq_len(n_items)) { take(); take() }
    }
  }

  data.frame(file = rep(stem, k), tier = f_tier[seq_len(k)],
             tier_index = f_ti[seq_len(k)], index = f_ix[seq_len(k)],
             label = f_lab[seq_len(k)], xmin = f_a[seq_len(k)],
             xmax = f_b[seq_len(k)],
             stringsAsFactors = FALSE)
}

#' Read every TextGrid in a folder, tolerating individual bad files.
#' Returns the intervals plus an `errors` attribute naming files that failed,
#' so the app can report them instead of dying on one corrupt grid.
textgrid_read_dir <- function(paths) {
  outs <- list(); errs <- character(0)
  for (p in paths) {
    r <- tryCatch(textgrid_read(p), error = function(e) conditionMessage(e))
    if (is.character(r)) errs[basename(p)] <- r else outs[[length(outs) + 1L]] <- r
  }
  res <- if (length(outs)) do.call(rbind, outs) else
    data.frame(file = character(0), tier = character(0), tier_index = integer(0),
               index = integer(0), label = character(0), xmin = numeric(0),
               xmax = numeric(0), stringsAsFactors = FALSE)
  attr(res, "errors") <- errs
  res
}
