# =============================================================================
#  Matching TextGrid intervals to tokens.
#
#  Shaped by a real corpus (S2_HT_citation: 565 intervals, 548s, mono- di- and
#  trisyllables in one recording), which made three things clear:
#
#    * A token is not an interval. A disyllable is segmented per syllable --
#      "47 1 HHRF MMHH M" and "47 2 HHRF MMHH M" -- and the playable span is
#      their union, INCLUDING the small gap between them (present in 51 of 53).
#    * Labels are structured, not atomic: "<citation> [position] [tones...]".
#      So a source has to be able to name a FIELD of the label.
#    * Identification is a JOIN on shared columns, not a rebuilt key string.
#      That way it works whether the dataset's token id is one column
#      (token_id = "S2_3_ct") or several pasted together, and never depends on
#      getting a separator or a column order to agree.
#
#  Sources available for a matching column:
#    "label"        the whole interval label
#    "label:N"      the Nth field of the label (split on label_sep)
#    "file:N"       the Nth piece of the filename stem (split on fname_sep)
#    "filestem"     the whole filename stem
#    "occurrence"   1-based index of this token's repetition within its file
#    "const:VALUE"  a fixed value
# =============================================================================

# Both sides must agree on encoding before match(). enc2utf8() alone is not
# enough: for a string marked "unknown" it converts FROM the locale's native
# encoding, so on a non-UTF-8 locale (many Windows R installs) valid UTF-8
# bytes get mangled. Every realistic source here emits UTF-8, so bytes that
# are valid UTF-8 are marked as such rather than reinterpreted.
am_norm <- function(x) {
  x <- as.character(x)
  if (!length(x)) return(x)
  unk <- !is.na(x) & Encoding(x) == "unknown" & validUTF8(x)
  if (any(unk)) { tmp <- x[unk]; Encoding(tmp) <- "UTF-8"; x[unk] <- tmp }
  trimws(enc2utf8(x))
}

# Loose form: drops leading zeros from numeric-looking values and case-folds,
# so a TextGrid's "03" meets a dataset's "3". Used to EXPLAIN misses by
# default, and to match only when the user opts in.
am_loose <- function(x) tolower(sub("^0+(?=[0-9])", "", am_norm(x), perl = TRUE))

am_field <- function(x, n, sep) {
  vapply(strsplit(am_norm(x), sep), function(p)
    if (length(p) >= n) p[n] else NA_character_, character(1))
}

# Guesses how a label's fields are separated, so nobody has to say. Looks at
# every run of non-alphanumeric characters between fields across a sample of
# real labels (e.g. the "_" in "01_1_HHHH", or repeated spaces in "01 1
# HHHH") and picks whichever run shows up most. A label with no such run, or
# only plain whitespace, falls back to splitting on any run of whitespace --
# the same default this always had.
am_guess_label_sep <- function(labels) {
  labels <- trimws(labels)
  labels <- labels[nzchar(labels)]
  if (!length(labels)) return("[[:space:]]+")
  sample <- utils::head(labels, 50)
  gaps <- unlist(regmatches(sample, gregexpr("[^[:alnum:]]+", sample)), use.names = FALSE)
  non_space <- gaps[nzchar(gaps) & !grepl("^[[:space:]]+$", gaps)]
  if (!length(non_space)) return("[[:space:]]+")
  best <- names(sort(table(non_space), decreasing = TRUE))[1]
  paste0("(?:", gsub("([\\^$.|?*+()\\[\\]{}])", "\\\\\\1", best, perl = TRUE), ")+")
}

# One place that knows what a source spec produces, so the matcher, the
# guesser and the on-screen preview can never disagree about it.
am_resolve_source <- function(tk, src, fname_sep = "_",
                              label_sep = "[[:space:]]+") {
  if (identical(src, "label"))      return(am_norm(tk$label))
  if (identical(src, "filestem"))   return(am_norm(tk$file))
  if (identical(src, "nparts"))     return(as.character(tk$n_parts))
  if (grepl("^label:[0-9]+$", src))
    return(am_field(tk$label, as.integer(sub("^label:", "", src)), label_sep))
  if (grepl("^file:[0-9]+$", src)) {
    n <- as.integer(sub("^file:", "", src))
    return(am_norm(vapply(strsplit(tk$file, fname_sep, fixed = TRUE), function(p)
      if (length(p) >= n) p[n] else NA_character_, character(1))))
  }
  if (grepl("^const:", src)) return(rep(am_norm(sub("^const:", "", src)), nrow(tk)))
  stop("Unknown audio source spec: ", src)
}

# How much of a column's real range of values a source can actually produce.
# 0 means this choice can never match anything, which is worth saying out loud
# rather than leaving the user to infer it from a zero match count.
am_source_coverage <- function(tk, src, want, fname_sep = "_",
                               label_sep = "[[:space:]]+") {
  want <- unique(am_loose(want)); want <- want[nzchar(want) & !is.na(want)]
  if (!length(want)) return(list(n = 0L, total = 0L, got = character(0)))
  got <- tryCatch(am_resolve_source(tk, src, fname_sep, label_sep),
                  error = function(e) character(0))
  got <- got[!is.na(got)]
  list(n = length(intersect(want, am_loose(got))), total = length(want),
       got = unique(got))
}

# Finds a field that numbers the pieces of a token (1, 2, 3 ...), if there is
# one, so that grouping by a shared key can still split a token said twice in
# a row. Looks only inside runs of intervals sharing a key: a counting field
# is all digits there and goes up by exactly one from piece to piece. Needs
# that to hold for at least 80% of steps -- one repeated production (1, 2, 1,
# 2) must not disqualify the very field that exposes it. NA when no field
# fits, in which case only the key decides.
am_detect_counter <- function(labels, key, file, label_sep, exclude = NA) {
  n <- length(labels)
  if (n < 2) return(NA_integer_)
  parts <- strsplit(am_norm(labels), label_sep)
  same <- c(FALSE, file[-1] == file[-n] & key[-1] == key[-n]) &
          !is.na(key) & c(FALSE, !is.na(key[-n]))
  if (!any(same)) return(NA_integer_)
  i <- which(same)                    # interval i continues the run of i - 1
  best <- NA_integer_; best_sc <- 0
  for (k in seq_len(max(lengths(parts)))) {
    if (!is.na(exclude) && k == exclude) next
    v <- vapply(parts, function(p) if (length(p) >= k) p[k] else NA_character_, "")
    a <- v[i - 1]; b <- v[i]
    if (!all(grepl("^[0-9]+$", c(a, b)))) next
    sc <- mean(as.integer(b) - as.integer(a) == 1)
    if (sc > best_sc) { best_sc <- sc; best <- k }
  }
  if (best_sc >= 0.8) best else NA_integer_
}

# ---- one row per TOKEN, not per interval -----------------------------------
#' Collapse an interval tier into tokens.
#'
#' @param position_field index of the label field holding syllable position,
#'   or NA when every interval is its own token. A new token begins wherever
#'   that field is absent or equals `first_position` -- which handles
#'   monosyllables (no position field at all), disyllables and trisyllables in
#'   one pass, and does not care that citation ids restart in each section.
#' @param key_field index of the label field that pieces of one token SHARE
#'   (e.g. 1, the item number in "47 1 HHRF" / "47 2 HHRF"). When given, it
#'   takes over from position_field: consecutive intervals with the same
#'   value there are one token, so nobody has to know which field numbers the
#'   pieces. See am_detect_counter() for how back-to-back repeats are kept
#'   apart.
#' @param chunk  with key_field: the number of intervals a token is known to
#'   span. A run of same-key intervals that is an exact multiple of it (and
#'   that neither a counter nor a pause has split) is cut into productions of
#'   that length -- the last resort for a repeat said without a pause.
#'
#' Repeats of one item said back to back share its key, so a run of
#' same-key intervals is split into productions by, in order:
#'   1. a counter field (1, 2, 1, 2 ...), when the labels have one;
#'   2. otherwise, a pause: a gap between two same-key intervals as long as
#'      the gaps this speaker leaves between DIFFERENT items (see
#'      am_pause_threshold) -- syllables of one word sit much closer;
#'   3. with `chunk`, a run of exactly k * chunk intervals.
#' Which of these was used is returned as attr(, "split_by").
audio_group_tokens <- function(intervals, position_field = NA,
                               label_sep = "[[:space:]]+", first_position = "1",
                               drop_empty = TRUE, key_field = NA, chunk = NA) {
  iv <- intervals
  if (drop_empty && nrow(iv)) iv <- iv[nzchar(am_norm(iv$label)), , drop = FALSE]
  if (!nrow(iv)) {
    iv$token_no <- integer(0); iv$n_parts <- integer(0)
    return(iv)
  }
  iv <- iv[order(iv$file, iv$xmin), , drop = FALSE]

  if (!is.na(key_field)) {
    n <- nrow(iv)
    key <- am_field(iv$label, key_field, label_sep)
    same_file <- c(FALSE, iv$file[-1] == iv$file[-n])
    same_key  <- c(FALSE, key[-1] == key[-n]) & !is.na(key) & c(FALSE, !is.na(key[-n]))
    starts <- !(same_file & same_key)
    # The same item said twice in a row ("47 1 .. 47 2 .. 47 1 .. 47 2")
    # shares its key across both productions. If some other field counts
    # the pieces, a count that fails to go up marks the second production.
    cf <- am_detect_counter(iv$label, key, iv$file, label_sep, exclude = key_field)
    split_by <- list(counter = cf, pause = NA_real_, chunk = NA_integer_)
    if (!is.na(cf)) {
      cnt <- suppressWarnings(as.integer(am_field(iv$label, cf, label_sep)))
      prev <- c(NA_integer_, cnt[-n])
      starts <- starts | (!is.na(cnt) & (is.na(prev) | cnt <= prev))
    } else if (n > 1) {
      # no counter: a clear pause inside a same-key run marks a new production
      thr <- am_pause_threshold(iv, key)
      gap <- c(NA_real_, iv$xmin[-1] - iv$xmax[-n])
      cut <- !starts & !is.na(gap) & gap >= thr
      if (any(cut)) { starts <- starts | cut; split_by$pause <- thr }
    }
    # a known token length: cut a run that is an exact multiple of it
    if (!is.na(chunk) && chunk >= 1) {
      run <- cumsum(starts | c(TRUE, iv$file[-1] != iv$file[-n]))
      len <- tabulate(run)[run]
      pos <- ave(seq_len(n), run, FUN = seq_along)
      cut <- len > chunk & len %% chunk == 0 & pos > 1 & (pos - 1) %% chunk == 0
      if (any(cut)) { starts <- starts | cut; split_by$chunk <- as.integer(chunk) }
    }
  } else if (is.na(position_field)) {
    starts <- rep(TRUE, nrow(iv))
  } else {
    pos <- am_field(iv$label, position_field, label_sep)
    # A label may simply have no position field -- a monosyllable is
    # "01 HH", whose second field is a tone transcription, not a position.
    # Anything non-numeric therefore means "this interval is a token on its
    # own"; testing only for NA would chain every monosyllable in the
    # recording into one token spanning minutes.
    has_pos <- !is.na(pos) & grepl("^[0-9]+$", pos)
    starts <- !has_pos | pos == first_position
  }
  starts[1] <- TRUE
  starts <- starts | c(TRUE, iv$file[-1] != iv$file[-nrow(iv)])  # never span files
  iv$token_no <- cumsum(starts)

  agg <- lapply(split(seq_len(nrow(iv)), iv$token_no), function(ix) {
    g <- iv[ix, , drop = FALSE]
    data.frame(file = g$file[1], tier = g$tier[1],
               label = g$label[1],            # first part carries the id fields
               all_labels = paste(g$label, collapse = " + "),
               xmin = min(g$xmin), xmax = max(g$xmax),
               n_parts = nrow(g), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, agg)
  out <- out[order(out$file, out$xmin), , drop = FALSE]
  if (!is.na(key_field)) attr(out, "split_by") <- split_by
  out
}

# How long a silence has to be to separate two productions of the same item.
# Read off the recording itself: the pauses a speaker leaves between
# DIFFERENT items (neighbouring intervals whose keys differ) say how they
# pace a list, and half of the shorter of those is still far longer than the
# gap between two syllables of one word. Never below 0.25 s (a stop closure
# left unlabelled can reach ~0.2 s); 0.5 s when there is nothing to go on.
am_pause_threshold <- function(iv, key) {
  n <- nrow(iv)
  if (n < 2) return(0.5)
  gap <- iv$xmin[-1] - iv$xmax[-n]
  diff_item <- iv$file[-1] == iv$file[-n] & !is.na(key[-1]) & !is.na(key[-n]) &
               key[-1] != key[-n] & gap > 0
  g <- gap[diff_item]
  if (length(g) < 3) return(0.5)
  max(0.25, stats::quantile(g, 0.1, names = FALSE) / 2)
}

#' Match grouped tokens to dataset rows.
#'
#' @param tokens     from audio_group_tokens()
#' @param token_tbl  one row per dataset token: tok_key plus the match columns
#' @param match_cols dataset columns that jointly identify a token
#' @param sources    named list: match column -> source spec
#' @param loose      TRUE to ignore zero padding and case when matching
audio_build_index <- function(tokens, token_tbl, match_cols, sources,
                              fname_sep = "_", label_sep = "[[:space:]]+",
                              loose = FALSE) {

  stopifnot(all(match_cols %in% names(token_tbl)), "tok_key" %in% names(token_tbl))
  miss <- setdiff(match_cols, names(sources))
  if (length(miss)) stop("No audio source chosen for: ", paste(miss, collapse = ", "))

  empty <- function(extra = NULL) {
    d <- data.frame(tok_key = character(0), file = character(0),
                    label = character(0), xmin = numeric(0), xmax = numeric(0),
                    occurrence = integer(0), n_parts = integer(0),
                    stringsAsFactors = FALSE)
    if (!is.null(extra)) d[[extra]] <- character(0)
    d
  }
  if (!nrow(tokens)) {
    return(list(index = empty(), other_material = empty(), incomplete = empty(),
                unmatched_tokens = data.frame(tok_key = token_tbl$tok_key,
                  reason = rep("no intervals available", nrow(token_tbl)),
                  stringsAsFactors = FALSE),
                near_misses = data.frame(), multi = character(0), loose = loose))
  }

  tk <- tokens

  vals <- lapply(match_cols, function(cl)
    am_resolve_source(tk, sources[[cl]], fname_sep, label_sep))
  names(vals) <- match_cols
  incomplete <- Reduce(`|`, lapply(vals, function(v) is.na(v) | !nzchar(v)))

  # ---- join on the match columns, via a signature per side ----------------
  shape <- if (loose) am_loose else am_norm
  sig <- function(cols) do.call(paste, c(lapply(cols, shape), list(sep = "\r")))
  audio_sig <- sig(vals)
  data_sig  <- sig(lapply(match_cols, function(cl) token_tbl[[cl]]))

  hit <- match(audio_sig, data_sig)
  ok  <- !is.na(hit) & !incomplete

  # several recordings of one token are KEPT, in time order, not discarded --
  # in fieldwork hearing both productions is usually the point
  occ <- integer(nrow(tk))
  if (any(ok)) occ[ok] <- stats::ave(seq_len(sum(ok)), hit[ok], FUN = seq_along)

  index <- data.frame(
    tok_key = token_tbl$tok_key[hit[ok]], file = tk$file[ok], label = tk$label[ok],
    xmin = tk$xmin[ok], xmax = tk$xmax[ok], occurrence = occ[ok],
    n_parts = tk$n_parts[ok], stringsAsFactors = FALSE)
  index <- index[order(index$tok_key, index$xmin), , drop = FALSE]
  multi <- unique(index$tok_key[duplicated(index$tok_key)])

  slim <- function(mask) data.frame(
    tok_key = audio_sig[mask], file = tk$file[mask], label = tk$label[mask],
    xmin = tk$xmin[mask], xmax = tk$xmax[mask],
    occurrence = rep(NA_integer_, sum(mask)),   # scalar NA breaks an empty mask
    n_parts = tk$n_parts[mask], stringsAsFactors = FALSE)

  # intervals that match nothing are NORMAL: a recording routinely holds
  # material outside the dataset. Only the ones that ALMOST match are worth
  # the user's attention, and they get their own report below.
  other <- slim(!ok & !incomplete)

  near <- data.frame()
  if (!loose && nrow(other)) {
    la <- sig_loose <- do.call(paste, c(lapply(vals, am_loose), list(sep = "\r")))[!ok & !incomplete]
    ld <- do.call(paste, c(lapply(match_cols, function(cl) am_loose(token_tbl[[cl]])), list(sep = "\r")))
    j <- match(la, ld)
    if (any(!is.na(j))) near <- data.frame(
      from_audio = gsub("\r", "_", other$tok_key[!is.na(j)]),
      looks_like = token_tbl$tok_key[j[!is.na(j)]],
      file = other$file[!is.na(j)], stringsAsFactors = FALSE)
  }

  # ---- tokens with no audio, and why --------------------------------------
  missing_tok <- setdiff(token_tbl$tok_key, index$tok_key)
  reason <- rep("no interval matched", length(missing_tok))
  file_cols <- match_cols[vapply(match_cols, function(cl)
    grepl("^file:[0-9]+$", sources[[cl]]) || identical(sources[[cl]], "filestem"),
    logical(1))]
  if (length(file_cols) && length(missing_tok)) {
    tsig <- sig(lapply(file_cols, function(cl) token_tbl[[cl]]))
    names(tsig) <- token_tbl$tok_key
    asig <- unique(sig(vals[file_cols]))
    reason[!(tsig[missing_tok] %in% asig)] <- "no audio file for this token"
  }
  if (!loose && nrow(near)) {
    reason[missing_tok %in% near$looks_like] <- "near-miss: padding or case differs"
  }

  list(index = index, other_material = other, incomplete = slim(incomplete),
       unmatched_tokens = data.frame(tok_key = missing_tok, reason = reason,
                                     stringsAsFactors = FALSE),
       near_misses = near, multi = multi, loose = loose)
}

# ---- pairing a TextGrid with its recording ---------------------------------
# Requiring the two stems to be identical is too strict for real folders.
# ProsodyPro, for one, renames the recording to <name>_original.WAV while
# every analysis file it writes keeps <name> -- so the grid and its own audio
# never match exactly. Tries, in order: identical stems; the audio stem
# extending the grid stem; the grid stem extending the audio stem; and
# finally, if the folder holds exactly one recording, that one.
#
# Returns an index into audio_stems for each grid stem, NA where unpaired.
audio_pair_files <- function(tg_stems, audio_stems) {
  t <- tolower(am_norm(tg_stems))
  a <- tolower(am_norm(audio_stems))
  out <- match(t, a)
  if (!length(a)) return(out)

  pick_one <- function(cand, by) {
    if (!length(cand)) return(NA_integer_)
    cand[by(nchar(a[cand]))]      # closest name, not just the first alphabetically
  }
  for (k in which(is.na(out))) {
    out[k] <- pick_one(which(startsWith(a, t[k])), which.min)
  }
  for (k in which(is.na(out))) {
    out[k] <- pick_one(which(startsWith(t[k], a)), which.max)
  }
  if (length(a) == 1L) out[is.na(out)] <- 1L
  out
}

audio_summarise <- function(res, n_tokens) {
  n <- length(unique(res$index$tok_key))
  bits <- sprintf("%d/%d tokens matched", n, n_tokens)
  if (length(res$multi))       bits <- c(bits, sprintf("%d with repeats", length(res$multi)))
  if (nrow(res$near_misses))   bits <- c(bits, sprintf("%d near-misses", nrow(res$near_misses)))
  if (nrow(res$incomplete))    bits <- c(bits, sprintf("%d incomplete", nrow(res$incomplete)))
  paste(bits, collapse = " · ")
}
