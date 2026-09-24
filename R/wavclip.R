# =============================================================================
#  Extract one interval from a WAV file, without reading the whole file.
#
#  Needed because Shiny's addResourcePath serves static files with a plain 200
#  and no Accept-Ranges: a browser asked to play 0.6s at minute 8 of a 90MB
#  recording downloads all 90MB first. Cutting the clip server-side and serving
#  that instead sidesteps the problem entirely, and keeps every served file
#  small enough to be instant.
#
#  Base R only. Walks the RIFF chunk list rather than assuming the 44-byte
#  canonical header -- field recorders routinely insert LIST/fact/bext chunks,
#  and assuming a fixed offset would silently return the wrong audio.
# =============================================================================

wav_info <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))

  if (rawToChar(readBin(con, "raw", 4)) != "RIFF") stop("Not a RIFF file: ", basename(path))
  readBin(con, "integer", 1, size = 4, endian = "little")           # riff size
  if (rawToChar(readBin(con, "raw", 4)) != "WAVE") stop("Not a WAVE file: ", basename(path))

  fmt <- NULL; data_off <- NA_real_; data_len <- NA_real_
  pos <- 12
  repeat {
    id_raw <- readBin(con, "raw", 4)
    if (length(id_raw) < 4) break
    id  <- rawToChar(id_raw)
    sz  <- readBin(con, "integer", 1, size = 4, endian = "little")
    if (length(sz) == 0) break
    sz  <- as.numeric(sz)
    if (sz < 0) sz <- sz + 2^32                                      # >2GB chunk
    body <- pos + 8

    if (id == "fmt ") {
      fmt <- list(
        format      = readBin(con, "integer", 1, size = 2, signed = FALSE, endian = "little"),
        channels    = readBin(con, "integer", 1, size = 2, signed = FALSE, endian = "little"),
        sample_rate = readBin(con, "integer", 1, size = 4, endian = "little"))
      readBin(con, "integer", 1, size = 4, endian = "little")        # byte rate
      readBin(con, "integer", 1, size = 2, signed = FALSE, endian = "little")  # block align
      fmt$bits <- readBin(con, "integer", 1, size = 2, signed = FALSE, endian = "little")
      if (sz > 16) readBin(con, "raw", sz - 16)
    } else if (id == "data") {
      data_off <- body; data_len <- sz
      seek(con, body + sz + (sz %% 2), origin = "start")
    } else {
      seek(con, body + sz + (sz %% 2), origin = "start")
    }
    pos <- body + sz + (sz %% 2)
  }

  if (is.null(fmt)) stop("No fmt chunk in: ", basename(path))
  if (is.na(data_off)) stop("No data chunk in: ", basename(path))
  if (fmt$format != 1)
    stop("Only uncompressed PCM WAV can be cut directly (format ", fmt$format, ")")

  c(fmt, list(data_offset = data_off, data_length = data_len,
              frame_bytes = fmt$channels * fmt$bits / 8,
              duration = data_len / (fmt$sample_rate * fmt$channels * fmt$bits / 8)))
}

wav_clip <- function(path, out, start, end, pad = 0) {
  inf <- wav_info(path)
  start <- max(0, start - pad)
  end   <- min(inf$duration, end + pad)
  if (!(end > start)) stop("Empty interval requested")

  fb <- inf$frame_bytes
  # round, not floor: a boundary written as frame/rate can come back a
  # hair below that frame in binary floating point, and floor() would then
  # start the clip one sample early. Rounding picks the nearest sample,
  # which is what a boundary means anyway.
  off <- inf$data_offset + round(start * inf$sample_rate) * fb
  n   <- round((end - start) * inf$sample_rate) * fb
  n   <- min(n, inf$data_offset + inf$data_length - off)

  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  seek(con, off, origin = "start")            # seek, never read the whole file
  payload <- readBin(con, "raw", n)

  o <- file(out, "wb"); on.exit(close(o), add = TRUE)
  w32 <- function(v) writeBin(as.integer(v), o, size = 4, endian = "little")
  w16 <- function(v) writeBin(as.integer(v), o, size = 2, endian = "little")
  writeBin(charToRaw("RIFF"), o); w32(36 + length(payload))
  writeBin(charToRaw("WAVE"), o)
  writeBin(charToRaw("fmt "), o); w32(16); w16(1); w16(inf$channels)
  w32(inf$sample_rate)
  w32(inf$sample_rate * fb); w16(fb); w16(inf$bits)
  writeBin(charToRaw("data"), o); w32(length(payload))
  writeBin(payload, o)
  invisible(out)
}
