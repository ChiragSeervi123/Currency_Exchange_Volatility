# =============================================================================
# prepare_data.R
# -----------------------------------------------------------------------------
# Purpose : Build the two CSV files FXLab reads from the untouched FRED
#           downloads in data/raw/ (copied from the original project, see
#           data/raw/download_log.csv for retrieval time and MD5 checksums).
#
#   data/raw/DEXINUS_FRED_raw.csv   -> data/exchange_rates.csv  (Date,USDINR)
#   data/raw/DTWEXBGS_FRED_raw.csv  -> data/dollar_index.csv    (Date,DollarIndex)
#
# Only the column names change. Every row is kept, including days on which
# FRED published no value (blank), so the app can report them as missing.
# Optional: set REFRESH_FROM_FRED <- TRUE to download fresh copies first
# (needs internet; the old raw files are replaced and the log is updated).
#
# Run from the project folder:  Rscript scripts/prepare_data.R
# =============================================================================

REFRESH_FROM_FRED <- FALSE

if (!file.exists("FXLab.Rproj")) stop("Run this script from the FXLab project folder.", call. = FALSE)

series <- list(
  list(id = "DEXINUS",  out = file.path("data", "exchange_rates.csv"), value_name = "USDINR"),
  list(id = "DTWEXBGS", out = file.path("data", "dollar_index.csv"),   value_name = "DollarIndex")
)
raw_path <- function(id) file.path("data", "raw", paste0(id, "_FRED_raw.csv"))

if (REFRESH_FROM_FRED) {
  for (s in series) {
    url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", s$id)
    tmp <- tempfile(fileext = ".csv")
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    d <- utils::read.csv(tmp, colClasses = "character")
    if (ncol(d) != 2 || nrow(d) < 100) stop("Unexpected file downloaded for ", s$id, call. = FALSE)
    file.copy(tmp, raw_path(s$id), overwrite = TRUE)
    log_path <- file.path("data", "raw", "download_log.csv")
    log <- utils::read.csv(log_path, colClasses = "character")
    entry <- log[log$series_id == s$id, ][1, ]
    entry$retrieved_at_utc <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    entry$rows <- nrow(d); entry$first_date <- d[[1]][1]; entry$last_date <- d[[1]][nrow(d)]
    entry$md5_checksum <- unname(tools::md5sum(raw_path(s$id)))
    entry$local_file <- raw_path(s$id)
    utils::write.csv(rbind(log[log$series_id != s$id, ], entry), log_path, row.names = FALSE)
    message("Downloaded ", s$id, " (", nrow(d), " rows)")
  }
}

for (s in series) {
  raw <- utils::read.csv(raw_path(s$id), colClasses = "character", check.names = FALSE,
                         na.strings = character(0))
  if (ncol(raw) != 2) stop(raw_path(s$id), " should have two columns.", call. = FALSE)
  out <- data.frame(Date = raw[[1]], Value = trimws(raw[[2]]), stringsAsFactors = FALSE)
  names(out)[2] <- s$value_name
  utils::write.csv(out, s$out, row.names = FALSE, quote = FALSE, na = "")
  message(sprintf("Wrote %s: %d rows (%d without a published value), %s to %s",
                  s$out, nrow(out), sum(out[[2]] %in% c("", ".")), out$Date[1], out$Date[nrow(out)]))
}
