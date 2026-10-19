# =============================================================================
# historical_analysis.R
# -----------------------------------------------------------------------------
# Purpose : Load, validate and summarise historical exchange-rate data.
#
# Expected CSV format (one row per date, header required):
#     Date,USDINR
#     2026-10-08,96.78
#     2026-10-09,96.73
#   - Date   : ISO format YYYY-MM-DD.
#   - USDINR : Indian rupees per 1 U.S. dollar, a positive number.
#   - A blank value, "NA" or "." marks a day with no published rate (e.g. a
#     holiday). Such rows are counted and excluded - never filled in.
#   - FRED downloads (columns observation_date,DEXINUS) are accepted as-is.
#
# Validation: missing columns, invalid dates, non-numeric and non-positive
# rates, duplicate dates, chronological order, too few observations and
# implausible one-day jumps. Problems are reported, never hidden.
# =============================================================================

MISSING_CODES <- c("", ".", "NA", "N/A", "#N/A", "NULL")

SERIES_SPECS <- list(
  usdinr = list(value_aliases = c("usdinr", "usd_inr", "dexinus", "rate", "exchangerate", "inrperusd"),
                expected = "Date,USDINR", label = "USD/INR rate"),
  dollar_index = list(value_aliases = c("dollarindex", "dtwexbgs", "usdindex", "broaddollarindex"),
                      expected = "Date,DollarIndex", label = "dollar index value")
)

normalise_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

row_examples <- function(rows, max_show = 5) {
  paste0(paste(utils::head(rows, max_show), collapse = ", "), if (length(rows) > max_show) ", ..." else "")
}

# ---- Parse and validate a CSV ---------------------------------------------------------------
# 'text' is the full CSV content. Returns list(data = data.frame(date, value), report = list(...)).
parse_rate_csv <- function(text, series = c("usdinr", "dollar_index"),
                           max_rows = FXLAB_CONFIG$limits$upload_max_rows) {
  series <- match.arg(series)
  spec <- SERIES_SPECS[[series]]
  invalid <- function(msg) fx_abort(msg, "INVALID_DATA", 422L)

  text <- sub("^﻿", "", text)                     # drop a UTF-8 byte-order mark
  if (!nzchar(trimws(text))) invalid("The CSV file is empty.")

  raw <- tryCatch(
    utils::read.csv(text = text, colClasses = "character", check.names = FALSE,
                    na.strings = character(0), strip.white = TRUE, blank.lines.skip = TRUE),
    error = function(e) invalid(paste0("The file could not be read as CSV. Check that it is a plain ",
                                       "comma-separated file with the header ", spec$expected, "."))
  )
  if (nrow(raw) == 0) invalid("The CSV file has a header but no data rows.")
  if (nrow(raw) > max_rows) invalid(sprintf("The CSV file has %d rows; the maximum is %d.", nrow(raw), max_rows))

  keys     <- normalise_name(names(raw))
  date_col <- which(keys %in% c("date", "observationdate"))[1]
  val_col  <- which(keys %in% spec$value_aliases)[1]
  if (is.na(date_col) || is.na(val_col)) {
    invalid(sprintf("Missing required column(s): %s. Expected header: %s. Columns found: %s.",
                    paste(c(if (is.na(date_col)) "Date", if (is.na(val_col)) sub("Date,", "", spec$expected)),
                          collapse = " and "),
                    spec$expected, paste(names(raw), collapse = ", ")))
  }
  extra_cols <- setdiff(names(raw), names(raw)[c(date_col, val_col)])

  date_txt  <- raw[[date_col]]
  value_txt <- raw[[val_col]]
  line_no   <- seq_len(nrow(raw)) + 1                  # CSV line number (line 1 = header)
  warnings  <- character(0)

  # Dates: strict ISO format, real calendar dates only.
  dates <- as.Date(date_txt, format = "%Y-%m-%d")
  bad_date <- is.na(dates) | !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_txt) |
    (!is.na(dates) & format(dates) != date_txt)
  dates[bad_date] <- NA

  # Values: missing codes are "no rate published"; anything else must be a positive number.
  is_missing <- toupper(value_txt) %in% MISSING_CODES
  values     <- suppressWarnings(as.numeric(value_txt))
  non_numeric <- !is_missing & (is.na(values) | !is.finite(values))
  non_positive <- !is_missing & !non_numeric & values <= 0

  n_rows <- nrow(raw)
  n_problem <- sum(bad_date | non_numeric | non_positive)
  if (n_problem > 0 && n_problem > 0.05 * sum(!is_missing)) {
    invalid(sprintf(paste0("Too many invalid rows (%d of %d): %d invalid date(s) [lines %s], ",
                           "%d non-numeric value(s) [lines %s], %d zero/negative value(s) [lines %s]. ",
                           "Expected format: %s with dates as YYYY-MM-DD."),
                    n_problem, n_rows, sum(bad_date), row_examples(line_no[bad_date]),
                    sum(non_numeric), row_examples(line_no[non_numeric]),
                    sum(non_positive), row_examples(line_no[non_positive]), spec$expected))
  }
  if (any(bad_date)) warnings <- c(warnings, sprintf(
    "%d row(s) with an invalid date were excluded (CSV lines %s).", sum(bad_date), row_examples(line_no[bad_date])))
  if (any(non_numeric)) warnings <- c(warnings, sprintf(
    "%d row(s) with a non-numeric %s were excluded (CSV lines %s).", sum(non_numeric), spec$label,
    row_examples(line_no[non_numeric])))
  if (any(non_positive)) warnings <- c(warnings, sprintf(
    "%d row(s) with a zero or negative %s were excluded (CSV lines %s).", sum(non_positive), spec$label,
    row_examples(line_no[non_positive])))

  keep <- !bad_date & !is_missing & !non_numeric & !non_positive
  d <- data.frame(date = dates[keep], value = values[keep], line = line_no[keep])
  n_missing <- sum(is_missing & !bad_date)
  if (n_missing > 0) warnings <- c(warnings, sprintf(
    "%d date(s) have no published value (blank/NA, typically holidays); they were excluded, not filled in.",
    n_missing))

  # Chronological order.
  was_sorted <- !is.unsorted(d$date)
  if (!was_sorted) {
    d <- d[order(d$date, d$line), ]
    warnings <- c(warnings, "Rows were not in chronological order; they were sorted by date.")
  }

  # Duplicate dates: identical copies are dropped; conflicting values are rejected.
  dup <- duplicated(d$date)
  n_dup <- sum(dup)
  if (n_dup > 0) {
    dup_dates <- unique(d$date[dup])
    conflict <- vapply(dup_dates, function(dt) length(unique(d$value[d$date == dt])) > 1, logical(1))
    if (any(conflict)) {
      invalid(sprintf("Conflicting values for the same date: %s. Keep one value per date and upload again.",
                      row_examples(format(dup_dates[conflict]))))
    }
    d <- d[!dup, ]
    warnings <- c(warnings, sprintf("%d exact duplicate row(s) were removed.", n_dup))
  }

  if (nrow(d) < FXLAB_CONFIG$limits$min_valid_observations) {
    fx_insufficient(sprintf("Only %d valid observation(s) remain after validation; at least %d are needed.",
                            nrow(d), FXLAB_CONFIG$limits$min_valid_observations))
  }

  # Plausibility checks (reported only; the data are not changed).
  gaps <- diff(as.numeric(d$date))
  log_change <- 100 * abs(diff(log(d$value)))
  big <- which(log_change > 10)
  if (length(big) > 0) warnings <- c(warnings, sprintf(paste0(
    "%d one-day change(s) larger than 10%% (%s). They were kept; confirm they are genuine events ",
    "(e.g. a devaluation) rather than data-entry errors."),
    length(big), row_examples(format(d$date[big + 1]), 3)))
  weekend <- sum(format(d$date, "%u") %in% c("6", "7"))
  if (weekend > 0) warnings <- c(warnings, sprintf("%d observation(s) fall on a weekend.", weekend))
  if (length(extra_cols) > 0) warnings <- c(warnings, sprintf(
    "Extra column(s) ignored: %s.", paste(extra_cols, collapse = ", ")))

  report <- list(
    rows_read              = n_rows,
    valid_observations     = nrow(d),
    missing_values         = n_missing,
    invalid_dates          = sum(bad_date),
    non_numeric_values     = sum(non_numeric),
    non_positive_values    = sum(non_positive),
    duplicate_rows_removed = n_dup,
    sorted_chronologically = was_sorted,
    first_date             = format(min(d$date)),
    last_date              = format(max(d$date)),
    weekend_observations   = weekend,
    largest_gap_days       = if (length(gaps)) max(gaps) else NA,
    largest_abs_daily_log_change_pct = if (length(log_change)) max(log_change) else NA,
    columns_used           = c(names(raw)[date_col], names(raw)[val_col]),
    warnings               = I(warnings)
  )
  rownames(d) <- NULL
  list(data = d[, c("date", "value")], report = report)
}

# 'display_path' is the project-relative path shown in messages (never the full disk path).
read_rate_file <- function(path, series, display_path = basename(path)) {
  if (!file.exists(path)) {
    fx_abort(sprintf(paste0("The %s data file is missing. Place a CSV file at '%s' in the project folder ",
                            "with the header %s%s."),
                     SERIES_SPECS[[series]]$label, display_path, SERIES_SPECS[[series]]$expected,
                     if (series == "usdinr") ", or upload one in the Historical Analysis section" else ""),
             "DATA_UNAVAILABLE", 404L)
  }
  text <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                   error = function(e) fx_abort("The data file exists but could not be read.",
                                                "INVALID_DATA", 422L))
  parse_rate_csv(text, series, max_rows = Inf)
}

# ---- Dataset state -------------------------------------------------------------------------------
# The bundled files are read once at start-up. An uploaded USD/INR file replaces
# the bundled one for all analyses until it is reset (kept in memory only).
.fx_state <- new.env(parent = emptyenv())

make_dataset <- function(parsed, id, series, file_name) {
  meta <- FXLAB_CONFIG$data_sources[[series]]
  list(id = id, series = series, name = meta$name,
       source = if (id == "bundled") meta$source else "Uploaded by the user (source not verified)",
       url = if (id == "bundled") meta$url else NULL, unit = meta$unit, file_name = file_name,
       loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       data = parsed$data, report = parsed$report)
}

load_datasets <- function(root = FXLAB_ROOT) {
  for (series in names(FXLAB_CONFIG$data_files)) {
    rel  <- FXLAB_CONFIG$data_files[[series]]
    path <- file.path(root, rel)
    .fx_state[[series]] <- NULL
    .fx_state[[paste0(series, "_error")]] <- NULL
    tryCatch(
      .fx_state[[series]] <- make_dataset(read_rate_file(path, series, gsub("\\\\", "/", rel)), "bundled",
                                          series, basename(path)),
      fxlab_error = function(e) {
        .fx_state[[paste0(series, "_error")]] <- e
        message("[Chart Reader] ", series, " data not loaded: ", conditionMessage(e))
      }
    )
  }
  .fx_state$uploaded <- NULL
  invisible(TRUE)
}

get_dataset <- function(series = c("usdinr", "dollar_index")) {
  series <- match.arg(series)
  if (series == "usdinr" && !is.null(.fx_state$uploaded)) return(.fx_state$uploaded)
  ds <- .fx_state[[series]]
  if (is.null(ds)) {
    err <- .fx_state[[paste0(series, "_error")]]
    if (!is.null(err)) stop(err)
    fx_abort(sprintf("No %s dataset is loaded.", SERIES_SPECS[[series]]$label), "DATA_UNAVAILABLE", 404L)
  }
  ds
}

dataset_status <- function(series) {
  ds <- tryCatch(get_dataset(series), fxlab_error = function(e) e)
  if (inherits(ds, "fxlab_error")) {
    return(list(available = FALSE, message = conditionMessage(ds)))
  }
  list(available = TRUE, kind = ds$id, name = ds$name, file_name = ds$file_name,
       observations = nrow(ds$data), first_date = format(min(ds$data$date)),
       last_date = format(max(ds$data$date)))
}

upload_usdinr_csv <- function(csv_text, file_name = "uploaded.csv") {
  if (!is.character(csv_text) || length(csv_text) != 1) {
    fx_abort("'csv_text' must contain the CSV file content as text.", field = "csv_text")
  }
  if (nchar(csv_text, type = "bytes") > FXLAB_CONFIG$limits$upload_max_bytes) {
    fx_abort(sprintf("The file is larger than %d MB.", FXLAB_CONFIG$limits$upload_max_bytes / 1024^2),
             "PAYLOAD_TOO_LARGE", 413L, field = "csv_text")
  }
  safe_name <- substr(gsub("[^A-Za-z0-9._ -]", "_", basename(as.character(file_name %||% "uploaded.csv"))), 1, 100)
  parsed <- parse_rate_csv(csv_text, "usdinr")
  .fx_state$uploaded <- make_dataset(parsed, "uploaded", "usdinr", safe_name)
  dataset_info(.fx_state$uploaded)
}

reset_uploaded_dataset <- function() {
  had <- !is.null(.fx_state$uploaded)
  .fx_state$uploaded <- NULL
  had
}

dataset_info <- function(ds) {
  list(kind = ds$id, name = ds$name, source = ds$source, url = ds$url, unit = ds$unit,
       file_name = ds$file_name, loaded_at = ds$loaded_at,
       observations = nrow(ds$data), first_date = format(min(ds$data$date)),
       last_date = format(max(ds$data$date)), quality = ds$report)
}

# ---- Analysis window -------------------------------------------------------------------------------
# Bundled data default to 2016-01-01 onwards; uploaded data default to the full file.
select_window <- function(ds, start_date = NULL, end_date = NULL, min_obs = 3) {
  d <- ds$data
  first <- min(d$date); last <- max(d$date)
  default_start <- if (ds$id == "bundled" && FXLAB_CONFIG$default_start_date > first &&
                       sum(d$date >= FXLAB_CONFIG$default_start_date) >= 30) FXLAB_CONFIG$default_start_date else first
  start <- start_date %||% default_start
  end   <- end_date %||% last
  if (start > end) fx_abort("'start_date' must be on or before 'end_date'.", field = "start_date")
  w <- d[d$date >= start & d$date <= end, ]
  if (nrow(w) < min_obs) {
    fx_insufficient(sprintf(paste0("Only %d observation(s) between %s and %s; at least %d are needed. ",
                                   "The dataset covers %s to %s."),
                            nrow(w), format(start), format(end), min_obs, format(first), format(last)))
  }
  rownames(w) <- NULL
  list(data = w, start_requested = format(start), end_requested = format(end),
       first_observation = format(min(w$date)), last_observation = format(max(w$date)), dataset = ds)
}

# Levels and returns in a window. Returns are between consecutive valid observations.
window_series <- function(win) {
  d <- win$data
  r <- compute_returns(d$value)
  list(dates = d$date, rate = d$value, return_dates = d$date[-1],
       log_return_pct = r$log_pct, simple_return_pct = r$simple_pct,
       gap_days = diff(as.numeric(d$date)))
}

# ---- Historical summary --------------------------------------------------------------------------
annual_summary <- function(dates, rate, trading_days = FXLAB_CONFIG$trading_days_per_year) {
  yr <- as.integer(format(dates, "%Y"))
  log_ret <- c(NA, 100 * diff(log(rate)))
  rows <- lapply(sort(unique(yr)), function(y) {
    i <- which(yr == y)
    prev_close <- if (min(i) > 1) rate[min(i) - 1] else NA_real_
    base <- if (is.na(prev_close)) rate[min(i)] else prev_close
    r <- log_ret[i]
    r <- r[is.finite(r)]
    data.frame(
      year = y, observations = length(i),
      first_date = format(dates[min(i)]), last_date = format(dates[max(i)]),
      open_rate = rate[min(i)], close_rate = rate[max(i)],
      change_pct = 100 * (rate[max(i)] / base - 1),
      change_basis = if (is.na(prev_close)) "first observation of the year" else "previous year's close",
      mean_rate = mean(rate[i]), min_rate = min(rate[i]), max_rate = max(rate[i]),
      annualised_vol_pct = if (length(r) >= 2) stats::sd(r) * sqrt(trading_days) else NA_real_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

historical_summary <- function(start_date = NULL, end_date = NULL) {
  ds  <- get_dataset("usdinr")
  win <- select_window(ds, start_date, end_date, min_obs = 3)
  s   <- window_series(win)
  n   <- length(s$rate)
  years <- as.numeric(max(s$dates) - min(s$dates)) / 365.25
  warnings <- character(0)
  if (length(s$log_return_pct) < 30) warnings <- c(warnings, sprintf(
    "Only %d returns in this window; statistics are imprecise.", length(s$log_return_pct)))
  if (any(s$gap_days > 5)) warnings <- c(warnings, sprintf(
    "%d return(s) span more than 5 calendar days (gaps in the data).", sum(s$gap_days > 5)))

  ord <- order(s$log_return_pct)
  moves <- function(idx) data.frame(date = format(s$return_dates[idx]), log_return_pct = s$log_return_pct[idx],
                                    rate = s$rate[idx + 1], previous_rate = s$rate[idx])
  list(
    dataset = dataset_info(ds),
    window = list(start_requested = win$start_requested, end_requested = win$end_requested,
                  first_observation = format(min(s$dates)), last_observation = format(max(s$dates)),
                  observations = n, returns = length(s$log_return_pct), years = years),
    latest = list(date = format(s$dates[n]), rate = s$rate[n],
                  previous_date = format(s$dates[n - 1]), previous_rate = s$rate[n - 1],
                  change = s$rate[n] - s$rate[n - 1],
                  change_pct = 100 * (s$rate[n] / s$rate[n - 1] - 1),
                  note = "Last observation in the selected window of the dataset. This is not a live quote."),
    # The most recent observations (at most 60), e.g. for a small trend line.
    recent = list(date = I(format(utils::tail(s$dates, 60))), rate = I(utils::tail(s$rate, 60))),
    change = list(first_date = format(s$dates[1]), first_rate = s$rate[1],
                  last_date = format(s$dates[n]), last_rate = s$rate[n],
                  absolute = s$rate[n] - s$rate[1],
                  percent = 100 * (s$rate[n] / s$rate[1] - 1),
                  log_percent = 100 * log(s$rate[n] / s$rate[1]),
                  compound_annual_pct = if (years > 0) 100 * ((s$rate[n] / s$rate[1])^(1 / years) - 1) else NA),
    level_statistics  = describe_sample(s$rate, include_cv = TRUE),
    return_statistics = describe_sample(s$log_return_pct),
    simple_return_mean_pct = mean(s$simple_return_pct),
    volatility = volatility_measures(s$log_return_pct, dates = s$return_dates),
    annual = annual_summary(s$dates, s$rate),
    largest_rises = moves(utils::head(rev(ord), 5)),
    largest_falls = moves(utils::head(ord, 5)),
    warnings = I(warnings)
  )
}

historical_data <- function(start_date = NULL, end_date = NULL,
                            max_points = FXLAB_CONFIG$limits$historical_max_points) {
  ds  <- get_dataset("usdinr")
  win <- select_window(ds, start_date, end_date, min_obs = 2)
  s   <- window_series(win)
  n   <- length(s$rate)
  roll <- c(NA, rolling_sd(s$log_return_pct, FXLAB_CONFIG$rolling_window) *
              sqrt(FXLAB_CONFIG$trading_days_per_year))
  log_ret <- c(NA, s$log_return_pct)
  idx <- thin_index(n, max_points)
  list(
    dataset = list(kind = ds$id, name = ds$name, source = ds$source, unit = ds$unit),
    window = list(first_observation = format(s$dates[1]), last_observation = format(s$dates[n])),
    n_total = n, n_returned = length(idx), thinned = length(idx) < n,
    note = if (length(idx) < n) sprintf(
      "%d of %d observations are returned for charting (evenly spaced). All statistics use every observation.",
      length(idx), n) else NULL,
    rolling_window = FXLAB_CONFIG$rolling_window,
    series = list(
      date = I(format(s$dates[idx])),
      rate = I(s$rate[idx]),
      log_return_pct = I(log_ret[idx]),
      rolling_ann_vol_pct = I(roll[idx]),
      cumulative_return_pct = I(cumulative_return_pct(s$rate)[idx])
    )
  )
}

# ---- Samples for the statistics / testing endpoints -------------------------------------------------
# variable: "log_return", "simple_return" or "level".
historical_variable <- function(variable = "log_return", start_date = NULL, end_date = NULL) {
  ds  <- get_dataset("usdinr")
  win <- select_window(ds, start_date, end_date, min_obs = 3)
  s   <- window_series(win)
  labels <- c(log_return = "Daily log return of USD/INR (%)",
              simple_return = "Daily simple return of USD/INR (%)",
              level = "USD/INR exchange rate (INR per USD)")
  values <- switch(variable, log_return = s$log_return_pct, simple_return = s$simple_return_pct, level = s$rate)
  dates  <- if (variable == "level") s$dates else s$return_dates
  list(values = values, dates = dates, label = labels[[variable]], variable = variable,
       description = sprintf("%s, %s to %s (%d values, %s data)", labels[[variable]],
                             format(min(dates)), format(max(dates)), length(values),
                             if (ds$id == "bundled") "bundled FRED" else "uploaded"),
       dataset_kind = ds$id)
}

# USD/INR and dollar-index log returns on dates where BOTH series have a value.
aligned_returns <- function(start_date = NULL, end_date = NULL) {
  fx  <- get_dataset("usdinr")
  usd <- get_dataset("dollar_index")
  win <- select_window(fx, start_date, end_date, min_obs = 3)
  m <- merge(win$data, usd$data, by = "date", suffixes = c("_fx", "_usd"))
  m <- m[order(m$date), ]
  if (nrow(m) < 4) {
    fx_insufficient(sprintf(paste0("Only %d date(s) have both a USD/INR rate and a dollar-index value in this ",
                                   "window. The dollar index covers %s to %s."),
                            nrow(m), format(min(usd$data$date)), format(max(usd$data$date))))
  }
  list(dates = m$date[-1],
       usdinr_return_pct = 100 * diff(log(m$value_fx)),
       dollar_index_return_pct = 100 * diff(log(m$value_usd)),
       n_fx_dates = nrow(win$data), n_common_dates = nrow(m),
       first_date = format(min(m$date)), last_date = format(max(m$date)))
}
