# Tests for backend/historical_analysis.R: CSV validation, dataset state and summaries.

csv <- function(...) paste(c(...), collapse = "\n")
dates <- format(seq(as.Date("2024-01-01"), by = "day", length.out = 15))
good_rows <- paste0(dates, ",", sprintf("%.2f", 83 + seq_along(dates) / 10))

test_that("a valid CSV is parsed and every quality count is reported", {
  rows <- good_rows
  rows[3] <- paste0(dates[3], ",")            # missing value (holiday)
  rows[4] <- paste0(dates[4], ",NA")          # missing value written as NA
  text <- csv("Date,USDINR", rev(rows), good_rows[10])   # reverse order + one exact duplicate
  p <- parse_rate_csv(text, "usdinr")
  expect_equal(p$report$rows_read, 16)
  expect_equal(p$report$missing_values, 2)
  expect_equal(p$report$duplicate_rows_removed, 1)
  expect_false(p$report$sorted_chronologically)
  expect_equal(nrow(p$data), 13)
  expect_false(is.unsorted(p$data$date))
  expect_true(any(grepl("not filled in", p$report$warnings)))
  expect_true(any(grepl("sorted by date", p$report$warnings)))
})

test_that("FRED-format headers and a byte-order mark are accepted", {
  p <- parse_rate_csv(paste0("﻿", csv("observation_date,DEXINUS", good_rows)), "usdinr")
  expect_equal(nrow(p$data), 15)
  expect_equal(p$report$columns_used, c("observation_date", "DEXINUS"))
})

test_that("structural problems are rejected with a clear message", {
  expect_fx_error(parse_rate_csv("", "usdinr"), "INVALID_DATA", "empty")
  expect_fx_error(parse_rate_csv("Date,USDINR", "usdinr"), "INVALID_DATA", "no data rows")
  expect_fx_error(parse_rate_csv(csv("Date,Price", good_rows), "usdinr"), "INVALID_DATA", "Missing required column")
  expect_fx_error(parse_rate_csv(csv("Day,USDINR", good_rows), "usdinr"), "INVALID_DATA", "Date")
  conflict <- csv("Date,USDINR", good_rows, paste0(dates[5], ",99.99"))
  expect_fx_error(parse_rate_csv(conflict, "usdinr"), "INVALID_DATA", "Conflicting values")
  expect_fx_error(parse_rate_csv(csv("Date,USDINR", good_rows[1:5]), "usdinr"), "INSUFFICIENT_DATA")
})

test_that("a few bad rows are excluded with warnings; many bad rows reject the file", {
  long_dates <- format(seq(as.Date("2023-01-01"), by = "day", length.out = 60))
  rows <- paste0(long_dates, ",", 80 + seq_along(long_dates) / 100)
  rows[10] <- "2023-02-30,81.00"              # impossible calendar date
  rows[20] <- paste0(long_dates[20], ",abc")   # non-numeric
  p <- parse_rate_csv(csv("Date,USDINR", rows), "usdinr")
  expect_equal(p$report$invalid_dates, 1)
  expect_equal(p$report$non_numeric_values, 1)
  expect_equal(nrow(p$data), 58)
  expect_true(any(grepl("line", p$report$warnings)))
  many <- rows
  many[1:10] <- paste0(long_dates[1:10], ",-5")
  expect_fx_error(parse_rate_csv(csv("Date,USDINR", many), "usdinr"), "INVALID_DATA", "Too many invalid rows")
  wrong_format <- paste0(format(as.Date(long_dates), "%d/%m/%Y"), ",80")
  expect_fx_error(parse_rate_csv(csv("Date,USDINR", wrong_format), "usdinr"), "INVALID_DATA", "YYYY-MM-DD")
})

test_that("the bundled FRED file loads with its documented quality figures", {
  ds <- get_dataset("usdinr")
  expect_equal(ds$id, "bundled")
  expect_equal(ds$report$rows_read, 14029)
  expect_equal(ds$report$missing_values, 551)
  expect_equal(ds$report$valid_observations, 13478)
  expect_equal(ds$report$duplicate_rows_removed, 0)
  expect_equal(nrow(get_dataset("dollar_index")$data), 5208)
})

test_that("a missing data file gives DATA_UNAVAILABLE, never invented data", {
  expect_fx_error(read_rate_file(file.path(tempdir(), "nope.csv"), "usdinr", "data/exchange_rates.csv"),
                  "DATA_UNAVAILABLE", "data/exchange_rates.csv")
})

test_that("date windows are validated", {
  ds <- get_dataset("usdinr")
  expect_fx_error(select_window(ds, as.Date("2020-01-01"), as.Date("2019-01-01")), "VALIDATION_ERROR", "start_date")
  expect_fx_error(select_window(ds, as.Date("1960-01-01"), as.Date("1960-12-31")), "INSUFFICIENT_DATA", "dataset covers")
  w <- select_window(ds)
  expect_equal(w$first_observation, "2016-01-04")
})

test_that("historical summary and chart data are consistent and bounded", {
  s <- historical_summary(as.Date("2024-01-01"), as.Date("2024-12-31"))
  expect_equal(s$window$returns, s$window$observations - 1)
  expect_equal(s$latest$rate, s$change$last_rate)
  expect_match(s$latest$note, "not a live quote")
  expect_equal(s$latest$change, s$latest$rate - s$latest$previous_rate)
  expect_equal(s$latest$change_pct, 100 * (s$latest$rate / s$latest$previous_rate - 1))
  expect_lte(length(s$recent$rate), 60)
  expect_equal(as.numeric(utils::tail(s$recent$rate, 1)), s$latest$rate)
  expect_equal(s$volatility$annualised_vol_pct, s$return_statistics$sd * sqrt(252))
  expect_equal(sum(s$annual$observations), s$window$observations)
  d <- historical_data(max_points = 200)
  expect_lte(d$n_returned, 200)
  expect_true(d$thinned)
  expect_equal(length(d$series$date), d$n_returned)
  expect_true(is.na(d$series$log_return_pct[1]))
})

test_that("uploading replaces the dataset until it is reset", {
  on.exit(reset_uploaded_dataset())
  info <- upload_usdinr_csv(csv("Date,USDINR", good_rows), "my<file>.csv")
  expect_equal(info$kind, "uploaded")
  expect_equal(info$file_name, "my_file_.csv")
  expect_equal(get_dataset("usdinr")$id, "uploaded")
  expect_equal(historical_summary()$window$observations, 15)
  expect_true(reset_uploaded_dataset())
  expect_equal(get_dataset("usdinr")$id, "bundled")
  expect_fx_error(upload_usdinr_csv(strrep("x", 6 * 1024^2)), "PAYLOAD_TOO_LARGE")
})

test_that("USD/INR and dollar-index returns are aligned on common dates (original project: 2,490 pairs)", {
  a <- aligned_returns(as.Date("2016-01-01"), as.Date("2025-12-31"))
  expect_length(a$usdinr_return_pct, 2490)
  expect_length(a$dollar_index_return_pct, 2490)
})
