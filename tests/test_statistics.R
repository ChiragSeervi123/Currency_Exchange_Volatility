# Tests for backend/statistics.R: returns, descriptive statistics, volatility and chart data.

test_that("simple, log and cumulative returns match their definitions", {
  r <- compute_returns(c(100, 110, 99))
  expect_equal(r$simple_pct, c(10, -10))
  expect_equal(r$log_pct, 100 * c(log(1.1), log(0.9)))
  expect_equal(cumulative_return_pct(c(100, 110, 99)), c(0, 10, -1))
  # Log returns add up to the log change over the whole period.
  expect_equal(sum(r$log_pct), 100 * log(99 / 100))
})

test_that("returns reject non-positive, non-finite or too few prices", {
  expect_fx_error(compute_returns(c(100, 0, 99)), "INVALID_DATA", "positive")
  expect_fx_error(compute_returns(c(100, -5)), "INVALID_DATA")
  expect_fx_error(compute_returns(c(100, NA, 99)), "INVALID_DATA", "finite")
  expect_fx_error(compute_returns(100), "INSUFFICIENT_DATA")
})

test_that("mean, median, variance, SD and quantiles match known values", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  s <- describe_sample(x)
  expect_equal(s$n, 8)
  expect_equal(s$mean, 5)
  expect_equal(s$median, 4.5)
  expect_equal(s$min, 2); expect_equal(s$max, 9); expect_equal(s$range, 7)
  expect_equal(s$variance, 32 / 7)            # sample variance, divisor n - 1
  expect_equal(s$variance_population, 4)      # population variance, divisor n
  expect_equal(s$sd, sqrt(32 / 7))
  expect_equal(s$standard_error, sqrt(32 / 7) / sqrt(8))
  expect_equal(s$quantiles$value, unname(stats::quantile(x, s$quantiles$probability, type = 7)))
  expect_equal(s$q1, 4); expect_equal(s$q3, 5.5); expect_equal(s$iqr, 1.5)
})

test_that("skewness and kurtosis follow the documented moment formulas", {
  set.seed(1)
  x <- rexp(500)
  m <- function(k) mean((x - mean(x))^k)
  s <- describe_sample(x)
  expect_equal(s$skewness, m(3) / m(2)^1.5)
  expect_equal(s$kurtosis, m(4) / m(2)^2)
  expect_equal(s$excess_kurtosis, s$kurtosis - 3)
  skip_if_not_installed("moments")
  expect_equal(s$skewness, moments::skewness(x))
  expect_equal(s$kurtosis, moments::kurtosis(x))
})

test_that("missing and non-finite values are excluded and reported, not converted", {
  s <- describe_sample(c(1, NA, 3, Inf, 5))
  expect_equal(s$n, 3)
  expect_equal(s$n_excluded, 2)
  expect_equal(s$mean, 3)
  expect_true(any(grepl("missing", s$warnings)))
  expect_true(any(grepl("non-finite", s$warnings)))
})

test_that("zero variance and tiny samples are handled safely", {
  s <- describe_sample(c(5, 5, 5, 5))
  expect_equal(s$sd, 0)
  expect_true(is.na(s$skewness))
  expect_true(is.na(s$kurtosis))
  expect_true(any(grepl("identical", s$warnings)))
  expect_fx_error(describe_sample(7), "INSUFFICIENT_DATA")
  expect_fx_error(describe_sample(c(NA, 1)), "INSUFFICIENT_DATA")
})

test_that("rolling SD and annualised volatility are computed correctly", {
  set.seed(2)
  x <- rnorm(60)
  roll <- rolling_sd(x, 20)
  expect_true(all(is.na(roll[1:19])))
  expect_equal(roll[20], sd(x[1:20]))
  expect_equal(roll[60], sd(x[41:60]))
  v <- volatility_measures(x)
  expect_equal(v$daily_sd_pct, sd(x))
  expect_equal(v$annualised_vol_pct, sd(x) * sqrt(252))
  expect_equal(v$mean_abs_return_pct, mean(abs(x)))
})

test_that("histogram bins cover every value and the density integrates to 1", {
  set.seed(3)
  x <- rnorm(1000)
  h <- histogram_bins(x)
  expect_equal(sum(h$count), 1000)
  expect_equal(sum(h$density * (h$upper - h$lower)), 1)
  expect_equal(min(h$lower), min(x)); expect_equal(max(h$upper), max(x))
  one <- histogram_bins(c(3, 3, 3))
  expect_equal(nrow(one), 1); expect_equal(one$count, 3)
})

test_that("tail comparison and Q-Q data are consistent with the sample", {
  set.seed(4)
  x <- rt(2000, df = 3)
  tc <- tail_comparison(x)
  z <- abs(x - mean(x)) / sd(x)
  expect_equal(tc$empirical_share[3], mean(z > 3))
  expect_equal(tc$normal_share, 2 * pnorm(-(1:4)))
  q <- qq_points(x, max_points = 300)
  expect_lte(length(q$sample), 300)
  expect_equal(q$n, 2000)
  expect_false(is.unsorted(q$sample))
})

test_that("custom statistics requests validate input and report removed entries", {
  out <- statistics_request(list(source = "custom", values = "1, 2, NA, 4, 10"))
  expect_equal(out$statistics$n, 4)
  expect_equal(out$n_missing_removed, 1)
  expect_fx_error(statistics_request(list(source = "custom", values = "1, 2, abc")), "VALIDATION_ERROR", "abc")
  expect_fx_error(statistics_request(list(source = "custom", values = list())), "VALIDATION_ERROR")
  prices <- statistics_request(list(source = "custom", data_type = "prices", values = c(80, 81, 79.5, 82)))
  expect_equal(prices$statistics$n, 3)
  expect_equal(prices$cumulative_return_pct, 100 * (82 / 80 - 1))
  expect_fx_error(statistics_request(list(source = "custom", data_type = "prices", values = c(80, -1, 82))),
                  "VALIDATION_ERROR", "positive")
})

# Cross-check against the original report (Pns Project, outputs/results/key_results.csv),
# which analysed the same FRED file for 2016-01-01 to 2025-12-31.
test_that("daily log-return statistics reproduce the original project's results", {
  h <- historical_variable("log_return", as.Date("2016-01-01"), as.Date("2025-12-31"))
  s <- describe_sample(h$values)
  expect_equal(s$n, 2496)
  expect_equal(s$mean, 0.0120162866152738, tolerance = 1e-10)
  expect_equal(s$sd, 0.309130362677209, tolerance = 1e-10)
  expect_equal(s$skewness, 0.348170465244479, tolerance = 1e-8)
  expect_equal(s$kurtosis, 7.18846250186363, tolerance = 1e-8)
  expect_equal(s$min, -1.94386285962711, tolerance = 1e-10)
  expect_equal(s$max, 2.21384472424067, tolerance = 1e-10)
})
