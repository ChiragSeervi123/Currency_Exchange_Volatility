# Tests for backend/hypothesis_testing.R.
# Reference values for 2016-2025 come from the original project's key_results.csv / tables.

hist_body <- function(...) modifyList(list(source = "historical", start_date = "2016-01-01", end_date = "2025-12-31"),
                                      list(...))

test_that("one-sample t-test matches R's t.test on entered data", {
  x <- c(0.12, -0.05, 0.31, 0.02, -0.11, 0.08, 0.15, -0.02, 0.04, 0.09)
  r <- run_hypothesis_test(list(test = "one_sample_t", source = "custom", values = x, mu0 = 0.1, alternative = "less"))
  tt <- t.test(x, mu = 0.1, alternative = "less")
  expect_equal(r$statistic, unname(tt$statistic))
  expect_equal(r$p_value, tt$p.value)
  expect_equal(r$df, unname(tt$parameter))
  expect_equal(r$hypotheses$alternative, "H1: mu < 0.1")
  expect_true(any(grepl("Small sample", r$warnings)))
})

test_that("one-sample t-test on USD/INR returns reproduces the original project", {
  r <- run_hypothesis_test(hist_body(test = "one_sample_t"))
  expect_equal(r$n, 2496)
  expect_equal(r$statistic, 1.94200751322314, tolerance = 1e-9)
  expect_equal(r$df, 2495)
  expect_equal(r$p_value, 0.0522486041214814, tolerance = 1e-8)
  expect_equal(r$decision, "Fail to reject H0")
  expect_false(r$reject_null)
  # A non-significant result must never be described as proof of H0.
  expect_match(r$interpretation, "does not prove")
  nw <- r$results[2, ]
  expect_match(nw$test, "Newey-West")
  expect_equal(nw$statistic, 2.0046904660256, tolerance = 1e-8)
  expect_equal(nw$p_value, 0.0449961483944688, tolerance = 1e-8)
})

test_that("confidence intervals match t.test and the chi-square formula", {
  r <- run_hypothesis_test(hist_body(test = "confidence_interval", alpha = 0.05))
  sd_row <- r$intervals[r$intervals$parameter == "Standard deviation", ]
  expect_equal(sd_row$lower, 0.300786905639648, tolerance = 1e-9)
  expect_equal(sd_row$upper, 0.317953344921182, tolerance = 1e-9)
  mean_row <- r$intervals[r$intervals$parameter == "Mean", ]
  expect_equal(mean_row$lower, -0.000116992731245582, tolerance = 1e-6)
  expect_equal(mean_row$upper, 0.0241495659617933, tolerance = 1e-9)
  expect_true(any(grepl("too narrow", r$warnings)))  # heavy tails warning
  r90 <- run_hypothesis_test(hist_body(test = "confidence_interval", alpha = 0.1))
  expect_lt(r90$confidence_interval$upper - r90$confidence_interval$lower,
            r$confidence_interval$upper - r$confidence_interval$lower)
})

test_that("normality tests reproduce Jarque-Bera and Shapiro-Wilk", {
  r <- run_hypothesis_test(hist_body(test = "normality"))
  expect_equal(r$results$statistic[1], 1874.92331738316, tolerance = 1e-8)
  expect_equal(r$results$statistic[2], 0.951214, tolerance = 1e-5)
  expect_true(r$reject_null)
  skip_if_not_installed("moments")
  h <- historical_variable("log_return", as.Date("2016-01-01"), as.Date("2025-12-31"))
  expect_equal(r$results$statistic[1], unname(moments::jarque.test(h$values)$statistic))
})

test_that("two-period tests reproduce the Welch, F and Brown-Forsythe results", {
  w <- run_hypothesis_test(hist_body(test = "two_sample_t", split_date = "2021-01-01"))
  expect_equal(w$statistic, 0.742431498167367, tolerance = 1e-9)
  expect_equal(w$df, 2273.99376995371, tolerance = 1e-9)
  expect_equal(w$p_value, 0.457902622003146, tolerance = 1e-8)
  expect_equal(w$groups$n, c(1247, 1249))
  v <- run_hypothesis_test(hist_body(test = "variance_equality", split_date = "2021-01-01"))
  expect_equal(v$statistic, 0.527279069043394, tolerance = 1e-9)
  expect_equal(v$results$statistic[2], 89.0152382732941, tolerance = 1e-8)
  expect_true(v$reject_null)
})

test_that("Ljung-Box tests reproduce the autocorrelation results", {
  r <- run_hypothesis_test(hist_body(test = "autocorrelation", lag = 10))
  expect_equal(r$results$statistic[1], 20.74547, tolerance = 1e-6)
  expect_equal(r$results$p_value[1], 0.02294006, tolerance = 1e-6)
  expect_equal(r$results$decision[2], "Reject H0")       # volatility clustering
  expect_length(r$chart$acf, 20)
})

test_that("two user-entered samples work and invalid requests are rejected", {
  r <- run_hypothesis_test(list(test = "two_sample_t", source = "custom", values = c(1, 2, 3, 4, 5), values2 = c(3, 4, 5, 6, 8)))
  expect_equal(r$statistic, unname(t.test(c(3, 4, 5, 6, 8), c(1, 2, 3, 4, 5))$statistic))
  expect_fx_error(run_hypothesis_test(list(test = "anova")), "VALIDATION_ERROR", "test")
  expect_fx_error(run_hypothesis_test(hist_body(alpha = 0.5)), "VALIDATION_ERROR", "alpha")
  expect_fx_error(run_hypothesis_test(list(test = "normality", source = "custom", values = c(1, 2, 3))),
                  "INSUFFICIENT_DATA")
  expect_fx_error(run_hypothesis_test(list(test = "one_sample_t", source = "custom", values = c(2, 2, 2))),
                  "DEGENERATE_DATA")
  expect_fx_error(run_hypothesis_test(hist_body(test = "two_sample_t", split_date = "2030-01-01")),
                  "INSUFFICIENT_DATA", "split date")
  expect_fx_error(run_hypothesis_test(hist_body(start_date = "2016-02-30")), "VALIDATION_ERROR", "calendar date")
})

test_that("tests on exchange-rate levels carry a warning", {
  r <- run_hypothesis_test(hist_body(test = "one_sample_t", variable = "level", mu0 = 70))
  expect_match(r$warnings[1], "LEVELS are not independent")
})
