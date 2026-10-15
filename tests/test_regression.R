# Tests for backend/regression_analysis.R.

test_that("custom regression matches lm() and cor.test()", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(2.1, 3.9, 6.2, 8.1, 9.8, 12.2, 13.7, 16.4)
  r <- run_regression(list(model = "custom", x = x, y = y, x_label = "hours", y_label = "score"))
  fit <- summary(lm(y ~ x))
  expect_equal(r$coefficients$estimate, unname(coef(fit)[, 1]))
  expect_equal(r$coefficients$std_error, unname(coef(fit)[, 2]))
  expect_equal(r$coefficients$p_value, unname(coef(fit)[, 4]))
  expect_equal(r$fit$r_squared, fit$r.squared)
  expect_equal(r$correlation$pearson_r, cor(x, y))
  expect_equal(r$correlation$spearman_rho, cor(x, y, method = "spearman"))
  ci <- confint(lm(y ~ x))
  expect_equal(r$coefficients$ci_lower, unname(ci[, 1]))
  expect_equal(r$fit$r_squared, r$correlation$pearson_r^2)    # simple regression identity
  expect_equal(r$x_label, "hours")
  expect_true(any(grepl("Small sample", r$warnings)))
})

test_that("USD/INR vs dollar-index regression reproduces the original project", {
  r <- run_regression(list(model = "returns_vs_dollar_index", start_date = "2016-01-01", end_date = "2025-12-31"))
  expect_equal(r$n, 2490)
  expect_equal(r$coefficients$estimate[2], 0.438193187695652, tolerance = 1e-9)
  expect_equal(r$coefficients$estimate[1], 0.011204528312963, tolerance = 1e-8)
  expect_equal(r$fit$r_squared, 0.207148233813627, tolerance = 1e-9)
  expect_equal(r$correlation$pearson_r, 0.455135401626403, tolerance = 1e-9)
  expect_equal(r$correlation$spearman_rho, 0.428709500699321, tolerance = 1e-9)
  expect_equal(r$diagnostics$value[1], 2.04396024867615, tolerance = 1e-9)    # Durbin-Watson
  expect_equal(r$diagnostics$value[3], 0.825702913635223, tolerance = 1e-8)   # Breusch-Pagan
  expect_lte(length(r$chart$x), FXLAB_CONFIG$limits$chart_max_points)
})

test_that("time-trend regression reproduces the slope and warns about non-stationarity", {
  r <- run_regression(list(model = "time_trend", start_date = "2016-01-01", end_date = "2025-12-31"))
  expect_equal(r$coefficients$estimate[2], 2.46005875962448, tolerance = 1e-9)
  expect_equal(r$fit$r_squared, 0.924061329893519, tolerance = 1e-9)
  expect_true(any(grepl("NOT reliable", r$warnings)))
})

test_that("degenerate or malformed regression input is rejected", {
  expect_fx_error(run_regression(list(model = "custom", x = c(1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5))),
                  "DEGENERATE_DATA", "x variable is constant")
  expect_fx_error(run_regression(list(model = "custom", x = c(1, 2, 3, 4, 5), y = c(3, 3, 3, 3, 3))),
                  "DEGENERATE_DATA", "y variable is constant")
  expect_fx_error(run_regression(list(model = "custom", x = c(1, 2, 3, 4, 5), y = c(1, 2, 3))),
                  "VALIDATION_ERROR", "same number")
  expect_fx_error(run_regression(list(model = "custom", x = c(1, 2, 3), y = c(1, 2, 4))), "INSUFFICIENT_DATA")
  expect_fx_error(run_regression(list(model = "custom", x = "1, 2, NA, 4, 5", y = "1, 2, 3, 4, 5")),
                  "VALIDATION_ERROR", "missing")
  expect_fx_error(run_regression(list(model = "logistic")), "VALIDATION_ERROR", "model")
})
