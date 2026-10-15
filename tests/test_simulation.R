# Tests for backend/simulation.R (GBM and Monte Carlo).

sim <- function(...) {
  args <- modifyList(list(start_rate = 85, mu_daily_pct = 0.01, sigma_daily_pct = 0.3, days = 60,
                          n_paths = 500, seed = 123, n_display_paths = 10), list(...))
  do.call(simulate_exchange_rate, args)
}

test_that("output dimensions match the request", {
  s <- sim(days = 60, n_paths = 500, n_display_paths = 10)
  expect_length(s$paths$values, 10)
  expect_true(all(vapply(s$paths$values, length, 1L) == 61))
  expect_length(s$paths$day, 61)
  for (b in c("p05", "p25", "p50", "p75", "p95", "mean")) expect_length(s$bands[[b]], 61)
  expect_equal(nrow(s$path_statistics), 10)
  expect_equal(s$terminal$n_paths, 500)
  # Fewer paths than requested display paths: all paths are shown.
  expect_length(sim(n_paths = 3, n_display_paths = 10)$paths$values, 3)
})

test_that("the initial exchange rate is preserved and every rate is positive", {
  s <- sim(start_rate = 83.25)
  expect_true(all(vapply(s$paths$values, function(p) p[1], 0) == 83.25))
  expect_equal(as.numeric(s$bands$p50[1]), 83.25)
  expect_true(s$all_positive)
  # Extreme but allowed inputs still give strictly positive rates.
  wild <- sim(mu_daily_pct = -5, sigma_daily_pct = 20, days = 500, n_paths = 200)
  expect_true(wild$all_positive)
  expect_true(all(unlist(wild$paths$values) > 0))
})

test_that("a fixed seed reproduces the simulation exactly; a generated seed is reported", {
  a <- sim(seed = 99); b <- sim(seed = 99); c <- sim(seed = 100)
  expect_identical(a$terminal, b$terminal)
  expect_identical(a$paths, b$paths)
  expect_false(identical(a$terminal$mean, c$terminal$mean))
  r <- sim(seed = NULL)
  expect_equal(r$seed_source, "generated")
  expect_true(is.numeric(r$seed))
  expect_identical(sim(seed = r$seed)$terminal, r$terminal)
})

test_that("summaries use ALL simulated paths, not only the displayed ones", {
  s <- sim(n_paths = 2000, n_display_paths = 5, days = 30, seed = 7)
  # Re-create every terminal value with the same random-number sequence.
  set.seed(7, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  mu <- 0.0001; sig <- 0.003
  log_s <- rep(log(85), 2000)
  for (t in 1:30) log_s <- log_s + (mu - sig^2 / 2) + sig * rnorm(2000)
  terminal <- exp(log_s)
  expect_equal(s$terminal$mean, mean(terminal), tolerance = 1e-12)
  expect_equal(s$terminal$sd, sd(terminal), tolerance = 1e-12)
  expect_equal(s$terminal$median, median(terminal), tolerance = 1e-12)
  expect_equal(s$terminal$percentiles$value, unname(quantile(terminal, s$terminal$percentiles$probability)),
               tolerance = 1e-12)
  expect_equal(s$terminal$prob_above_start, mean(terminal > 85))
})

test_that("pooled daily-return statistics use every simulated return (all paths x all days)", {
  s <- sim(n_paths = 300, days = 40, seed = 11, mu_daily_pct = 0.02, sigma_daily_pct = 0.5)
  # Re-create every simulated daily log return with the same random-number sequence.
  set.seed(11, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  z <- t(vapply(1:40, function(i) rnorm(300), numeric(300)))
  r <- as.vector(100 * ((0.0002 - 0.005^2 / 2) + 0.005 * z))
  dr <- s$daily_returns
  expect_equal(dr$n, 12000)
  expect_equal(dr$mean_pct, mean(r), tolerance = 1e-10)
  expect_equal(dr$sd_pct, sd(r), tolerance = 1e-10)
  expect_equal(dr$variance, var(r), tolerance = 1e-10)
  expect_equal(dr$skewness, skewness_g1(r), tolerance = 1e-8)
  expect_equal(dr$kurtosis, kurtosis_b2(r), tolerance = 1e-8)
  expect_equal(dr$min_pct, min(r)); expect_equal(dr$max_pct, max(r))
  expect_equal(sum(dr$histogram$count) + dr$n_outside_histogram, 12000)
  expect_equal(dr$within_2sd$share, mean(abs(z) < 2))
  expect_equal(dr$within_2sd$normal_share, pnorm(2) - pnorm(-2))
  expect_equal(dr$jarque_bera$statistic, jarque_bera_test(r)$statistic, tolerance = 1e-6)
  expect_equal(sum(dr$histogram$normal_expected_count), 12000 * (pnorm(6) - pnorm(-6)), tolerance = 1e-9)
})

test_that("day-by-day spread, total return and extremes are reported", {
  s <- sim(n_paths = 200, days = 20, seed = 5)
  expect_length(s$bands$cross_sectional_sd, 21)
  expect_equal(as.numeric(s$bands$cross_sectional_sd[1]), 0)
  expect_equal(as.numeric(s$bands$cross_sectional_sd[21]), s$terminal$sd, tolerance = 1e-7)
  expect_equal(s$terminal$mean_return_pct, 100 * (s$terminal$mean / 85 - 1))
  expect_equal(s$terminal$median_return_pct, 100 * (s$terminal$median / 85 - 1))
  expect_lte(s$terminal$lowest_point, s$terminal$min)
  expect_gte(s$terminal$highest_point, s$terminal$max)
})

test_that("displayed paths stay within the point budget", {
  big <- sim(days = 1000, n_paths = 500, n_display_paths = 500, seed = 1)
  expect_equal(big$paths$n_returned, floor(FXLAB_CONFIG$limits$sim_max_display_points / 1001))
  expect_true(big$paths$capped)
  expect_length(big$paths$values, big$paths$n_returned)
  all500 <- sim(days = 180, n_paths = 500, n_display_paths = 500, seed = 1)
  expect_equal(all500$paths$n_returned, 500)
  expect_false(all500$paths$capped)
  expect_fx_error(validate_simulation_input(list(start_rate = 85, mu_daily_pct = 0, sigma_daily_pct = 0.3,
                                                 days = 10, n_paths = 10, n_display_paths = 501)),
                  "VALIDATION_ERROR", "n_display_paths")
})

test_that("GBM Monte Carlo agrees with the exact log-normal theory", {
  s <- sim(n_paths = 10000, days = 252, seed = 2026, threshold = 90)
  se <- s$terminal$mean_standard_error
  expect_lt(abs(s$terminal$mean - s$theory$mean), 4 * se)
  expect_equal(s$theory$mean, 85 * exp(0.0001 * 252))
  expect_equal(s$theory$median, 85 * exp((0.0001 - 0.003^2 / 2) * 252))
  expect_equal(s$terminal$sd, s$theory$sd, tolerance = 0.05)
  p <- s$threshold$theory_prob_above
  expect_lt(abs(s$threshold$prob_above - p), 4 * sqrt(p * (1 - p) / 10000))
  expect_equal(s$threshold$prob_above + s$threshold$prob_below, 1)
  expect_gte(s$threshold$prob_touch, s$threshold$prob_above)
  expect_equal(s$terminal$log_return_sd_pct, s$theory$log_return_sd_pct, tolerance = 0.05)
})

test_that("zero volatility gives the deterministic growth path", {
  s <- sim(sigma_daily_pct = 0, mu_daily_pct = 0.05, days = 10, n_paths = 5)
  expect_equal(as.numeric(s$paths$values[[1]]), 85 * exp(0.0005 * (0:10)), tolerance = 1e-6)
  expect_equal(s$terminal$sd, 0)
})

test_that("Student-t and bootstrap shocks keep the requested daily volatility", {
  t_sim <- sim(model = "gbm_student_t", t_df = 4, n_paths = 5000, days = 100, seed = 3)
  b_sim <- sim(model = "gbm_bootstrap", n_paths = 5000, days = 100, seed = 3)
  expect_equal(t_sim$terminal$log_return_sd_pct, t_sim$theory$log_return_sd_pct, tolerance = 0.06)
  expect_equal(b_sim$terminal$log_return_sd_pct, b_sim$theory$log_return_sd_pct, tolerance = 0.06)
  expect_null(t_sim$theory$mean)          # no closed-form mean for non-normal shocks
  expect_gt(b_sim$bootstrap_pool$size, 1000)
  pool <- historical_shock_pool()$z
  expect_equal(mean(pool), 0, tolerance = 1e-12)
  expect_equal(mean(pool^2), 1, tolerance = 1e-12)
})

test_that("invalid or excessive simulation requests are rejected", {
  ok <- list(start_rate = 85, mu_daily_pct = 0.01, sigma_daily_pct = 0.3, days = 100, n_paths = 100)
  bad <- function(...) validate_simulation_input(modifyList(ok, list(...)))
  expect_type(validate_simulation_input(ok), "list")
  expect_fx_error(bad(start_rate = 0), "VALIDATION_ERROR", "start_rate")
  expect_fx_error(bad(start_rate = "85"), "VALIDATION_ERROR", "number")
  expect_fx_error(bad(sigma_daily_pct = -0.1), "VALIDATION_ERROR", "sigma_daily_pct")
  expect_fx_error(bad(sigma_daily_pct = 25), "VALIDATION_ERROR")
  expect_fx_error(bad(mu_daily_pct = 6), "VALIDATION_ERROR")
  expect_fx_error(bad(days = 0), "VALIDATION_ERROR", "days")
  expect_fx_error(bad(days = 1001), "VALIDATION_ERROR")
  expect_fx_error(bad(days = 10.5), "VALIDATION_ERROR", "whole")
  expect_fx_error(bad(n_paths = 10001), "VALIDATION_ERROR", "n_paths")
  expect_fx_error(bad(days = 1000, n_paths = 10000), "VALIDATION_ERROR", "exceeds the limit")
  expect_fx_error(bad(model = "jump_diffusion"), "VALIDATION_ERROR", "model")
  expect_fx_error(bad(model = "gbm_student_t", t_df = 2), "VALIDATION_ERROR", "t_df")
  expect_fx_error(bad(seed = -4), "VALIDATION_ERROR", "seed")
  expect_fx_error(bad(threshold = 0), "VALIDATION_ERROR", "threshold")
  expect_fx_error(validate_simulation_input(list()), "VALIDATION_ERROR", "required")
})

test_that("defaults are estimated from the historical data and labelled as such", {
  d <- simulation_defaults()
  expect_equal(d$source, "historical")
  r <- historical_variable("log_return")$values / 100
  expect_equal(d$sigma_daily_pct, 100 * sd(r))
  expect_equal(d$mu_daily_pct, 100 * (mean(r) + var(r) / 2))
  expect_equal(d$start_rate, tail(get_dataset("usdinr")$data$value, 1))
  expect_match(d$description, "not a live quote")
})
