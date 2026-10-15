# Tests for backend/distributions.R.

test_that("normal distribution probabilities, quantiles and moments", {
  d <- distribution_analysis("normal", list(mean = 0, sd = 1), query = list(type = "less_equal", a = 1.96),
                             quantile_p = 0.975)
  expect_equal(d$probability$probability, pnorm(1.96))
  expect_equal(d$quantile_query$value, qnorm(0.975))
  expect_equal(d$theoretical$mean, 0); expect_equal(d$theoretical$variance, 1)
  between <- distribution_analysis("normal", list(mean = 10, sd = 2), query = list(type = "between", a = 8, b = 12))
  expect_equal(between$probability$probability, pnorm(12, 10, 2) - pnorm(8, 10, 2))
  upper <- distribution_analysis("normal", list(mean = 10, sd = 2), query = list(type = "greater", a = 13))
  expect_equal(upper$probability$probability, pnorm(13, 10, 2, lower.tail = FALSE))
})

test_that("binomial probabilities use exact integer bounds", {
  d <- distribution_analysis("binomial", list(size = 20, prob = 0.3), query = list(type = "between", a = 4, b = 8))
  expect_equal(d$probability$probability, sum(dbinom(4:8, 20, 0.3)))
  g <- distribution_analysis("binomial", list(size = 20, prob = 0.3), query = list(type = "greater", a = 5))
  expect_equal(g$probability$probability, 1 - pbinom(5, 20, 0.3))
  expect_equal(g$probability$point_probability, dbinom(5, 20, 0.3))
  frac <- distribution_analysis("binomial", list(size = 20, prob = 0.3), query = list(type = "between", a = 3.5, b = 6.2))
  expect_equal(frac$probability$probability, sum(dbinom(4:6, 20, 0.3)))
  expect_equal(d$theoretical$mean, 6); expect_equal(d$theoretical$variance, 4.2)
  # The plotted support runs from the 0.01% to the 99.99% quantile, so the PMF sums to almost 1.
  expect_gt(sum(d$curve$density), 0.9998)
  expect_lte(sum(d$curve$density), 1 + 1e-12)
})

test_that("Poisson, uniform and exponential moments match their formulas", {
  p <- distribution_analysis("poisson", list(lambda = 4), query = list(type = "less_equal", a = 2.7))
  expect_equal(p$theoretical$mean, 4); expect_equal(p$theoretical$variance, 4)
  expect_equal(p$probability$probability, ppois(2, 4))
  u <- distribution_analysis("uniform", list(min = 2, max = 8))
  expect_equal(u$theoretical$mean, 5); expect_equal(u$theoretical$variance, 36 / 12)
  e <- distribution_analysis("exponential", list(rate = 0.5))
  expect_equal(e$theoretical$mean, 2); expect_equal(e$theoretical$variance, 4)
  expect_equal(e$theoretical$median, log(2) / 0.5)
})

test_that("random samples are reproducible and agree with theory", {
  a <- distribution_analysis("exponential", list(rate = 2), sample_size = 20000, seed = 11)
  b <- distribution_analysis("exponential", list(rate = 2), sample_size = 20000, seed = 11)
  expect_identical(a$sample$summary$mean, b$sample$summary$mean)
  expect_equal(a$sample$summary$seed, 11)
  se <- a$sample$summary$standard_error_of_mean
  expect_lt(abs(a$sample$summary$mean - 0.5), 5 * se)
  expect_equal(sum(a$sample$histogram$count), 20000)
  disc <- distribution_analysis("poisson", list(lambda = 3), sample_size = 5000, seed = 5)
  expect_equal(sum(disc$sample$discrete_frequencies$observed_count), 5000)
})

test_that("invalid distributions and parameters are rejected", {
  expect_fx_error(distribution_analysis("gamma", list()), "VALIDATION_ERROR", "distribution")
  expect_fx_error(distribution_analysis("normal", list(mean = 0, sd = 0)), "VALIDATION_ERROR", "params.sd")
  expect_fx_error(distribution_analysis("uniform", list(min = 3, max = 1)), "VALIDATION_ERROR", "less than")
  expect_fx_error(distribution_analysis("binomial", list(size = 10.5, prob = 0.2)), "VALIDATION_ERROR", "whole")
  expect_fx_error(distribution_analysis("binomial", list(size = 10, prob = 1.2)), "VALIDATION_ERROR")
  expect_fx_error(distribution_analysis("poisson", list(lambda = -1)), "VALIDATION_ERROR")
  expect_fx_error(distribution_analysis("normal", list(), query = list(type = "between", a = 2, b = 1)),
                  "VALIDATION_ERROR", "greater than or equal")
  expect_fx_error(distribution_analysis("normal", list(), quantile_p = 1), "VALIDATION_ERROR")
  expect_fx_error(distribution_analysis("normal", list(), sample_size = 1e6), "VALIDATION_ERROR")
})
