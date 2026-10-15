# =============================================================================
# regression_analysis.R
# -----------------------------------------------------------------------------
# Purpose : Correlation and simple linear regression  y = b0 + b1 x + e.
#
# Models:
#   returns_vs_dollar_index  y = daily log return of USD/INR (%),
#                            x = daily log return of the Nominal Broad U.S.
#                            Dollar Index (%), on dates where both series have
#                            a value. A genuine second variable from FRED.
#   time_trend               y = USD/INR level, x = years since the first
#                            observation. Levels trend and are autocorrelated,
#                            so OLS standard errors are NOT reliable here.
#   custom                   paired x and y values entered by the user.
#
# Output: Pearson r (with CI) and Spearman rho, OLS coefficients with t-tests
# and confidence intervals, heteroskedasticity-robust (HC1) standard errors,
# R-squared, F-test and residual diagnostics. Association is not causation.
# =============================================================================

# ---- Diagnostics ported from the original project (R/00_setup.R) ---------------------------
# White HC1 robust standard errors for a fitted lm().
hc1_coef_table <- function(model, conf_level = 0.95) {
  X <- stats::model.matrix(model); e <- stats::residuals(model)
  n <- nrow(X); k <- ncol(X)
  bread <- solve(crossprod(X))
  vc <- bread %*% crossprod(X * e) %*% bread * n / (n - k)
  b <- stats::coef(model); se <- sqrt(diag(vc)); t <- b / se
  crit <- stats::qt(1 - (1 - conf_level) / 2, df = n - k)
  data.frame(term = names(b), robust_se = se, robust_t = t, robust_p = 2 * stats::pt(-abs(t), df = n - k),
             robust_ci_lower = b - crit * se, robust_ci_upper = b + crit * se, row.names = NULL)
}

# Breusch-Pagan (Koenker studentised): n R^2 from regressing e^2 on x. H0: constant variance.
breusch_pagan_test <- function(model) {
  e2 <- stats::residuals(model)^2
  X  <- stats::model.matrix(model)[, -1, drop = FALSE]
  aux <- stats::lm(e2 ~ X)
  stat <- length(e2) * summary(aux)$r.squared
  list(statistic = stat, df = ncol(X), p_value = stats::pchisq(stat, ncol(X), lower.tail = FALSE))
}

durbin_watson <- function(e) sum(diff(e)^2) / sum(e^2)

correlation_strength <- function(r) {
  a <- abs(r)
  word <- if (a < 0.1) "negligible" else if (a < 0.3) "weak" else if (a < 0.5) "moderate" else if (a < 0.7) "strong" else "very strong"
  paste(word, if (r > 0) "positive" else if (r < 0) "negative" else "")
}

# ---- Core fit -------------------------------------------------------------------------------
fit_simple_regression <- function(x, y, alpha = 0.05, x_label = "x", y_label = "y") {
  ok <- is.finite(x) & is.finite(y)
  n_dropped <- sum(!ok)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 4) fx_insufficient(sprintf("Regression needs at least 4 complete (x, y) pairs; %d available.", n))
  if (stats::sd(x) <= 1e-12 * max(1, abs(mean(x)))) {
    fx_degenerate("The x variable is constant, so the regression slope and correlation are undefined.")
  }
  if (stats::sd(y) <= 1e-12 * max(1, abs(mean(y)))) {
    fx_degenerate("The y variable is constant, so the correlation is undefined (the fitted slope would be exactly 0).")
  }
  level <- 1 - alpha
  model <- stats::lm(y ~ x)
  sm <- summary(model)
  ci <- stats::confint(model, level = level)
  hc <- hc1_coef_table(model, level)
  co <- sm$coefficients
  coefficients <- data.frame(
    term = c("Intercept (b0)", sprintf("Slope (b1) on %s", x_label)),
    estimate = co[, 1], std_error = co[, 2], t_value = co[, 3], p_value = co[, 4],
    ci_lower = ci[, 1], ci_upper = ci[, 2],
    robust_se = hc$robust_se, robust_p = hc$robust_p,
    robust_ci_lower = hc$robust_ci_lower, robust_ci_upper = hc$robust_ci_upper, row.names = NULL)

  pear <- stats::cor.test(x, y, method = "pearson", conf.level = level)
  spear <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  f <- sm$fstatistic
  e <- stats::residuals(model); fitted <- stats::fitted(model)
  bp <- breusch_pagan_test(model)
  jb <- if (n >= 8) jarque_bera_test(e) else NULL
  dw <- durbin_watson(e)

  b1 <- unname(stats::coef(model)[2])
  sig <- co[2, 4] < alpha
  idx <- thin_index(n, FXLAB_CONFIG$limits$chart_max_points)
  xr <- range(x)

  list(
    n = n, n_dropped = n_dropped, x_label = x_label, y_label = y_label, alpha = alpha,
    equation = sprintf("%s = %.6g %s %.6g * %s", y_label, coef(model)[1], if (b1 < 0) "-" else "+", abs(b1), x_label),
    correlation = list(
      pearson_r = unname(pear$estimate), pearson_ci_lower = pear$conf.int[1], pearson_ci_upper = pear$conf.int[2],
      pearson_t = unname(pear$statistic), pearson_df = unname(pear$parameter), pearson_p = pear$p.value,
      spearman_rho = unname(spear$estimate), spearman_p = spear$p.value,
      strength = correlation_strength(unname(pear$estimate))),
    coefficients = coefficients,
    fit = list(r_squared = sm$r.squared, adj_r_squared = sm$adj.r.squared, residual_se = sm$sigma,
               f_statistic = unname(f["value"]), f_df1 = unname(f["numdf"]), f_df2 = unname(f["dendf"]),
               f_p_value = stats::pf(f["value"], f["numdf"], f["dendf"], lower.tail = FALSE)[[1]]),
    diagnostics = data.frame(
      diagnostic = c("Durbin-Watson statistic", "Lag-1 autocorrelation of residuals",
                     "Breusch-Pagan LM (H0: constant variance)", "Jarque-Bera on residuals (H0: normal residuals)"),
      value = c(dw, stats::acf(e, lag.max = 1, plot = FALSE)$acf[2], bp$statistic, if (is.null(jb)) NA else jb$statistic),
      p_value = c(NA, NA, bp$p_value, if (is.null(jb)) NA else jb$p_value),
      reading = c(if (dw < 1.5) "Below 1.5: positive autocorrelation in residuals" else if (dw > 2.5) "Above 2.5: negative autocorrelation" else "Close to 2: little first-order autocorrelation",
                  "",
                  if (bp$p_value < alpha) "Variance not constant: prefer the robust (HC1) standard errors" else "No evidence against constant variance",
                  if (is.null(jb)) "Too few residuals" else if (jb$p_value < alpha) "Residuals not normal (heavy tails are common in returns)" else "No evidence against normal residuals"),
      stringsAsFactors = FALSE),
    interpretation = paste(
      sprintf("Pearson r = %.3f indicates a %s linear relationship; R-squared = %.3f, so %.1f%% of the variation in %s is explained by %s.",
              pear$estimate, correlation_strength(unname(pear$estimate)), sm$r.squared, 100 * sm$r.squared, y_label, x_label),
      sprintf("Each 1-unit increase in %s is associated with an average change of %.4g in %s.", x_label, b1, y_label),
      if (sig) sprintf("The slope is statistically significant at the %g%% level (p = %.3g).", 100 * alpha, co[2, 4])
      else sprintf("The slope is not statistically significant at the %g%% level (p = %.3g); this does not prove there is no relationship.", 100 * alpha, co[2, 4]),
      "Correlation and regression describe association, not causation."),
    chart = list(
      x = I(x[idx]), y = I(y[idx]), fitted = I(fitted[idx]), residual = I(e[idx]),
      n_shown = length(idx), thinned = length(idx) < n,
      line = list(x = I(xr), y = I(unname(stats::coef(model)[1] + b1 * xr)))
    ),
    residuals = list(sd = stats::sd(e), min = min(e), max = max(e))
  )
}

# ---- Entry point used by POST /regression -------------------------------------------------------
run_regression <- function(body) {
  model <- v_choice(body$model, "model", c("returns_vs_dollar_index", "time_trend", "custom"),
                    default = "returns_vs_dollar_index")
  alpha <- v_alpha(body$alpha)
  start <- v_date(body$start_date, "start_date"); end <- v_date(body$end_date, "end_date")
  warnings <- character(0)

  if (model == "returns_vs_dollar_index") {
    a <- aligned_returns(start, end)
    out <- fit_simple_regression(a$dollar_index_return_pct, a$usdinr_return_pct, alpha,
                                 x_label = "Broad Dollar Index daily log return (%)",
                                 y_label = "USD/INR daily log return (%)")
    out$data_description <- sprintf(paste0("Daily log returns on %d dates where both USD/INR and the Broad ",
                                           "Dollar Index have a value (%s to %s). Source: FRED DEXINUS and DTWEXBGS."),
                                    a$n_common_dates, a$first_date, a$last_date)
    out$chart$dates <- I(format(a$dates[thin_index(length(a$dates), FXLAB_CONFIG$limits$chart_max_points)]))
    warnings <- c(warnings, "Returns (not levels) are used: regressing one trending level on another can give a high but meaningless ('spurious') R-squared.")
  } else if (model == "time_trend") {
    h <- historical_variable("level", start, end)
    t_years <- as.numeric(h$dates - min(h$dates)) / 365.25
    out <- fit_simple_regression(t_years, h$values, alpha, x_label = "Years since first observation",
                                 y_label = "USD/INR (INR per USD)")
    out$data_description <- h$description
    out$chart$dates <- I(format(h$dates[thin_index(length(h$dates), FXLAB_CONFIG$limits$chart_max_points)]))
    out$first_date <- format(min(h$dates))
    warnings <- c(warnings, sprintf(paste0(
      "Exchange-rate levels are non-stationary and their residuals are strongly autocorrelated (Durbin-Watson = %.3f). ",
      "The OLS standard errors, t-tests and p-values of this trend regression are therefore NOT reliable; ",
      "treat the slope as a descriptive average change per year."), out$diagnostics$value[1]))
  } else {
    px <- parse_numeric_input(body$x, "x"); py <- parse_numeric_input(body$y, "y")
    if (px$n_missing_removed + py$n_missing_removed > 0) {
      fx_abort("Paired data cannot contain missing entries: remove incomplete pairs so that x and y stay aligned.",
               field = if (px$n_missing_removed > 0) "x" else "y")
    }
    if (length(px$values) != length(py$values)) {
      fx_abort(sprintf("'x' and 'y' must have the same number of entries (x: %d, y: %d).",
                       length(px$values), length(py$values)), field = "y")
    }
    clean_label <- function(v, fallback) {
      if (is.character(v) && length(v) == 1 && nzchar(trimws(v))) substr(trimws(v), 1, 60) else fallback
    }
    out <- fit_simple_regression(px$values, py$values, alpha, x_label = clean_label(body$x_label, "x"),
                                 y_label = clean_label(body$y_label, "y"))
    out$data_description <- sprintf("User-entered paired data (%d pairs)", out$n)
    if (out$n < 30) warnings <- c(warnings, "Small sample: p-values and intervals assume normally distributed errors.")
  }
  out$warnings <- I(c(warnings, if (out$n_dropped > 0) sprintf("%d incomplete pair(s) excluded.", out$n_dropped)))
  c(list(model = model), out)
}
