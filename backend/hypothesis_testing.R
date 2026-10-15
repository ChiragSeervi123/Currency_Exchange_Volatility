# =============================================================================
# hypothesis_testing.R
# -----------------------------------------------------------------------------
# Purpose : Hypothesis tests and confidence intervals for exchange-rate returns
#           (or any sample the user enters). R's own test functions are used
#           wherever they exist (t.test, var.test, shapiro.test, Box.test).
#
# Tests:
#   one_sample_t        H0: mu = mu0 (Student t). For time-ordered data a
#                       Newey-West (autocorrelation-robust) z-test is added.
#   confidence_interval t-interval for the mean; chi-square intervals for the
#                       variance and SD (exact only for normal data).
#   normality           H0: data are normal (Jarque-Bera; Shapiro-Wilk, n <= 5000).
#   two_sample_t        H0: mu2 = mu1 (Welch t-test, unequal variances).
#   variance_equality   H0: sigma2^2 = sigma1^2 (F-test; Brown-Forsythe as the
#                       robust alternative).
#   autocorrelation     H0: no autocorrelation up to lag h (Ljung-Box), for the
#                       returns and the squared returns (volatility clustering).
#
# Wording: "fail to reject H0" never means that H0 has been proven true.
# =============================================================================

TEST_TYPES <- c("one_sample_t", "confidence_interval", "normality", "two_sample_t",
                "variance_equality", "autocorrelation")

decision_text <- function(p, alpha) if (p < alpha) "Reject H0" else "Fail to reject H0"

result_row <- function(name, statistic_name, statistic, df = NA, p_value, alpha, note = NULL) {
  data.frame(test = name, statistic_name = statistic_name, statistic = unname(statistic),
             df = if (length(df) == 0) NA_character_ else as.character(df),
             p_value = unname(p_value), decision = decision_text(p_value, alpha),
             note = note %||% "", stringsAsFactors = FALSE)
}

alt_symbol <- function(alternative) c(two.sided = "!=", less = "<", greater = ">")[[alternative]]

# ---- Helpers ported from the original project (R/00_setup.R) ------------------------------
# Newey-West (HAC) test of H0: mean = mu0, Bartlett weights, lag = floor(4 (n/100)^(2/9)).
newey_west_mean_test <- function(x, mu0 = 0, alternative = "two.sided", conf_level = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  lag <- floor(4 * (n / 100)^(2 / 9))
  u <- x - mean(x)
  lrv <- sum(u^2) / n
  for (j in seq_len(lag)) lrv <- lrv + 2 * (1 - j / (lag + 1)) * sum(u[(j + 1):n] * u[1:(n - j)]) / n
  se <- sqrt(lrv / n)
  z  <- (mean(x) - mu0) / se
  p  <- switch(alternative, two.sided = 2 * stats::pnorm(-abs(z)), less = stats::pnorm(z),
               greater = stats::pnorm(z, lower.tail = FALSE))
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(estimate = mean(x), se = se, statistic = z, p_value = p, lag = lag,
       ci = c(mean(x) - crit * se, mean(x) + crit * se))
}

# Brown-Forsythe test: one-way ANOVA on absolute deviations from each group's median.
brown_forsythe_test <- function(x1, x2) {
  g <- factor(c(rep("1", length(x1)), rep("2", length(x2))))
  x <- c(x1, x2)
  med <- tapply(x, g, stats::median)
  dev <- abs(x - med[as.character(g)])
  tab <- stats::anova(stats::lm(dev ~ g))
  list(statistic = tab[["F value"]][1], df1 = tab[["Df"]][1], df2 = tab[["Df"]][2], p_value = tab[["Pr(>F)"]][1])
}

# Jarque-Bera: JB = n/6 * (S^2 + (K - 3)^2 / 4) ~ chi-square(2) under normality (large n).
jarque_bera_test <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  s <- skewness_g1(x); k <- kurtosis_b2(x)
  jb <- n / 6 * (s^2 + (k - 3)^2 / 4)
  list(statistic = jb, df = 2, p_value = stats::pchisq(jb, df = 2, lower.tail = FALSE), skewness = s, kurtosis = k)
}

# Density of the test statistic under H0, with critical values, for the chart.
null_distribution <- function(dist, observed, alpha, alternative = "two.sided", df1 = NULL, df2 = NULL) {
  if (dist == "t") {
    lim <- max(4.5, min(abs(observed) * 1.15, 40))
    x <- seq(-lim, lim, length.out = 301)
    dens <- stats::dt(x, df1)
    crit <- switch(alternative, two.sided = c(-1, 1) * stats::qt(1 - alpha / 2, df1),
                   less = stats::qt(alpha, df1), greater = stats::qt(1 - alpha, df1))
  } else if (dist == "F") {
    hi <- max(stats::qf(0.999, df1, df2), observed * 1.15)
    x <- seq(0, hi, length.out = 301)
    dens <- stats::df(x, df1, df2)
    crit <- c(stats::qf(alpha / 2, df1, df2), stats::qf(1 - alpha / 2, df1, df2))
  } else {                                   # chi-square, upper-tail test
    hi <- max(stats::qchisq(0.999, df1), min(observed * 1.15, stats::qchisq(0.999, df1) * 20))
    x <- seq(0, hi, length.out = 301)
    dens <- stats::dchisq(x, df1)
    crit <- stats::qchisq(1 - alpha, df1)
    alternative <- "greater"
  }
  dens[!is.finite(dens)] <- NA
  list(distribution = dist, df1 = df1, df2 = df2, x = I(x), density = I(dens),
       critical_values = I(crit), observed = observed, alternative = alternative,
       observed_off_scale = observed > max(x) || observed < min(x))
}

# ---- Individual tests ---------------------------------------------------------------------------
test_one_sample_t <- function(x, alpha, mu0, alternative, time_ordered, label) {
  require_n(x, 2, "The one-sample t-test")
  require_variation(x, "The t statistic")
  n <- length(x)
  tt <- stats::t.test(x, mu = mu0, alternative = alternative, conf.level = 1 - alpha)
  warnings <- character(0)
  rows <- list(result_row("One-sample t-test", "t", tt$statistic, round(tt$parameter, 2), tt$p.value, alpha))
  if (n < 30) {
    jb <- if (n >= 8) jarque_bera_test(x) else NULL
    warnings <- c(warnings, sprintf(paste0("Small sample (n = %d): the t-test assumes the data are roughly normal%s."),
                                    n, if (!is.null(jb) && jb$p_value < 0.05) "; the Jarque-Bera test suggests they are not" else ""))
  }
  if (time_ordered && n >= 30) {
    lb <- stats::Box.test(x, lag = min(10, floor(n / 5)), type = "Ljung-Box")
    nw <- newey_west_mean_test(x, mu0, alternative, 1 - alpha)
    rows[[2]] <- result_row(sprintf("Newey-West robust z-test (%d lags)", nw$lag), "z", nw$statistic, NA,
                            nw$p_value, alpha, "Robustness check: allows autocorrelation and changing variance")
    if (lb$p.value < alpha) warnings <- c(warnings, sprintf(paste0(
      "The data show autocorrelation (Ljung-Box p = %.3g), so the independence assumption of the t-test is ",
      "questionable; compare with the Newey-West result."), lb$p.value))
  }
  est <- unname(tt$estimate)
  sig <- tt$p.value < alpha
  direction <- c(two.sided = "differs from", less = "is less than", greater = "is greater than")[[alternative]]
  interp <- if (sig) {
    sprintf("At the %g%% significance level the sample provides statistically significant evidence that the mean %s %g (sample mean = %.5g, p = %.3g).",
            100 * alpha, direction, mu0, est, tt$p.value)
  } else {
    sprintf(paste0("At the %g%% significance level there is not enough evidence to conclude that the mean %s %g ",
                   "(sample mean = %.5g, p = %.3g). This does not prove that the mean equals %g; the effect may be too ",
                   "small to detect with this sample."), 100 * alpha, direction, mu0, est, tt$p.value, mu0)
  }
  list(
    test_name = "One-sample t-test for a population mean",
    hypotheses = list(null = sprintf("H0: mu = %g", mu0), alternative = sprintf("H1: mu %s %g", alt_symbol(alternative), mu0)),
    n = n, statistic_name = "t", statistic = unname(tt$statistic), df = unname(tt$parameter), p_value = tt$p.value,
    reject_null = sig, decision = decision_text(tt$p.value, alpha),
    estimates = list(mean = est, sd = stats::sd(x), standard_error = stats::sd(x) / sqrt(n), mu0 = mu0),
    confidence_interval = list(level = 1 - alpha, lower = tt$conf.int[1], upper = tt$conf.int[2],
                               parameter = "population mean",
                               note = if (alternative != "two.sided") "One-sided interval matching the one-sided alternative." else NULL),
    results = do.call(rbind, rows),
    interpretation = interp,
    assumptions = I(c("Observations are independent.",
                      "The sample mean is approximately normal: true for normal data, or for large n by the Central Limit Theorem.")),
    warnings = I(warnings),
    chart = null_distribution("t", unname(tt$statistic), alpha, alternative, df1 = unname(tt$parameter))
  )
}

test_confidence_interval <- function(x, alpha, label, is_return) {
  require_n(x, 2, "A confidence interval")
  require_variation(x, "The confidence interval for the variance")
  n <- length(x); s2 <- stats::var(x); m <- mean(x)
  level <- 1 - alpha
  tt <- stats::t.test(x, conf.level = level)
  chi_hi <- stats::qchisq(1 - alpha / 2, n - 1); chi_lo <- stats::qchisq(alpha / 2, n - 1)
  var_ci <- c((n - 1) * s2 / chi_hi, (n - 1) * s2 / chi_lo)
  rows <- data.frame(
    parameter = c("Mean", "Variance", "Standard deviation"),
    method = c("Student t interval: mean +/- t(1 - alpha/2, n - 1) * s / sqrt(n)",
               "Chi-square interval: (n - 1)s^2 / chi2 quantiles",
               "Square root of the variance interval"),
    estimate = c(m, s2, sqrt(s2)), lower = c(tt$conf.int[1], var_ci[1], sqrt(var_ci[1])),
    upper = c(tt$conf.int[2], var_ci[2], sqrt(var_ci[2])), stringsAsFactors = FALSE)
  if (is_return) {
    a <- sqrt(FXLAB_CONFIG$trading_days_per_year)
    rows <- rbind(rows, data.frame(parameter = "Annualised volatility (%)",
                                   method = "SD interval x sqrt(252)", estimate = sqrt(s2) * a,
                                   lower = sqrt(var_ci[1]) * a, upper = sqrt(var_ci[2]) * a))
  }
  warnings <- character(0)
  ek <- kurtosis_b2(x) - 3
  if (is.finite(ek) && ek > 1) warnings <- c(warnings, sprintf(paste0(
    "Excess kurtosis is %.2f (heavy tails). The chi-square intervals for the variance and SD assume normal data, ",
    "so they are likely too narrow here."), ek))
  if (n < 30) warnings <- c(warnings, "Small sample: the t-interval relies on approximately normal data.")
  contains0 <- tt$conf.int[1] <= 0 && tt$conf.int[2] >= 0
  list(
    test_name = "Confidence intervals for the mean, variance and standard deviation",
    hypotheses = NULL, n = n, statistic_name = NULL, statistic = NULL, df = n - 1, p_value = NULL,
    reject_null = NULL, decision = NULL,
    estimates = list(mean = m, variance = s2, sd = sqrt(s2)),
    confidence_interval = list(level = level, lower = tt$conf.int[1], upper = tt$conf.int[2], parameter = "population mean"),
    intervals = rows,
    results = NULL,
    interpretation = sprintf(paste0("We are %g%% confident that the population mean lies between %.5g and %.5g. ",
                                    "The interval %s 0, which matches a two-sided t-test of H0: mu = 0 at alpha = %g %s H0."),
                             100 * level, tt$conf.int[1], tt$conf.int[2], if (contains0) "contains" else "excludes",
                             alpha, if (contains0) "failing to reject" else "rejecting"),
    assumptions = I(c("Independent observations.",
                      "Mean interval: approximately normal sample mean (CLT for large n).",
                      "Variance/SD intervals: normally distributed data.")),
    warnings = I(warnings),
    chart = list(type = "intervals")
  )
}

test_normality <- function(x, alpha, label) {
  require_n(x, 8, "The normality tests")
  require_variation(x, "A normality test")
  n <- length(x)
  jb <- jarque_bera_test(x)
  rows <- list(result_row("Jarque-Bera", "JB", jb$statistic, 2, jb$p_value, alpha,
                          sprintf("Skewness %.3f, kurtosis %.3f (normal: 0 and 3)", jb$skewness, jb$kurtosis)))
  warnings <- character(0)
  sw <- NULL
  if (n <= 5000) {
    sw <- stats::shapiro.test(x)
    rows[[2]] <- result_row("Shapiro-Wilk", "W", sw$statistic, NA, sw$p.value, alpha)
  } else {
    warnings <- c(warnings, "Shapiro-Wilk is only defined for 3 to 5000 observations in R, so it was not run.")
  }
  if (n < 50) warnings <- c(warnings, "Jarque-Bera relies on a large-sample approximation; with n < 50 prefer Shapiro-Wilk.")
  if (n > 1000) warnings <- c(warnings, paste0(
    "With a large sample, normality tests detect even small departures from normality. ",
    "Judge the practical size of the departure from the Q-Q plot and the kurtosis."))
  primary_p <- if (!is.null(sw)) sw$p.value else jb$p_value
  primary_name <- if (!is.null(sw)) "Shapiro-Wilk" else "Jarque-Bera"
  sig <- primary_p < alpha
  list(
    test_name = "Normality tests",
    hypotheses = list(null = "H0: the data come from a normal distribution",
                      alternative = "H1: the data do not come from a normal distribution"),
    n = n, statistic_name = if (!is.null(sw)) "W" else "JB",
    statistic = if (!is.null(sw)) unname(sw$statistic) else jb$statistic,
    df = NULL, p_value = primary_p, reject_null = sig, decision = decision_text(primary_p, alpha),
    estimates = list(skewness = jb$skewness, kurtosis = jb$kurtosis, excess_kurtosis = jb$kurtosis - 3),
    results = do.call(rbind, rows),
    interpretation = if (sig) sprintf(paste0("%s rejects normality at the %g%% level (p = %.3g): the data are unlikely to ",
                                             "come from a normal distribution. Excess kurtosis of %.2f indicates %s tails."),
                                      primary_name, 100 * alpha, primary_p, jb$kurtosis - 3,
                                      if (jb$kurtosis > 3) "heavier" else "lighter")
                     else sprintf(paste0("%s does not reject normality at the %g%% level (p = %.3g). The data are consistent ",
                                         "with a normal distribution, but this does not prove they are normal."),
                                  primary_name, 100 * alpha, primary_p),
    assumptions = I(c("Independent observations.", "Jarque-Bera uses a large-sample chi-square(2) approximation.")),
    warnings = I(warnings),
    chart = c(list(type = "qq"), qq_points(x))
  )
}

test_two_sample_t <- function(x1, x2, alpha, alternative, labels) {
  require_n(x1, 2, sprintf("Group 1 (%s)", labels[1])); require_n(x2, 2, sprintf("Group 2 (%s)", labels[2]))
  if (stats::sd(x1) == 0 && stats::sd(x2) == 0) fx_degenerate("Both groups have zero variance; the t statistic is undefined.")
  tt <- stats::t.test(x2, x1, alternative = alternative, var.equal = FALSE, conf.level = 1 - alpha)
  diff <- mean(x2) - mean(x1)
  sig <- tt$p.value < alpha
  warnings <- character(0)
  if (min(length(x1), length(x2)) < 30) warnings <- c(warnings, "At least one group has fewer than 30 values; the test assumes roughly normal data.")
  list(
    test_name = "Welch two-sample t-test (unequal variances)",
    hypotheses = list(null = "H0: mu2 - mu1 = 0", alternative = sprintf("H1: mu2 - mu1 %s 0", alt_symbol(alternative))),
    n = length(x1) + length(x2), statistic_name = "t", statistic = unname(tt$statistic), df = unname(tt$parameter),
    p_value = tt$p.value, reject_null = sig, decision = decision_text(tt$p.value, alpha),
    groups = data.frame(group = c("Group 1", "Group 2"), label = labels, n = c(length(x1), length(x2)),
                        mean = c(mean(x1), mean(x2)), sd = c(stats::sd(x1), stats::sd(x2)),
                        variance = c(stats::var(x1), stats::var(x2))),
    estimates = list(difference = diff),
    confidence_interval = list(level = 1 - alpha, lower = tt$conf.int[1], upper = tt$conf.int[2],
                               parameter = "mu2 - mu1"),
    results = result_row("Welch two-sample t-test", "t", tt$statistic, round(tt$parameter, 1), tt$p.value, alpha),
    interpretation = if (sig) sprintf("At the %g%% level the mean of group 2 (%s) differs significantly from group 1 (%s): difference = %.5g, p = %.3g.",
                                      100 * alpha, labels[2], labels[1], diff, tt$p.value)
                     else sprintf(paste0("At the %g%% level there is not enough evidence that the means of the two groups differ ",
                                         "(difference = %.5g, p = %.3g). This does not show that the means are equal."),
                                  100 * alpha, diff, tt$p.value),
    assumptions = I(c("Independent observations within and between groups.",
                      "Approximately normal group means (CLT for large groups). Equal variances are NOT assumed.")),
    warnings = I(warnings),
    chart = null_distribution("t", unname(tt$statistic), alpha, alternative, df1 = unname(tt$parameter))
  )
}

test_variance_equality <- function(x1, x2, alpha, labels) {
  require_n(x1, 3, sprintf("Group 1 (%s)", labels[1])); require_n(x2, 3, sprintf("Group 2 (%s)", labels[2]))
  require_variation(x1, "The F-test (group 1)"); require_variation(x2, "The F-test (group 2)")
  ft <- stats::var.test(x2, x1, conf.level = 1 - alpha)
  bf <- brown_forsythe_test(x1, x2)
  rows <- rbind(result_row("F-test for equal variances", "F", ft$statistic,
                           paste(ft$parameter, collapse = ", "), ft$p.value, alpha, "Assumes normal data"),
                result_row("Brown-Forsythe (median-based Levene)", "F", bf$statistic,
                           paste(bf$df1, bf$df2, sep = ", "), bf$p_value, alpha, "Robust to non-normal data"))
  warnings <- character(0)
  ek <- max(kurtosis_b2(x1), kurtosis_b2(x2)) - 3
  if (is.finite(ek) && ek > 1) warnings <- c(warnings, "Heavy tails detected: the F-test can reject too often; rely on Brown-Forsythe.")
  sig_f <- ft$p.value < alpha; sig_bf <- bf$p_value < alpha
  list(
    test_name = "Tests for equal variances (volatility) in two groups",
    hypotheses = list(null = "H0: sigma2^2 = sigma1^2", alternative = "H1: sigma2^2 != sigma1^2"),
    n = length(x1) + length(x2), statistic_name = "F", statistic = unname(ft$statistic),
    df = paste(ft$parameter, collapse = ", "), p_value = ft$p.value, reject_null = sig_f,
    decision = decision_text(ft$p.value, alpha),
    groups = data.frame(group = c("Group 1", "Group 2"), label = labels, n = c(length(x1), length(x2)),
                        mean = c(mean(x1), mean(x2)), sd = c(stats::sd(x1), stats::sd(x2)),
                        variance = c(stats::var(x1), stats::var(x2))),
    estimates = list(variance_ratio = unname(ft$estimate), sd_ratio = sqrt(unname(ft$estimate))),
    confidence_interval = list(level = 1 - alpha, lower = ft$conf.int[1], upper = ft$conf.int[2],
                               parameter = "variance ratio sigma2^2 / sigma1^2"),
    results = rows,
    interpretation = sprintf(paste0("Variance ratio (group 2 / group 1) = %.3f (SD ratio %.3f). F-test: %s (p = %.3g). ",
                                    "Brown-Forsythe: %s (p = %.3g).%s"),
                             ft$estimate, sqrt(ft$estimate), decision_text(ft$p.value, alpha), ft$p.value,
                             decision_text(bf$p_value, alpha), bf$p_value,
                             if (sig_f && sig_bf) " Both tests indicate that volatility differs between the groups."
                             else if (!sig_f && !sig_bf) " Neither test finds evidence of different volatility (this does not prove equality)."
                             else " The tests disagree; with non-normal data the robust Brown-Forsythe result is more reliable."),
    assumptions = I(c("Independent observations.", "F-test: both groups normally distributed.",
                      "Brown-Forsythe: no normality assumption.")),
    warnings = I(warnings),
    chart = null_distribution("F", unname(ft$statistic), alpha, df1 = ft$parameter[[1]], df2 = ft$parameter[[2]])
  )
}

test_autocorrelation <- function(x, alpha, lag, label) {
  n <- length(x)
  require_n(x, 20, "The Ljung-Box test")
  require_variation(x, "Autocorrelation")
  if (lag >= n / 2) fx_abort(sprintf("'lag' must be less than half the sample size (%d).", floor(n / 2)), field = "lag")
  sq <- (x - mean(x))^2
  lb  <- stats::Box.test(x, lag = lag, type = "Ljung-Box")
  lbs <- stats::Box.test(sq, lag = lag, type = "Ljung-Box")
  max_lag <- min(20, n - 1)
  acf_r  <- stats::acf(x, lag.max = max_lag, plot = FALSE)$acf[-1]
  acf_sq <- stats::acf(sq, lag.max = max_lag, plot = FALSE)$acf[-1]
  bound <- stats::qnorm(1 - alpha / 2) / sqrt(n)
  rows <- rbind(result_row("Ljung-Box: values", "Q", lb$statistic, lag, lb$p.value, alpha, "Linear predictability"),
                result_row("Ljung-Box: squared deviations", "Q", lbs$statistic, lag, lbs$p.value, alpha,
                           "Volatility clustering"))
  list(
    test_name = sprintf("Ljung-Box tests for autocorrelation (lag %d)", lag),
    hypotheses = list(null = sprintf("H0: autocorrelations at lags 1-%d are all zero", lag),
                      alternative = "H1: at least one autocorrelation is non-zero"),
    n = n, statistic_name = "Q", statistic = unname(lb$statistic), df = lag, p_value = lb$p.value,
    reject_null = lb$p.value < alpha, decision = decision_text(lb$p.value, alpha),
    estimates = list(lag1_autocorrelation = acf_r[1], lag1_autocorrelation_squared = acf_sq[1]),
    results = rows,
    interpretation = paste(
      if (lb$p.value < alpha) sprintf("The values show statistically significant autocorrelation (p = %.3g); past values carry some linear information about future values.", lb$p.value)
      else sprintf("No significant autocorrelation in the values (p = %.3g), consistent with returns that are hard to predict linearly (this does not prove independence).", lb$p.value),
      if (lbs$p.value < alpha) sprintf("The squared deviations ARE autocorrelated (p = %.3g): large moves tend to be followed by large moves (volatility clustering).", lbs$p.value)
      else sprintf("No significant autocorrelation in squared deviations (p = %.3g).", lbs$p.value)),
    assumptions = I(c("Observations are in time order.", "Chi-square approximation with 'lag' degrees of freedom (large n).")),
    warnings = I(character(0)),
    chart = list(type = "acf", lags = I(seq_len(max_lag)), acf = I(acf_r), acf_squared = I(acf_sq), bound = bound)
  )
}

# ---- Entry point used by POST /hypothesis-test ------------------------------------------------------
run_hypothesis_test <- function(body) {
  test   <- v_choice(body$test, "test", TEST_TYPES, default = "one_sample_t")
  alpha  <- v_alpha(body$alpha)
  source <- v_choice(body$source, "source", c("historical", "custom"), default = "historical")
  alternative <- v_choice(body$alternative, "alternative", c("two.sided", "less", "greater"), default = "two.sided")
  two_sample <- test %in% c("two_sample_t", "variance_equality")
  start <- v_date(body$start_date, "start_date"); end <- v_date(body$end_date, "end_date")

  if (source == "historical") {
    variable <- v_choice(body$variable, "variable", c("log_return", "simple_return", "level"), default = "log_return")
    h <- historical_variable(variable, start, end)
    x <- h$values; dates <- h$dates
    data_desc <- h$description
    level_warning <- if (variable == "level") {
      paste0("Exchange-rate LEVELS are not independent observations (they trend and are strongly ",
             "autocorrelated), so test results and intervals based on them are not reliable. Use returns instead.")
    } else NULL
    if (two_sample) {
      split <- v_date(body$split_date, "split_date") %||% dates[floor(length(dates) / 2) + 1]
      x1 <- x[dates < split]; x2 <- x[dates >= split]
      labels <- c(sprintf("%s to %s", format(min(dates)), format(split - 1)),
                  sprintf("%s to %s", format(split), format(max(dates))))
      if (length(x1) < 3 || length(x2) < 3) fx_insufficient(sprintf(
        "The split date %s leaves too few observations on one side (%d before, %d from the split).",
        format(split), length(x1), length(x2)))
    }
    time_ordered <- TRUE
  } else {
    p1 <- parse_numeric_input(body$values, "values")
    x <- p1$values
    data_desc <- sprintf("User-entered sample (%d values%s)", length(x),
                         if (p1$n_missing_removed > 0) sprintf(", %d missing entries removed", p1$n_missing_removed) else "")
    if (two_sample) {
      p2 <- parse_numeric_input(body$values2, "values2")
      x1 <- x; x2 <- p2$values
      labels <- c("Sample 1", "Sample 2")
      data_desc <- sprintf("Two user-entered samples (n1 = %d, n2 = %d)", length(x1), length(x2))
    }
    level_warning <- NULL
    time_ordered <- FALSE
    variable <- "custom"
  }

  res <- switch(test,
    one_sample_t = test_one_sample_t(x, alpha, v_number(body$mu0, "mu0", default = 0), alternative,
                                     time_ordered, data_desc),
    confidence_interval = test_confidence_interval(x, alpha, data_desc, is_return = variable %in% c("log_return", "simple_return")),
    normality = test_normality(x, alpha, data_desc),
    two_sample_t = test_two_sample_t(x1, x2, alpha, alternative, labels),
    variance_equality = test_variance_equality(x1, x2, alpha, labels),
    autocorrelation = test_autocorrelation(x, alpha, v_integer(body$lag, "lag", min = 1, max = 50,
                                                                default = FXLAB_CONFIG$ljung_box_default_lag), data_desc)
  )
  if (!is.null(level_warning)) res$warnings <- I(c(level_warning, res$warnings))
  c(list(test = test, alpha = alpha, source = source, data_description = data_desc), res)
}
