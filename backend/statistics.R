# =============================================================================
# statistics.R
# -----------------------------------------------------------------------------
# Purpose : Descriptive statistics, returns and volatility. These functions are
#           used by the API and can be called directly from the RStudio console.
#
# Conventions (documented in README.md, section "Statistical formulas"):
#   Simple return (%)   R_t = 100 * (S_t / S_(t-1) - 1)
#   Log return (%)      r_t = 100 * ln(S_t / S_(t-1))
#   Returns are measured between consecutive VALID observations; days without
#   a published rate (holidays) are skipped, never filled in.
#   Cumulative return   100 * (S_t / S_0 - 1)
#   Variance / SD       SAMPLE versions, divisor n - 1 (R's var() and sd()).
#                       The population variance (divisor n) is reported too.
#   Quantiles           R's default definition (type 7).
#   Skewness            g1 = m3 / m2^(3/2), with m_k = mean((x - mean(x))^k)
#   Kurtosis            b2 = m4 / m2^2 (normal = 3); excess kurtosis = b2 - 3
#   Daily volatility    SD of daily log returns (%)
#   Annualised vol.     daily SD * sqrt(252) - assumes ~252 trading days a year
#                       and returns that are roughly uncorrelated day to day.
# =============================================================================

# ---- Returns --------------------------------------------------------------------------
compute_returns <- function(prices) {
  if (!is.numeric(prices) || any(!is.finite(prices))) {
    fx_abort("Returns need finite numeric prices.", "INVALID_DATA", 422L)
  }
  if (any(prices <= 0)) {
    fx_abort("Returns need strictly positive prices (an exchange rate cannot be zero or negative).",
             "INVALID_DATA", 422L)
  }
  require_n(prices, 2, "Calculating returns")
  n <- length(prices)
  list(
    simple_pct = 100 * (prices[-1] / prices[-n] - 1),
    log_pct    = 100 * diff(log(prices))
  )
}

cumulative_return_pct <- function(prices) 100 * (prices / prices[1] - 1)

# ---- Moments ----------------------------------------------------------------------------
central_moment <- function(x, k) mean((x - mean(x))^k)

skewness_g1 <- function(x) {
  m2 <- central_moment(x, 2)
  if (length(x) < 3 || m2 <= 0) return(NA_real_)
  central_moment(x, 3) / m2^1.5
}

kurtosis_b2 <- function(x) {
  m2 <- central_moment(x, 2)
  if (length(x) < 4 || m2 <= 0) return(NA_real_)
  central_moment(x, 4) / m2^2
}

# ---- Descriptive statistics ------------------------------------------------------------
QUANTILE_PROBS <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

# Summary of one numeric sample. Non-finite values are removed and COUNTED.
describe_sample <- function(x, include_cv = FALSE) {
  if (!is.numeric(x)) fx_abort("Descriptive statistics need numeric data.", "INVALID_DATA", 422L)
  n_input    <- length(x)
  n_missing  <- sum(is.na(x))
  n_nonfinite <- sum(!is.na(x) & !is.finite(x))
  x <- x[is.finite(x)]
  require_n(x, 2, "Descriptive statistics (variance and standard deviation)")

  n   <- length(x)
  warnings <- character(0)
  if (n_missing > 0) warnings <- c(warnings, sprintf("%d missing value(s) were excluded.", n_missing))
  if (n_nonfinite > 0) warnings <- c(warnings, sprintf("%d non-finite value(s) (Inf/NaN) were excluded.", n_nonfinite))

  s    <- stats::sd(x)
  zero_var <- s <= 1e-12 * max(1, abs(mean(x)))
  if (zero_var) {
    warnings <- c(warnings, "All values are identical: variance is zero, so skewness and kurtosis are undefined.")
  }
  if (n < 30) warnings <- c(warnings, sprintf("Small sample (n = %d): estimates are imprecise.", n))

  q <- stats::quantile(x, QUANTILE_PROBS, type = FXLAB_CONFIG$quantile_type, names = FALSE)
  skew <- if (zero_var) NA_real_ else skewness_g1(x)
  kurt <- if (zero_var) NA_real_ else kurtosis_b2(x)

  out <- list(
    n                   = n,
    n_input             = n_input,
    n_excluded          = n_input - n,
    mean                = mean(x),
    median              = stats::median(x),
    min                 = min(x),
    max                 = max(x),
    range               = max(x) - min(x),
    variance            = stats::var(x),            # sample variance, divisor n - 1
    variance_population = stats::var(x) * (n - 1) / n,
    sd                  = s,                        # sample SD, divisor n - 1
    standard_error      = s / sqrt(n),
    q1                  = q[QUANTILE_PROBS == 0.25],
    q3                  = q[QUANTILE_PROBS == 0.75],
    iqr                 = q[QUANTILE_PROBS == 0.75] - q[QUANTILE_PROBS == 0.25],
    skewness            = skew,
    kurtosis            = kurt,
    excess_kurtosis     = kurt - 3,
    quantiles           = data.frame(probability = QUANTILE_PROBS, percentile = 100 * QUANTILE_PROBS,
                                     value = q),
    warnings            = I(warnings)
  )
  if (include_cv) {
    out$cv_pct <- if (abs(mean(x)) > 0) 100 * s / abs(mean(x)) else NA_real_
  }
  out
}

# Plain-language reading of skewness and excess kurtosis.
describe_shape <- function(skew, excess_kurt) {
  if (is.na(skew) || is.na(excess_kurt)) return("Shape statistics are undefined for this sample.")
  sk <- if (abs(skew) < 0.1) "approximately symmetric" else if (skew > 0) "right-skewed (longer upper tail)"
        else "left-skewed (longer lower tail)"
  ku <- if (excess_kurt > 1) "heavier tails than a normal distribution (leptokurtic)"
        else if (excess_kurt < -1) "lighter tails than a normal distribution (platykurtic)"
        else "tails broadly similar to a normal distribution"
  sprintf("The distribution is %s, with %s (excess kurtosis = %.2f).", sk, ku, excess_kurt)
}

# ---- Volatility ---------------------------------------------------------------------------
# Rolling sample SD over the last 'window' values (NA until enough data).
rolling_sd <- function(x, window) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n < window || window < 2) return(out)
  m   <- stats::embed(x, window)           # row i holds x[i + window - 1], ..., x[i]
  mu  <- rowMeans(m)
  out[window:n] <- sqrt(rowSums((m - mu)^2) / (window - 1))
  out
}

volatility_measures <- function(log_returns_pct, dates = NULL,
                                window = FXLAB_CONFIG$rolling_window,
                                trading_days = FXLAB_CONFIG$trading_days_per_year) {
  r <- log_returns_pct[is.finite(log_returns_pct)]
  require_n(r, 2, "Volatility")
  daily_sd <- stats::sd(r)
  out <- list(
    daily_sd_pct          = daily_sd,
    daily_variance        = stats::var(r),
    annualised_vol_pct    = daily_sd * sqrt(trading_days),
    mean_abs_return_pct   = mean(abs(r)),
    trading_days_per_year = trading_days,
    rolling_window        = window
  )
  if (!is.null(dates) && length(r) >= window) {
    roll <- rolling_sd(log_returns_pct, window) * sqrt(trading_days)
    ok <- which(is.finite(roll))
    out$rolling_latest_pct = roll[max(ok)]
    out$rolling_latest_date = format(dates[max(ok)])
    out$rolling_max_pct  = max(roll[ok])
    out$rolling_max_date = format(dates[ok][which.max(roll[ok])])
    out$rolling_min_pct  = min(roll[ok])
    out$rolling_min_date = format(dates[ok][which.min(roll[ok])])
  }
  out
}

# ---- Data for charts (computed in R, drawn by the browser) ----------------------------------
# Equal-width histogram. The number of bins follows the Freedman-Diaconis rule,
# limited to [min_bins, max_bins].
histogram_bins <- function(x, n_bins = NULL, min_bins = 10, max_bins = 60) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) return(NULL)
  lo <- min(x); hi <- max(x)
  if (hi - lo <= 1e-12 * max(1, abs(lo))) {
    # All values identical: one bin of unit width centred on the value.
    w <- max(abs(lo) * 0.01, 0.5)
    return(data.frame(lower = lo - w / 2, upper = lo + w / 2, mid = lo, count = n,
                      density = 1 / w, relative_frequency = 1))
  }
  k <- if (is.null(n_bins)) grDevices::nclass.FD(x) else n_bins
  k <- max(min_bins, min(max_bins, k))
  breaks <- seq(lo, hi, length.out = k + 1)
  counts <- graphics::hist(x, breaks = breaks, plot = FALSE, right = TRUE, include.lowest = TRUE)$counts
  width  <- diff(breaks)
  data.frame(lower = breaks[-(k + 1)], upper = breaks[-1], mid = (breaks[-1] + breaks[-(k + 1)]) / 2,
             count = counts, density = counts / (n * width), relative_frequency = counts / n)
}

# Normal density with the sample mean and SD, for overlaying on the histogram.
normal_overlay <- function(x, points = 200) {
  x <- x[is.finite(x)]
  m <- mean(x); s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NULL)
  lo <- min(min(x), m - 4 * s); hi <- max(max(x), m + 4 * s)
  grid <- seq(lo, hi, length.out = points)
  list(x = I(grid), density = I(stats::dnorm(grid, m, s)), mean = m, sd = s)
}

# Normal Q-Q plot data: theoretical N(0,1) quantiles against standardised sample
# quantiles. Long samples are thinned evenly for display (extremes always kept).
qq_points <- function(x, max_points = 500) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 3) return(NULL)
  s <- stats::sd(x)
  if (s <= 0) return(NULL)
  theo <- stats::qnorm(stats::ppoints(n))
  samp <- (x - mean(x)) / s
  idx  <- if (n > max_points) unique(round(seq(1, n, length.out = max_points))) else seq_len(n)
  list(theoretical = I(theo[idx]), sample = I(samp[idx]), n = n, n_shown = length(idx))
}

# Share of observations more than k standard deviations from the mean, compared
# with the share a normal distribution would give. Ratios above 1 = heavier tails.
tail_comparison <- function(x, ks = 1:4) {
  x <- x[is.finite(x)]
  s <- stats::sd(x)
  if (length(x) < 3 || s <= 0) return(NULL)
  z <- abs(x - mean(x)) / s
  emp  <- vapply(ks, function(k) mean(z > k), numeric(1))
  norm <- 2 * stats::pnorm(-ks)
  data.frame(k_sd = ks, empirical_share = emp, normal_share = norm,
             empirical_count = vapply(ks, function(k) sum(z > k), numeric(1)),
             ratio = emp / norm)
}

# Evenly spaced indices for thinning long series in charts.
thin_index <- function(n, max_points) {
  if (n <= max_points) return(seq_len(n))
  unique(round(seq(1, n, length.out = max_points)))
}

# ---- Full descriptive analysis for a sample ---------------------------------------------------
# kind: "returns" (percent returns), "level" (exchange-rate levels) or "sample" (generic numbers)
descriptive_analysis <- function(x, label, kind = c("sample", "returns", "level"), dates = NULL) {
  kind <- match.arg(kind)
  stats_out <- describe_sample(x, include_cv = kind %in% c("level", "sample"))
  xf <- x[is.finite(x)]
  out <- list(
    label      = label,
    kind       = kind,
    statistics = stats_out,
    shape_interpretation = describe_shape(stats_out$skewness, stats_out$excess_kurtosis),
    histogram  = histogram_bins(xf),
    normal_fit = normal_overlay(xf),
    qq         = qq_points(xf),
    tails      = tail_comparison(xf)
  )
  if (kind == "returns") {
    out$volatility <- volatility_measures(x, dates = dates)
  }
  out
}

# ---- Entry point used by POST /statistics -------------------------------------------------------
# source = "historical": variable "log_return" | "simple_return" | "level", optional date window.
# source = "custom": 'values' (array or text). data_type "sample" (any numbers) or
#                    "prices" (positive rates in time order; returns are derived from them).
statistics_request <- function(body) {
  source <- v_choice(body$source, "source", c("historical", "custom"), default = "historical")
  if (source == "historical") {
    variable <- v_choice(body$variable, "variable", c("log_return", "simple_return", "level"), default = "log_return")
    h <- historical_variable(variable, v_date(body$start_date, "start_date"), v_date(body$end_date, "end_date"))
    out <- descriptive_analysis(h$values, h$label, if (variable == "level") "level" else "returns", h$dates)
    out$data_description <- h$description
    out$source <- source
    return(out)
  }
  data_type <- v_choice(body$data_type, "data_type", c("sample", "prices"), default = "sample")
  p <- parse_numeric_input(body$values, "values")
  x <- p$values
  if (data_type == "prices") {
    if (any(x <= 0)) fx_abort("Prices / exchange rates must all be positive.", field = "values")
    require_n(x, 3, "Return statistics from prices")
    r <- compute_returns(x)
    out <- descriptive_analysis(r$log_pct, "Log returns of the entered prices (%)", "returns")
    out$level_statistics <- describe_sample(x, include_cv = TRUE)
    out$cumulative_return_pct <- 100 * (x[length(x)] / x[1] - 1)
    out$simple_return_mean_pct <- mean(r$simple_pct)
    out$note <- "Annualised volatility assumes the prices are daily observations (252 per year)."
  } else {
    out <- descriptive_analysis(x, "Entered sample", "sample")
  }
  out$data_description <- sprintf("User-entered %s (%d values%s)", if (data_type == "prices") "prices" else "sample",
                                  length(x), if (p$n_missing_removed > 0)
                                    sprintf("; %d missing entries were removed", p$n_missing_removed) else "")
  out$n_missing_removed <- p$n_missing_removed
  out$source <- source
  out
}
