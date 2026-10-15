# =============================================================================
# simulation.R
# -----------------------------------------------------------------------------
# Purpose : Simulate future USD/INR paths with Geometric Brownian Motion (GBM)
#           and summarise them by Monte Carlo.
#
# Model (time step dt = 1 trading day; mu and sigma are DAILY parameters):
#     S_(t+dt) = S_t * exp( (mu - sigma^2 / 2) * dt + sigma * sqrt(dt) * Z_t )
#   - S_t    exchange rate (INR per USD)
#   - mu     expected daily return (drift), entered in % per trading day
#   - sigma  daily volatility, entered in % per trading day
#   - Z_t    independent shocks with mean 0 and variance 1:
#              gbm_normal    : Z ~ N(0, 1)                 (classic GBM)
#              gbm_student_t : Z = T * sqrt((df - 2) / df), T ~ Student-t(df)
#              gbm_bootstrap : Z resampled from standardised historical log-returns
#   Because the update multiplies by exp(.), every simulated rate is positive.
#
# Exact results for gbm_normal (horizon T days):
#     ln(S_T / S_0) ~ N((mu - sigma^2/2) T, sigma^2 T)
#     E[S_T] = S_0 exp(mu T),  median = S_0 exp((mu - sigma^2/2) T)
#     SD[S_T] = S_0 exp(mu T) sqrt(exp(sigma^2 T) - 1)
# For the other shock models the mean and variance of ln(S_T / S_0) are the
# same, but S_T is not log-normal (and with Student-t shocks E[S_T] is not even
# finite in theory), so only the log-return moments are compared with theory.
#
# Memory: paths are advanced one day at a time, so only the current value of
# every path is stored, plus the paths shown in the chart. Every summary
# (percentile bands, terminal statistics, probabilities) uses ALL paths.
# =============================================================================

SIM_BAND_PROBS <- c(0.05, 0.25, 0.50, 0.75, 0.95)
SIM_TERMINAL_PROBS <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

# Validated simulation inputs. 'body' is the parsed JSON request.
validate_simulation_input <- function(body) {
  lim <- FXLAB_CONFIG$limits
  def <- FXLAB_CONFIG$default_simulation
  model_ids <- vapply(FXLAB_CONFIG$simulation_models, function(m) m$id, character(1))
  inp <- list(
    start_rate      = v_number(body$start_rate, "start_rate", min = 0, max = 1e6, min_exclusive = TRUE),
    mu_daily_pct    = v_number(body$mu_daily_pct, "mu_daily_pct", min = -lim$max_abs_daily_mu_pct,
                               max = lim$max_abs_daily_mu_pct),
    sigma_daily_pct = v_number(body$sigma_daily_pct, "sigma_daily_pct", min = 0, max = lim$max_daily_sigma_pct),
    days            = v_integer(body$days, "days", min = 1, max = lim$sim_max_days),
    n_paths         = v_integer(body$n_paths, "n_paths", min = 1, max = lim$sim_max_paths),
    model           = v_choice(body$model, "model", model_ids, default = def$model),
    t_df            = v_number(body$t_df, "t_df", min = lim$sim_min_t_df, max = lim$sim_max_t_df,
                               default = def$t_df),
    seed            = v_seed(body$seed),
    threshold       = v_number(body$threshold, "threshold", min = 0, max = 1e7, min_exclusive = TRUE,
                               required = FALSE),
    n_display_paths = v_integer(body$n_display_paths, "n_display_paths", min = 1,
                                max = lim$sim_max_display_paths, default = def$n_display_paths)
  )
  cells <- as.numeric(inp$days) * inp$n_paths
  if (cells > lim$sim_max_cells) {
    fx_abort(sprintf(paste0("days x n_paths = %s exceeds the limit of %s. Reduce the number of days or paths ",
                            "(for example %d paths allow at most %d days)."),
                     format(cells, big.mark = ","), format(lim$sim_max_cells, big.mark = ","),
                     inp$n_paths, floor(lim$sim_max_cells / inp$n_paths)), field = "n_paths")
  }
  inp
}

# Standardised historical shocks for the bootstrap model: mean 0 and variance 1
# exactly (population moments), so resampling them gives E[Z] = 0, Var[Z] = 1.
historical_shock_pool <- function() {
  h <- historical_variable("log_return")
  r <- h$values
  require_n(r, 30, "The bootstrap model")
  require_variation(r, "The bootstrap model")
  z <- (r - mean(r)) / sqrt(mean((r - mean(r))^2))
  list(z = z, description = h$description)
}

wilson_interval <- function(successes, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- successes / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(max(0, centre - half), min(1, centre + half))
}

# Core simulation. Arguments as returned by validate_simulation_input().
simulate_exchange_rate <- function(start_rate, mu_daily_pct, sigma_daily_pct, days, n_paths,
                                   model = "gbm_normal", t_df = 5, seed = NULL, threshold = NULL,
                                   n_display_paths = 20, shock_pool = NULL) {
  mu    <- mu_daily_pct / 100
  sigma <- sigma_daily_pct / 100
  dt    <- 1
  drift_step <- (mu - sigma^2 / 2) * dt
  vol_step   <- sigma * sqrt(dt)
  # Paths returned for charts: as requested, but at most sim_max_display_points values in total.
  k_wanted   <- min(n_display_paths, n_paths)
  k_show     <- max(1L, min(k_wanted, floor(FXLAB_CONFIG$limits$sim_max_display_points / (days + 1))))

  seed_used <- seed %||% sample.int(1e9, 1)
  set.seed(seed_used, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

  pool_info <- NULL
  draw <- switch(model,
    gbm_normal = function(n) stats::rnorm(n),
    gbm_student_t = function(n) stats::rt(n, df = t_df) * sqrt((t_df - 2) / t_df),
    gbm_bootstrap = {
      pool <- shock_pool %||% historical_shock_pool()
      pool_info <- list(size = length(pool$z), source = pool$description)
      function(n) pool$z[sample.int(length(pool$z), n, replace = TRUE)]
    }
  )

  log_s    <- rep(log(start_rate), n_paths)
  run_max  <- rep(start_rate, n_paths)
  run_min  <- rep(start_rate, n_paths)
  bands    <- matrix(NA_real_, days + 1, length(SIM_BAND_PROBS))
  bands[1, ] <- start_rate
  mean_path <- numeric(days + 1); mean_path[1] <- start_rate
  xs_sd    <- numeric(days + 1)                 # SD of the rate across paths, day by day
  shown    <- matrix(NA_real_, days + 1, k_show); shown[1, ] <- start_rate

  # Every simulated daily log return (all paths x all days) is summarised on the fly:
  # power sums of the deviation d = r - ret_center (exact moments without storing
  # an N x T matrix) and a fixed histogram grid of ret_center +/- 6 model SDs.
  ret_center <- 100 * drift_step                # expected daily log return (%)
  ret_scale  <- 100 * vol_step                  # model daily SD (%)
  half_width <- if (ret_scale > 0) 6 * ret_scale else max(abs(ret_center), 1e-6)
  ret_breaks <- seq(ret_center - half_width, ret_center + half_width, length.out = 61)
  ret_counts <- numeric(60)
  ret_outside <- 0
  p1 <- p2 <- p3 <- p4 <- 0
  d_min <- Inf; d_max <- -Inf
  within_2sd <- 0

  for (t in seq_len(days)) {
    z <- draw(n_paths)
    log_s <- log_s + drift_step + vol_step * z
    s <- exp(log_s)
    run_max <- pmax(run_max, s)
    run_min <- pmin(run_min, s)
    bands[t + 1, ]  <- stats::quantile(s, SIM_BAND_PROBS, type = 7, names = FALSE)
    mean_path[t + 1] <- mean(s)
    xs_sd[t + 1] <- if (n_paths > 1) stats::sd(s) else 0
    shown[t + 1, ]  <- s[seq_len(k_show)]

    d <- ret_scale * z                           # daily log return (%) minus ret_center
    p1 <- p1 + sum(d); p2 <- p2 + sum(d^2); p3 <- p3 + sum(d^3); p4 <- p4 + sum(d^4)
    d_min <- min(d_min, d); d_max <- max(d_max, d)
    within_2sd <- within_2sd + sum(abs(z) < 2)
    idx <- findInterval(ret_center + d, ret_breaks, rightmost.closed = TRUE)
    inside <- idx >= 1 & idx <= 60
    ret_counts <- ret_counts + tabulate(idx[inside], nbins = 60)
    ret_outside <- ret_outside + sum(!inside)
  }
  terminal <- exp(log_s)
  if (any(!is.finite(terminal)) || any(terminal <= 0)) {
    fx_abort("The simulation produced values outside the representable range. Use a smaller drift or volatility.",
             "DEGENERATE_DATA", 422L)
  }

  horizon <- days * dt
  log_ret_T <- log(terminal / start_rate)
  sd_T <- if (n_paths > 1) stats::sd(terminal) else NA_real_
  se_mean <- if (n_paths > 1) sd_T / sqrt(n_paths) else NA_real_
  z95 <- stats::qnorm(0.975)

  terminal_summary <- list(
    n_paths = n_paths, mean = mean(terminal), median = stats::median(terminal), sd = sd_T,
    min = min(terminal), max = max(terminal),
    mean_standard_error = se_mean,
    mean_ci95 = I(if (is.finite(se_mean)) mean(terminal) + c(-1, 1) * z95 * se_mean else c(NA, NA)),
    percentiles = data.frame(probability = SIM_TERMINAL_PROBS, percentile = 100 * SIM_TERMINAL_PROBS,
                             value = stats::quantile(terminal, SIM_TERMINAL_PROBS, type = 7, names = FALSE)),
    log_return_mean_pct = 100 * mean(log_ret_T),
    log_return_sd_pct = if (n_paths > 1) 100 * stats::sd(log_ret_T) else NA_real_,
    prob_above_start = mean(terminal > start_rate),
    mean_return_pct = 100 * (mean(terminal) / start_rate - 1),        # E[S_T]/S_0 - 1
    median_return_pct = 100 * (stats::median(terminal) / start_rate - 1),
    lowest_point = min(run_min),                                     # lowest rate reached by any path
    highest_point = max(run_max)
  )

  # ---- Simulated daily log returns (all paths x all days) ---------------------------------
  n_r <- n_paths * days
  m1  <- p1 / n_r
  c2  <- max(0, p2 / n_r - m1^2)
  c3  <- p3 / n_r - 3 * m1 * p2 / n_r + 2 * m1^3
  c4  <- p4 / n_r - 4 * m1 * p3 / n_r + 6 * m1^2 * p2 / n_r - 3 * m1^4
  ret_skew <- if (c2 > 0) c3 / c2^1.5 else NA_real_
  ret_kurt <- if (c2 > 0) c4 / c2^2 else NA_real_
  ret_var  <- if (n_r > 1) c2 * n_r / (n_r - 1) else NA_real_
  jb_stat  <- if (is.finite(ret_skew)) n_r / 6 * (ret_skew^2 + (ret_kurt - 3)^2 / 4) else NA_real_
  jb_p     <- if (is.finite(jb_stat)) stats::pchisq(jb_stat, 2, lower.tail = FALSE) else NA_real_
  widths   <- diff(ret_breaks)
  lower_b  <- ret_breaks[-61]; upper_b <- ret_breaks[-1]
  ret_hist <- data.frame(
    lower = lower_b, upper = upper_b, mid = (lower_b + upper_b) / 2, count = ret_counts,
    density = ret_counts / (n_r * widths),
    # Normal reference with the model's mean and SD (exact for normal shocks).
    normal_expected_count = if (ret_scale > 0) n_r * (stats::pnorm(upper_b, ret_center, ret_scale) -
                                                       stats::pnorm(lower_b, ret_center, ret_scale)) else NA_real_
  )
  daily_returns <- list(
    n = n_r, unit = "% per trading day (log return)",
    mean_pct = ret_center + m1, sd_pct = sqrt(ret_var), variance = ret_var,
    skewness = ret_skew, kurtosis = ret_kurt, excess_kurtosis = ret_kurt - 3,
    min_pct = ret_center + d_min, max_pct = ret_center + d_max,
    model_mean_pct = ret_center, model_sd_pct = ret_scale,
    histogram = ret_hist, n_outside_histogram = ret_outside,
    within_2sd = list(lower_pct = ret_center - 2 * ret_scale, upper_pct = ret_center + 2 * ret_scale,
                      share = if (ret_scale > 0) within_2sd / n_r else 1,
                      normal_share = 2 * stats::pnorm(2) - 1),
    jarque_bera = list(statistic = jb_stat, df = 2, p_value = jb_p,
                       decision = if (is.finite(jb_p)) (if (jb_p < 0.05) "Reject normality (5% level)" else
                         "No evidence against normality (5% level)") else "Not defined (zero variance)")
  )

  # ---- Theory -------------------------------------------------------------------
  m_log <- (mu - sigma^2 / 2) * horizon
  s_log <- sigma * sqrt(horizon)
  theory <- list(
    log_return_mean_pct = 100 * m_log,
    log_return_sd_pct = 100 * s_log,
    note = if (model == "gbm_normal") "Exact log-normal results for GBM with normal shocks." else
      paste0("Only the mean and SD of ln(S_T/S_0) are exact for this shock model; S_T is not log-normal. ",
             if (model == "gbm_student_t") "With Student-t shocks the theoretical mean of S_T is infinite, so judge the simulation by its median and percentiles." else "")
  )
  if (model == "gbm_normal") {
    theory$mean <- start_rate * exp(mu * horizon)
    theory$median <- start_rate * exp(m_log)
    theory$sd <- start_rate * exp(mu * horizon) * sqrt(exp(sigma^2 * horizon) - 1)
    theory$percentiles <- data.frame(probability = SIM_TERMINAL_PROBS,
                                     value = start_rate * exp(m_log + s_log * stats::qnorm(SIM_TERMINAL_PROBS)))
    theory$prob_above_start <- if (s_log > 0) stats::pnorm(m_log / s_log) else as.numeric(m_log > 0)
  }

  # ---- Threshold probabilities -----------------------------------------------------
  threshold_out <- NULL
  if (!is.null(threshold)) {
    above <- sum(terminal > threshold)
    below <- sum(terminal < threshold)
    touch <- if (threshold >= start_rate) sum(run_max >= threshold) else sum(run_min <= threshold)
    threshold_out <- list(
      threshold = threshold,
      prob_above = above / n_paths, prob_above_ci95 = I(wilson_interval(above, n_paths)),
      prob_below = below / n_paths, prob_below_ci95 = I(wilson_interval(below, n_paths)),
      prob_touch = touch / n_paths, prob_touch_ci95 = I(wilson_interval(touch, n_paths)),
      touch_definition = if (threshold >= start_rate)
        "Share of paths whose rate reaches or exceeds the threshold on at least one simulated day."
      else "Share of paths whose rate falls to or below the threshold on at least one simulated day.",
      theory_prob_above = if (model == "gbm_normal") {
        if (s_log > 0) stats::pnorm((log(threshold / start_rate) - m_log) / s_log, lower.tail = FALSE)
        else as.numeric(start_rate * exp(m_log) > threshold)
      } else NULL
    )
  }

  # ---- Convergence of the Monte Carlo mean (Law of Large Numbers) --------------------
  convergence <- NULL
  if (n_paths >= 20) {
    checkpoints <- unique(round(exp(seq(log(10), log(n_paths), length.out = 60))))
    y  <- terminal - start_rate                         # centred for numerical accuracy
    cs <- cumsum(y); cs2 <- cumsum(y^2)
    k  <- checkpoints
    run_mean <- cs[k] / k
    run_var  <- pmax(0, (cs2[k] - k * run_mean^2) / (k - 1))
    convergence <- list(paths = I(k), running_mean = I(start_rate + run_mean),
                        lower95 = I(start_rate + run_mean - z95 * sqrt(run_var / k)),
                        upper95 = I(start_rate + run_mean + z95 * sqrt(run_var / k)),
                        theory_mean = theory$mean)
  }

  # ---- Displayed paths and their own statistics ----------------------------------------
  lr <- 100 * diff(log(shown))                   # daily log returns of the displayed paths (days x k)
  if (!is.matrix(lr)) lr <- matrix(lr, nrow = days)
  path_stats <- data.frame(
    path = seq_len(k_show), final_rate = shown[days + 1, ],
    total_return_pct = 100 * (shown[days + 1, ] / start_rate - 1),
    max_rate = apply(shown, 2, max), min_rate = apply(shown, 2, min),
    mean_daily_log_return_pct = colMeans(lr),
    realised_daily_vol_pct = if (days > 1) apply(lr, 2, stats::sd) else NA_real_
  )

  terminal_hist <- histogram_bins(terminal, max_bins = 50)
  if (model == "gbm_normal" && s_log > 0 && !is.null(terminal_hist)) {
    terminal_hist$theoretical_density <- stats::dlnorm(terminal_hist$mid, log(start_rate) + m_log, s_log)
  }

  list(
    inputs = list(start_rate = start_rate, mu_daily_pct = mu_daily_pct, sigma_daily_pct = sigma_daily_pct,
                  days = days, n_paths = n_paths, model = model,
                  t_df = if (model == "gbm_student_t") t_df else NULL,
                  threshold = threshold, n_display_paths = k_show, dt_days = dt,
                  mu_annualised_pct = mu_daily_pct * FXLAB_CONFIG$trading_days_per_year,
                  sigma_annualised_pct = sigma_daily_pct * sqrt(FXLAB_CONFIG$trading_days_per_year)),
    seed = seed_used,
    seed_source = if (is.null(seed)) "generated" else "user",
    bootstrap_pool = pool_info,
    terminal = terminal_summary,
    theory = theory,
    threshold = threshold_out,
    terminal_histogram = terminal_hist,
    # Chart series are rounded to 8 significant digits to keep the response small.
    # signif() (not round()) keeps tiny but positive rates positive.
    bands = list(day = I(0:days),
                 p05 = I(signif(bands[, 1], 8)), p25 = I(signif(bands[, 2], 8)),
                 p50 = I(signif(bands[, 3], 8)), p75 = I(signif(bands[, 4], 8)),
                 p95 = I(signif(bands[, 5], 8)), mean = I(signif(mean_path, 8)),
                 cross_sectional_sd = I(signif(xs_sd, 8))),
    # Displayed paths use 6 significant digits (0.001 INR at USD/INR levels; up to sim_max_display_points values).
    paths = list(day = I(0:days), n_requested = k_wanted, n_returned = k_show, capped = k_show < k_wanted,
                 values = lapply(seq_len(k_show), function(j) I(signif(shown[, j], 6)))),
    path_statistics = path_stats,
    daily_returns = daily_returns,
    convergence = convergence,
    all_positive = all(terminal > 0) && all(run_min > 0)
  )
}

# Defaults for the simulation forms, estimated from the historical data when
# available. mu is estimated as mean(log return) + variance / 2 so that GBM's
# expected log return (mu - sigma^2/2) matches the sample mean log return.
simulation_defaults <- function() {
  def <- FXLAB_CONFIG$default_simulation
  base <- list(days = def$days, n_paths = def$n_paths, n_display_paths = def$n_display_paths,
               seed = def$seed, model = def$model, t_df = def$t_df)
  est <- tryCatch({
    h <- historical_variable("log_return")
    ds <- get_dataset("usdinr")
    r <- h$values / 100
    list(start_rate = utils::tail(ds$data$value, 1),
         mu_daily_pct = 100 * (mean(r) + stats::var(r) / 2),
         sigma_daily_pct = 100 * stats::sd(r),
         source = "historical",
         description = sprintf(paste0("Start rate = last observation in the dataset (%s, not a live quote); ",
                                      "drift and volatility estimated from %s."),
                               format(max(ds$data$date)), h$description),
         as_of = format(max(ds$data$date)))
  }, fxlab_error = function(e) NULL)
  if (is.null(est)) {
    fb <- FXLAB_CONFIG$fallback_simulation
    est <- list(start_rate = fb$start_rate, mu_daily_pct = fb$mu_daily_pct, sigma_daily_pct = fb$sigma_daily_pct,
                source = "example",
                description = "No historical data available: these are illustrative example inputs, not market data.",
                as_of = NULL)
  }
  est$threshold <- round(est$start_rate * 1.05, 2)
  c(est, base)
}
