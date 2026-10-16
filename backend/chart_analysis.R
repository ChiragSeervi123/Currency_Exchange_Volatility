# =============================================================================
# chart_analysis.R
# -----------------------------------------------------------------------------
# Purpose : Everything FXLab shows about an uploaded chart, computed in R from the
#           price series read out of the image (backend/chart_image.R). The
#           sections follow the Probability & Statistics syllabus:
#
#   descriptive  Unit 4 - central tendency (mean, median, mode), dispersion,
#                raw and central moments, skewness, kurtosis, the moment
#                generating function; histogram with a fitted normal curve.
#   probability  Units 1-3 - probability of up / down candles, conditional
#                probability, independence, Bayes' theorem; the joint (bivariate)
#                distribution of consecutive candles with marginal and
#                conditional distributions; binomial, Poisson and normal models
#                with their mean, variance, skewness and kurtosis.
#   hypothesis   Unit 5 - point and interval estimation, large-sample z-tests
#                (mean, proportion, difference of means), small-sample t-test,
#                F-test for variances, chi-square goodness of fit and
#                independence, one-way ANOVA - and a reasoned outlook (up, down
#                or close to the mean line) built from those results.
#   regression   Unit 6 - scatter diagram, least-squares line, Pearson and
#                Spearman rank correlation, multiple regression.
#
# Unit of analysis: one candle (or one sampled point of a line chart). Returns are
# log returns in percent per candle: r_t = 100 * ln(P_t / P_(t-1)), where P is the
# closing price. Significance level: alpha = 0.05 throughout.
# =============================================================================

CHART_ALPHA <- 0.05

# ---- Small helpers --------------------------------------------------------------------------
z_crit <- function(alpha, tails) stats::qnorm(1 - alpha / tails)
p_from_z <- function(z, alternative) switch(alternative, two.sided = 2 * stats::pnorm(-abs(z)),
                                            greater = stats::pnorm(z, lower.tail = FALSE), less = stats::pnorm(z))
p_from_t <- function(t, df, alternative) switch(alternative, two.sided = 2 * stats::pt(-abs(t), df),
                                                greater = stats::pt(t, df, lower.tail = FALSE), less = stats::pt(t, df))
alt_word <- function(alternative) c(two.sided = "Two-tailed", greater = "Right-tailed (one-tailed)", less = "Left-tailed (one-tailed)")[[alternative]]

# Density curve of a test statistic under H0 with its critical region, for the charts.
null_curve <- function(dist, observed, crit, alternative, df1 = NULL, df2 = NULL) {
  if (dist == "z" || dist == "t") {
    lim <- max(4, min(abs(observed) * 1.2, 12))
    x <- seq(-lim, lim, length.out = 241)
    dens <- if (dist == "z") stats::dnorm(x) else stats::dt(x, df1)
  } else if (dist == "F") {
    hi <- max(stats::qf(0.995, df1, df2), min(observed * 1.2, stats::qf(0.9999, df1, df2) * 3))
    x <- seq(0, hi, length.out = 241)
    dens <- stats::df(x, df1, df2)
  } else {
    hi <- max(stats::qchisq(0.995, df1), min(observed * 1.2, stats::qchisq(0.9999, df1) * 3))
    x <- seq(0, hi, length.out = 241)
    dens <- stats::dchisq(x, df1)
  }
  dens[!is.finite(dens)] <- NA
  list(distribution = dist, df1 = df1, df2 = df2, x = I(x), density = I(dens), critical = I(crit),
       alternative = alternative, observed = observed, observed_off_scale = observed > max(x) || observed < min(x))
}

# One test result in a common shape: the page shows formula, hypotheses, numbers, decision.
test_result <- function(id, title, formula, h0, h1, alternative, statistic_name, statistic, critical, p_value,
                        df = NULL, conclusion, details = list(), chart = NULL, note = NULL) {
  reject <- is.finite(p_value) && p_value < CHART_ALPHA
  list(id = id, title = title, formula = formula, h0 = h0, h1 = h1, tails = alt_word(alternative),
       statistic_name = statistic_name, statistic = statistic, df = df, critical = I(critical),
       p_value = p_value, alpha = CHART_ALPHA, reject = reject,
       decision = if (reject) "Reject H0" else "Fail to reject H0",
       conclusion = conclusion, details = details, chart = chart, note = note)
}

# Grouped-data mode from a histogram: Mo = L + (f1 - f0) / (2 f1 - f0 - f2) * h.
grouped_mode <- function(hist) {
  k <- which.max(hist$count)
  f1 <- hist$count[k]; f0 <- if (k > 1) hist$count[k - 1] else 0; f2 <- if (k < nrow(hist)) hist$count[k + 1] else 0
  h <- hist$upper[k] - hist$lower[k]
  den <- 2 * f1 - f0 - f2
  list(value = if (den > 0) hist$lower[k] + (f1 - f0) / den * h else hist$mid[k],
       modal_class = c(hist$lower[k], hist$upper[k]), f1 = f1, f0 = f0, f2 = f2, h = h)
}

moments_table <- function(x) {
  m <- mean(x)
  raw <- vapply(1:4, function(k) mean(x^k), numeric(1))
  central <- c(0, vapply(2:4, function(k) mean((x - m)^k), numeric(1)))     # mu_1 is 0 by definition
  list(raw = I(raw), central = I(central),
       beta1 = central[3]^2 / central[2]^3, beta2 = central[4] / central[2]^2,
       gamma1 = central[3] / central[2]^1.5, gamma2 = central[4] / central[2]^2 - 3)
}

# ---- Descriptive statistics (Unit 4) ----------------------------------------------------------
chart_descriptive <- function(close, r) {
  rs <- describe_sample(r, include_cv = FALSE)
  ps <- describe_sample(close, include_cv = TRUE)
  rh <- histogram_bins(r)
  ph <- histogram_bins(close)
  m <- mean(r); s <- stats::sd(r)
  rh$normal_expected_count <- length(r) * (stats::pnorm(rh$upper, m, s) - stats::pnorm(rh$lower, m, s))
  # Observed vs normal frequencies in standard-deviation bands (bar chart).
  cuts <- c(-Inf, -2, -1, 0, 1, 2, Inf)
  z <- (r - m) / s
  obs <- as.vector(table(cut(z, cuts)))
  bands <- data.frame(band = c("below -2σ", "-2σ to -1σ", "-1σ to mean", "mean to +1σ",
                               "+1σ to +2σ", "above +2σ"),
                      observed = obs, observed_share = obs / length(r),
                      normal_share = diff(stats::pnorm(cuts)), stringsAsFactors = FALSE)
  bands$normal_count <- bands$normal_share * length(r)
  # Moment generating function of the returns (in % units): empirical M(t) = mean(e^(t r))
  # against the normal MGF exp(mu t + sigma^2 t^2 / 2), for small t.
  tt <- seq(-1, 1, length.out = 41) / max(s, 1e-9)
  mgf <- list(t = I(tt), empirical = I(vapply(tt, function(t) mean(exp(t * r)), numeric(1))),
              normal = I(exp(m * tt + s^2 * tt^2 / 2)))
  list(
    returns = c(rs, list(mode = grouped_mode(rh), moments = moments_table(r))),
    prices = c(ps, list(mode = grouped_mode(ph))),
    returns_histogram = rh, normal_fit = normal_overlay(r), price_histogram = ph,
    sigma_bands = bands, mgf = mgf,
    shape_interpretation = describe_shape(rs$skewness, rs$excess_kurtosis)
  )
}

# ---- Probability (Units 1-3) -------------------------------------------------------------------
chart_probability <- function(r) {
  n <- length(r)
  dir <- ifelse(r > 0, "Up", ifelse(r < 0, "Down", "Flat"))
  n_up <- sum(dir == "Up"); n_down <- sum(dir == "Down"); n_flat <- sum(dir == "Flat")
  nd <- n_up + n_down
  p_up <- n_up / nd; p_down <- n_down / nd
  # Pairs of consecutive candles (previous, current), flat moves left out.
  prev <- dir[-n]; cur <- dir[-1]
  ok <- prev != "Flat" & cur != "Flat"
  prev <- factor(prev[ok], c("Up", "Down")); cur <- factor(cur[ok], c("Up", "Down"))
  counts <- unclass(table(prev, cur))         # a plain 2 x 2 matrix of counts
  npair <- sum(counts)
  joint <- counts / npair
  marg_prev <- rowSums(joint); marg_cur <- colSums(joint)
  cond <- counts / rowSums(counts)                       # P(current | previous)
  p_uu <- cond["Up", "Up"]; p_ud <- cond["Down", "Up"]   # P(Up now | Up before), P(Up now | Down before)
  bayes <- p_uu * marg_prev[["Up"]] / marg_cur[["Up"]]   # P(Up before | Up now)

  s <- stats::sd(r); m <- mean(r)
  big <- abs(r - m) > s
  p_big <- mean(big)
  p_big_after_big <- if (sum(big[-n]) > 0) mean(big[-1][big[-n]]) else NA_real_

  # Binomial: number of up candles among the next m candles, X ~ Bin(m, p).
  mm <- 10
  k <- 0:mm
  pmf <- stats::dbinom(k, mm, p_up)
  q_up <- 1 - p_up
  blocks <- floor(length(dir) / mm)
  observed_binom <- NULL
  if (blocks >= 5) {
    ups <- vapply(seq_len(blocks), function(b) sum(dir[((b - 1) * mm + 1):(b * mm)] == "Up"), numeric(1))
    observed_binom <- as.vector(table(factor(ups, k))) / blocks
  }
  binom <- list(m = mm, p = p_up, q = q_up, k = I(k), pmf = I(pmf), observed_share = if (!is.null(observed_binom)) I(observed_binom) else NULL,
                n_blocks = blocks, mean = mm * p_up, variance = mm * p_up * q_up, sd = sqrt(mm * p_up * q_up),
                skewness = if (p_up * q_up > 0) (q_up - p_up) / sqrt(mm * p_up * q_up) else NA_real_,
                kurtosis = if (p_up * q_up > 0) 3 + (1 - 6 * p_up * q_up) / (mm * p_up * q_up) else NA_real_,
                p_at_least_6 = sum(pmf[k >= 6]), p_at_most_4 = sum(pmf[k <= 4]), mode_k = k[which.max(pmf)])

  # Poisson: number of large moves (|r - mean| > 2 SD) in blocks of 20 candles.
  bl <- 20
  nb <- floor(n / bl)
  extreme <- abs(r - m) > 2 * s
  poisson <- NULL
  if (nb >= 3) {
    cnt <- vapply(seq_len(nb), function(b) sum(extreme[((b - 1) * bl + 1):(b * bl)]), numeric(1))
    lambda <- mean(cnt)
    kk <- 0:max(4, max(cnt) + 1)
    poisson <- list(block = bl, n_blocks = nb, lambda = lambda, counts = I(cnt), k = I(kk),
                    pmf = I(stats::dpois(kk, lambda)), observed_share = I(as.vector(table(factor(cnt, kk))) / nb),
                    sample_mean = mean(cnt), sample_variance = if (nb > 1) stats::var(cnt) else NA_real_,
                    mean = lambda, variance = lambda,
                    skewness = if (lambda > 0) 1 / sqrt(lambda) else NA_real_,
                    kurtosis = if (lambda > 0) 3 + 1 / lambda else NA_real_,
                    p_zero = stats::dpois(0, lambda), p_at_least_1 = 1 - stats::dpois(0, lambda))
  }

  # Normal model of the returns: R ~ N(mean, SD^2); theory against the observed shares.
  thr <- c(0, m + s, m - s)
  normal <- list(mean = m, sd = s, variance = s^2, skewness = 0, kurtosis = 3,
                 mgf = sprintf("M(t) = exp(%.4g t + %.4g t^2 / 2)", m, s^2),
                 comparisons = data.frame(
                   event = c("Candle closes higher: R > 0", "Gain larger than 1 SD: R > mean + SD",
                             "Loss larger than 1 SD: R < mean - SD", "Within 2 SD of the mean",
                             "Beyond 2 SD of the mean (extreme move)"),
                   normal = c(1 - stats::pnorm(0, m, s), 1 - stats::pnorm(thr[2], m, s), stats::pnorm(thr[3], m, s),
                              stats::pnorm(2) - stats::pnorm(-2), 2 * stats::pnorm(-2)),
                   observed = c(mean(r > 0), mean(r > thr[2]), mean(r < thr[3]), mean(abs(r - m) <= 2 * s), mean(abs(r - m) > 2 * s)),
                   stringsAsFactors = FALSE))

  list(
    n = n, n_up = n_up, n_down = n_down, n_flat = n_flat, p_up = p_up, p_down = p_down,
    flat_note = if (n_flat > 0) sprintf("%d candle(s) closed at the same pixel row as the previous one and are left out of the up/down probabilities.", n_flat) else NULL,
    joint = list(rows = I(c("Up", "Down")), cols = I(c("Up", "Down")), n_pairs = npair,
                 counts = I(unname(as.matrix(counts))), probabilities = I(unname(as.matrix(joint))),
                 marginal_previous = I(unname(marg_prev)), marginal_current = I(unname(marg_cur)),
                 conditional = I(unname(as.matrix(cond)))),
    conditional = list(p_up_given_up = p_uu, p_up_given_down = p_ud, p_down_given_down = cond["Down", "Down"],
                       p_down_given_up = cond["Up", "Down"]),
    independence = list(p_joint_up_up = joint["Up", "Up"], product = marg_prev[["Up"]] * marg_cur[["Up"]],
                        difference = joint["Up", "Up"] - marg_prev[["Up"]] * marg_cur[["Up"]]),
    bayes = list(p_prev_up = marg_prev[["Up"]], p_up_given_up = p_uu, p_up_now = marg_cur[["Up"]], result = bayes),
    big_moves = list(threshold = s, p_big = p_big, p_big_after_big = p_big_after_big),
    binomial = binom, poisson = poisson, normal = normal
  )
}

# ---- Hypothesis tests (Unit 5) and the outlook -----------------------------------------------------
chart_mean_test <- function(x, label, alternative = "two.sided", id = "mean") {
  n <- length(x); m <- mean(x); s <- stats::sd(x); se <- s / sqrt(n)
  large <- n >= 30
  stat <- m / se
  tails <- if (alternative == "two.sided") 2 else 1
  if (large) {
    crit <- z_crit(CHART_ALPHA, tails); p <- p_from_z(stat, alternative); df <- NULL; dist <- "z"
  } else {
    crit <- stats::qt(1 - CHART_ALPHA / tails, n - 1); p <- p_from_t(stat, n - 1, alternative); df <- n - 1; dist <- "t"
  }
  crit_vals <- switch(alternative, two.sided = c(-crit, crit), greater = crit, less = -crit)
  sym <- c(two.sided = "≠", greater = ">", less = "<")[[alternative]]
  sig <- p < CHART_ALPHA
  word <- if (m > 0) "rising" else "falling"
  list(n = n, mean = m, sd = s, se = se, large = large, stat = stat, crit = crit_vals, p = p, df = df,
       result = test_result(
         id, label,
         if (large) "Z = (x̄ − μ₀) / (s / √n)    (large sample, n ≥ 30)" else "t = (x̄ − μ₀) / (s / √n),  df = n − 1    (small sample, n < 30)",
         "H0: μ = 0 (no drift: the average return per candle is zero)",
         sprintf("H1: μ %s 0", sym), alternative, if (large) "Z" else "t", stat, crit_vals, p, df,
         if (sig) sprintf("The average return of %.4f%% per candle is significantly %s zero: the chart shows a real %s drift.",
                          m, c(two.sided = "different from", greater = "greater than", less = "less than")[[alternative]], word)
         else sprintf("The average return of %.4f%% per candle is not significantly %s zero: the drift could be chance.",
                      m, c(two.sided = "different from", greater = "greater than", less = "less than")[[alternative]]),
         details = list(n = n, mean = m, sd = s, standard_error = se),
         chart = null_curve(dist, stat, crit_vals, alternative, df1 = df)))
}

chart_hypothesis <- function(close, r, prob, trend) {
  n <- length(r); m <- mean(r); s <- stats::sd(r); se <- s / sqrt(n)
  tests <- list()

  # Point and interval estimates
  zc <- z_crit(CHART_ALPHA, 2)
  ci_mean <- if (n >= 30) c(m - zc * se, m + zc * se) else m + c(-1, 1) * stats::qt(0.975, n - 1) * se
  nd <- prob$n_up + prob$n_down; p <- prob$p_up; q <- 1 - p
  ci_p <- p + c(-1, 1) * zc * sqrt(p * q / nd)

  # 1. Mean return (two-tailed) and in the direction of the sample mean (one-tailed)
  t1 <- chart_mean_test(r, "Test for the mean return (drift)", "two.sided", "mean_two")
  tests[[1]] <- t1$result
  dir_alt <- if (m >= 0) "greater" else "less"
  t1b <- chart_mean_test(r, sprintf("One-tailed test: is the drift %s?", if (m >= 0) "positive" else "negative"), dir_alt, "mean_one")
  tests[[2]] <- t1b$result

  # 2. Proportion of up candles (large-sample test for a proportion)
  zp <- (p - 0.5) / sqrt(0.25 / nd)
  pp <- p_from_z(zp, "two.sided")
  tests[[3]] <- test_result("proportion", "Test for the proportion of up candles",
    "Z = (p̂ − P₀) / √(P₀ Q₀ / n),   P₀ = 0.5, Q₀ = 1 − P₀",
    "H0: P = 0.5 (up and down candles are equally likely)", "H1: P ≠ 0.5", "two.sided", "Z", zp,
    c(-zc, zc), pp, NULL,
    if (pp < CHART_ALPHA) sprintf("Up candles make up %.1f%% of the moves, significantly %s than half.", 100 * p, if (p > 0.5) "more" else "fewer")
    else sprintf("Up candles make up %.1f%% of the moves; this is not significantly different from 50%%.", 100 * p),
    details = list(n = nd, p_hat = p, q_hat = q, P0 = 0.5, Q0 = 0.5, standard_error = sqrt(0.25 / nd)),
    chart = null_curve("z", zp, c(-zc, zc), "two.sided"))

  # 3. Small-sample t-test on the most recent candles (recent momentum)
  k <- min(15, max(5, floor(n / 3)))
  recent <- utils::tail(r, k)
  if (stats::sd(recent) > 0) {
    tr <- mean(recent) / (stats::sd(recent) / sqrt(k))
    alt_r <- "two.sided"
    crit_r <- stats::qt(0.975, k - 1)
    pr <- p_from_t(tr, k - 1, alt_r)
    tests[[length(tests) + 1]] <- test_result("recent_t", sprintf("Small-sample t-test: the last %d candles", k),
      "t = (x̄ − μ₀) / (s / √n),  df = n − 1",
      sprintf("H0: μ = 0 for the last %d candles (no recent drift)", k), "H1: μ ≠ 0", alt_r, "t", tr,
      c(-crit_r, crit_r), pr, k - 1,
      if (pr < CHART_ALPHA) sprintf("The recent candles have a significant %s drift (mean %.4f%% per candle).", if (tr > 0) "upward" else "downward", mean(recent))
      else sprintf("The recent mean of %.4f%% per candle is not significantly different from zero.", mean(recent)),
      details = list(n = k, mean = mean(recent), sd = stats::sd(recent)),
      chart = null_curve("t", tr, c(-crit_r, crit_r), alt_r, df1 = k - 1))
  }

  # 4. Difference of means: first half vs second half
  h <- floor(n / 2)
  x1 <- r[seq_len(h)]; x2 <- r[(h + 1):n]
  se_d <- sqrt(stats::var(x1) / length(x1) + stats::var(x2) / length(x2))
  zd <- (mean(x2) - mean(x1)) / se_d
  large_d <- min(length(x1), length(x2)) >= 30
  if (large_d) { crit_d <- zc; pd <- p_from_z(zd, "two.sided"); df_d <- NULL; dist_d <- "z" } else {
    wt <- stats::t.test(x2, x1, var.equal = FALSE)
    df_d <- unname(wt$parameter); crit_d <- stats::qt(0.975, df_d); pd <- wt$p.value; dist_d <- "t"
  }
  tests[[length(tests) + 1]] <- test_result("two_means", "Difference of means: first half vs second half of the chart",
    if (large_d) "Z = (x̄₂ − x̄₁) / √(s₁²/n₁ + s₂²/n₂)" else "t = (x̄₂ − x̄₁) / √(s₁²/n₁ + s₂²/n₂)   (Welch)",
    "H0: μ₁ = μ₂ (the drift did not change)", "H1: μ₁ ≠ μ₂", "two.sided",
    if (large_d) "Z" else "t", zd, c(-crit_d, crit_d), pd, df_d,
    if (pd < CHART_ALPHA) sprintf("The average return changed significantly between the halves (%.4f%% → %.4f%% per candle).", mean(x1), mean(x2))
    else sprintf("No significant change in the average return between the halves (%.4f%% → %.4f%% per candle).", mean(x1), mean(x2)),
    details = list(n1 = length(x1), n2 = length(x2), mean1 = mean(x1), mean2 = mean(x2), sd1 = stats::sd(x1), sd2 = stats::sd(x2)),
    chart = null_curve(dist_d, zd, c(-crit_d, crit_d), "two.sided", df1 = df_d))

  # 5. F-test for equality of variances (volatility) of the two halves
  v1 <- stats::var(x1); v2 <- stats::var(x2)
  if (v1 > 0 && v2 > 0) {
    big_first <- v1 >= v2
    Fv <- if (big_first) v1 / v2 else v2 / v1
    d1 <- (if (big_first) length(x1) else length(x2)) - 1; d2 <- (if (big_first) length(x2) else length(x1)) - 1
    pf <- min(1, 2 * stats::pf(Fv, d1, d2, lower.tail = FALSE))
    crit_f <- stats::qf(1 - CHART_ALPHA / 2, d1, d2)
    tests[[length(tests) + 1]] <- test_result("variance_f", "F-test for variances: volatility of the two halves",
      "F = s₁² / s₂²  (larger variance on top),  df = (n₁ − 1, n₂ − 1)",
      "H0: σ₁² = σ₂² (volatility did not change)", "H1: σ₁² ≠ σ₂²", "two.sided", "F", Fv,
      crit_f, pf, sprintf("%d, %d", d1, d2),
      if (pf < CHART_ALPHA) sprintf("Volatility changed significantly: the SD per candle went from %.4f%% to %.4f%%.", sqrt(v1), sqrt(v2))
      else sprintf("No significant change in volatility (SD %.4f%% → %.4f%% per candle).", sqrt(v1), sqrt(v2)),
      details = list(var1 = v1, var2 = v2, sd1 = sqrt(v1), sd2 = sqrt(v2)),
      chart = null_curve("F", Fv, crit_f, "greater", df1 = d1, df2 = d2),
      note = "The F-test assumes normally distributed returns; with heavy tails it rejects too easily.")
  }

  # 6. Chi-square goodness of fit: are the returns normal?
  cuts <- c(-Inf, -2, -1, -0.5, 0, 0.5, 1, 2, Inf)
  z <- (r - m) / s
  obs <- as.vector(table(cut(z, cuts)))
  expd <- n * diff(stats::pnorm(cuts))
  # Merge outer classes until every expected frequency is at least 5.
  labs <- c("< -2", "-2 to -1", "-1 to -0.5", "-0.5 to 0", "0 to 0.5", "0.5 to 1", "1 to 2", "> 2")
  while (length(expd) > 3 && min(expd) < 5) {
    i <- which.min(expd)
    j <- if (i == 1) 2 else if (i == length(expd)) i - 1 else if (expd[i - 1] < expd[i + 1]) i - 1 else i + 1
    a <- min(i, j); b <- max(i, j)
    obs[a] <- obs[a] + obs[b]; expd[a] <- expd[a] + expd[b]
    labs[a] <- paste0(sub(" to .*| *$", "", labs[a]), " to ", sub(".* to ", "", labs[b]))
    obs <- obs[-b]; expd <- expd[-b]; labs <- labs[-b]
  }
  dfc <- length(obs) - 1 - 2
  if (dfc >= 1) {
    chi <- sum((obs - expd)^2 / expd)
    pc <- stats::pchisq(chi, dfc, lower.tail = FALSE)
    tests[[length(tests) + 1]] <- test_result("chisq_gof", "Chi-square test of goodness of fit: normal distribution",
      "χ² = Σ (O − E)² / E,  df = k − 1 − 2  (mean and SD estimated)",
      "H0: the returns follow a normal distribution", "H1: they do not", "greater", "χ²", chi,
      stats::qchisq(1 - CHART_ALPHA, dfc), pc, dfc,
      if (pc < CHART_ALPHA) "The returns do not fit a normal distribution well (see the skewness and kurtosis)."
      else "The returns are consistent with a normal distribution.",
      details = list(classes = data.frame(z_class = labs, observed = obs, expected = expd, contribution = (obs - expd)^2 / expd)),
      chart = null_curve("chisq", chi, stats::qchisq(1 - CHART_ALPHA, dfc), "greater", df1 = dfc))
  }

  # 7. Chi-square test of independence: previous direction x current direction
  ct <- prob$joint$counts
  if (all(rowSums(ct) > 0) && all(colSums(ct) > 0)) {
    e <- outer(rowSums(ct), colSums(ct)) / sum(ct)
    chi_i <- sum((ct - e)^2 / e)
    pi <- stats::pchisq(chi_i, 1, lower.tail = FALSE)
    cp <- prob$conditional
    tests[[length(tests) + 1]] <- test_result("chisq_independence", "Chi-square test of independence: does one candle predict the next?",
      "χ² = Σ (O − E)² / E,  E = (row total × column total) / N,  df = (r − 1)(c − 1) = 1",
      "H0: the direction of a candle is independent of the previous candle", "H1: they are associated", "greater",
      "χ²", chi_i, stats::qchisq(1 - CHART_ALPHA, 1), pi, 1,
      if (pi < CHART_ALPHA) sprintf("Consecutive candles are associated: P(up | previous up) = %.1f%% vs P(up | previous down) = %.1f%% - %s.",
                                    100 * cp$p_up_given_up, 100 * cp$p_up_given_down,
                                    if (cp$p_up_given_up > cp$p_up_given_down) "moves tend to continue (momentum)" else "moves tend to reverse (mean reversion)")
      else sprintf("No significant association: P(up | previous up) = %.1f%%, P(up | previous down) = %.1f%%.", 100 * cp$p_up_given_up, 100 * cp$p_up_given_down),
      details = list(observed = I(unname(ct)), expected = I(unname(e))),
      chart = null_curve("chisq", chi_i, stats::qchisq(1 - CHART_ALPHA, 1), "greater", df1 = 1))
  }

  # 8. One-way ANOVA: are the mean returns equal in four consecutive periods?
  g <- 4
  if (n >= 4 * g) {
    grp <- factor(ceiling(seq_len(n) * g / n), labels = paste("Period", seq_len(g)))
    a <- stats::anova(stats::lm(r ~ grp))
    Fa <- a[["F value"]][1]; pa <- a[["Pr(>F)"]][1]; df1 <- a$Df[1]; df2 <- a$Df[2]
    means <- tapply(r, grp, mean)
    tests[[length(tests) + 1]] <- test_result("anova", "Analysis of variance (ANOVA): mean return in four periods",
      "F = MSB / MSW = [SSB / (k − 1)] / [SSW / (N − k)]",
      "H0: μ₁ = μ₂ = μ₃ = μ₄ (the drift was the same in every quarter of the chart)",
      "H1: at least one period has a different mean", "greater", "F", Fa, stats::qf(1 - CHART_ALPHA, df1, df2), pa,
      sprintf("%d, %d", df1, df2),
      if (pa < CHART_ALPHA) "The average return differed significantly between the four periods: the trend was not steady."
      else "The four periods have statistically similar average returns: the behaviour was steady across the chart.",
      details = list(groups = data.frame(period = names(means), n = as.vector(table(grp)), mean = as.vector(means),
                                         sd = as.vector(tapply(r, grp, stats::sd))),
                     ss_between = a[["Sum Sq"]][1], ss_within = a[["Sum Sq"]][2],
                     ms_between = a[["Mean Sq"]][1], ms_within = a[["Mean Sq"]][2]),
      chart = null_curve("F", Fa, stats::qf(1 - CHART_ALPHA, df1, df2), "greater", df1 = df1, df2 = df2))
  }

  list(alpha = CHART_ALPHA,
       estimation = list(n = n, mean = m, sd = s, variance = s^2, standard_error = se,
                         ci_mean = I(ci_mean), ci_method = if (n >= 30) "Z interval: x̄ ± 1.96 s/√n" else "t interval: x̄ ± t(0.025, n − 1) s/√n",
                         p_hat = p, q_hat = q, n_moves = nd, ci_p = I(ci_p)),
       tests = tests,
       outlook = chart_outlook(close, r, t1b, prob, trend, tests))
}

# Our hypothesis about where the price goes next, assembled from the test results.
chart_outlook <- function(close, r, one_tailed, prob, trend, tests) {
  n <- length(r); m <- mean(r); s <- stats::sd(r)
  last <- utils::tail(close, 1)
  find <- function(id) { for (t in tests) if (identical(t$id, id)) return(t); NULL }
  recent <- find("recent_t"); indep <- find("chisq_independence"); ftest <- find("variance_f")
  drift_sig <- one_tailed$p < CHART_ALPHA
  recent_sig <- !is.null(recent) && recent$reject
  recent_dir <- if (!is.null(recent)) sign(recent$statistic) else 0

  # Where is the last price relative to the mean line (average price) and the trend line?
  mean_price <- mean(close); sd_price <- stats::sd(close)
  z_mean <- (last - mean_price) / sd_price
  fitted_last <- utils::tail(trend$chart$fitted, 1)
  z_trend <- (last - fitted_last) / trend$residuals$sd

  if (drift_sig) {
    verdict <- if (m > 0) "up" else "down"
    strength <- if (recent_sig && recent_dir == sign(m)) "strong" else "moderate"
  } else if (recent_sig) {
    verdict <- if (recent_dir > 0) "up" else "down"; strength <- "weak"
  } else {
    verdict <- "sideways"; strength <- if (abs(z_mean) < 1) "strong" else "moderate"
  }
  headline <- switch(verdict,
    up = "Our hypothesis: the price is more likely to keep moving UP",
    down = "Our hypothesis: the price is more likely to keep moving DOWN",
    sideways = "Our hypothesis: the price is likely to stay CLOSE TO ITS MEAN LINE")

  h <- 10
  med <- last * exp(h * m / 100)
  lo <- last * exp((h * m - 1.96 * s * sqrt(h)) / 100); hi <- last * exp((h * m + 1.96 * s * sqrt(h)) / 100)
  p_higher <- stats::pnorm(m * sqrt(h) / s)

  reasons <- c(
    sprintf("Average return: %.4f%% per candle; the one-tailed test gives p = %s, so the %s drift is %s at the 5%% level.",
            m, format_p(one_tailed$p), if (m >= 0) "upward" else "downward", if (drift_sig) "statistically significant" else "NOT significant"),
    sprintf("Up candles: %.1f%% of the moves (p̂ = %.3f, q̂ = %.3f).", 100 * prob$p_up, prob$p_up, 1 - prob$p_up),
    if (!is.null(recent)) sprintf("Recent momentum (last %d candles): mean %.4f%% per candle, %s (p = %s).", recent$details$n, recent$details$mean,
                                  if (recent$reject) "significant" else "not significant", format_p(recent$p_value)),
    sprintf("Trend line: price = %.6g %s %.6g × candle (R² = %.3f); the last price is %.2f residual SDs %s the line.",
            trend$coefficients$estimate[1], if (trend$coefficients$estimate[2] < 0) "−" else "+", abs(trend$coefficients$estimate[2]),
            trend$fit$r_squared, abs(z_trend), if (z_trend >= 0) "above" else "below"),
    sprintf("Mean line: the average price on the chart is %.6g; the last price (%.6g) is %.2f SDs %s it.", mean_price, last, abs(z_mean),
            if (z_mean >= 0) "above" else "below")
  )
  insights <- c(
    if (!is.null(indep)) { cp <- prob$conditional
      if (indep$reject) sprintf("Candles are not independent: after an up candle the next is up %.0f%% of the time, after a down candle %.0f%% - %s.",
                                100 * cp$p_up_given_up, 100 * cp$p_up_given_down,
                                if (cp$p_up_given_up > cp$p_up_given_down) "momentum (moves continue)" else "mean reversion (moves reverse)")
      else "Each candle's direction is statistically independent of the previous one (chi-square test), as in a random walk." },
    if (!is.null(ftest)) (if (ftest$reject) sprintf("Volatility changed between the two halves of the chart (SD %.3f%% → %.3f%% per candle).",
                                                    ftest$details$sd1, ftest$details$sd2)
                          else "Volatility was stable across the chart (F-test not significant)."),
    if (abs(z_trend) > 2) sprintf("The price is stretched %s the trend line (%.1f residual SDs); a move back toward the line is common after such stretches.",
                                  if (z_trend > 0) "above" else "below", abs(z_trend)),
    if (verdict == "sideways") sprintf("With no significant drift, prices tend to fluctuate around the mean line; about 95%% of the next-%d-candle outcomes fall between %.6g and %.6g under the random-walk model.", h, lo, hi)
  )
  steps <- 0:h
  projection <- list(step = I(steps), median = I(last * exp(steps * m / 100)),
                     low = I(last * exp((steps * m - 1.96 * s * sqrt(steps)) / 100)),
                     high = I(last * exp((steps * m + 1.96 * s * sqrt(steps)) / 100)))
  list(verdict = verdict, strength = strength, headline = headline, horizon = h, last_price = last,
       expected_price = med, range_low = lo, range_high = hi, p_higher = p_higher, projection = projection,
       mean_price = mean_price, z_mean = z_mean, z_trend = z_trend,
       reasons = I(reasons[!vapply(reasons, is.null, logical(1))]), insights = I(unlist(insights)),
       method = sprintf("Projection: random walk with the estimated drift and volatility, P(h) = P_last × exp(h·μ ± 1.96·σ·√h), h = %d candles.", h),
       caveat = paste("This outlook describes what the statistics of THIS chart suggest. It is not a forecast guarantee or investment",
                      "advice: prices read from an image are approximate and past behaviour may not continue."))
}

format_p <- function(p) if (!is.finite(p)) "—" else if (p < 0.0001) "< 0.0001" else sprintf("%.4f", p)

# ---- Correlation and regression (Unit 6) ------------------------------------------------------------
chart_regression <- function(close, r, series) {
  n <- length(close)
  t <- seq_len(n)
  trend <- fit_simple_regression(t, close, CHART_ALPHA, x_label = "Candle number", y_label = "Price")
  # Spearman rank correlation, by the textbook formula (no ties in t; ties in price get average ranks).
  rt <- rank(t); rp <- rank(close)
  d <- rt - rp
  rho_formula <- 1 - 6 * sum(d^2) / (n * (n^2 - 1))
  lag <- fit_simple_regression(r[-length(r)], r[-1], CHART_ALPHA, x_label = "Previous return (%)", y_label = "Current return (%)")
  quad <- stats::lm(close ~ t + I(t^2))
  sq <- summary(quad)
  co <- sq$coefficients
  f <- sq$fstatistic
  curve_word <- if (co[3, 4] < CHART_ALPHA) (if (co[3, 1] > 0) "curving upwards (the trend is accelerating or a bottom has formed)"
                                              else "curving downwards (the trend is slowing or a top has formed)") else "not significantly curved"
  multiple <- list(
    equation = sprintf("Price = %.6g %s %.6g t %s %.6g t²", co[1, 1], if (co[2, 1] < 0) "−" else "+", abs(co[2, 1]),
                       if (co[3, 1] < 0) "−" else "+", abs(co[3, 1])),
    coefficients = data.frame(term = c("Intercept (b0)", "t (candle number)", "t²"), estimate = co[, 1], std_error = co[, 2],
                              t_value = co[, 3], p_value = co[, 4], row.names = NULL),
    r_squared = sq$r.squared, adj_r_squared = sq$adj.r.squared, f_statistic = unname(f["value"]),
    f_df1 = unname(f["numdf"]), f_df2 = unname(f["dendf"]),
    f_p_value = stats::pf(f["value"], f["numdf"], f["dendf"], lower.tail = FALSE)[[1]],
    fitted = I(unname(stats::fitted(quad))),
    interpretation = sprintf("Adding t² raises R² from %.3f to %.3f. The price path is %s.", trend$fit$r_squared, sq$r.squared, curve_word))
  range_corr <- NULL
  if (!all(is.na(series$high))) {
    rng <- 100 * (series$high - series$low) / series$close
    ab <- abs(c(NA, r))
    ok <- is.finite(rng) & is.finite(ab)
    if (sum(ok) > 5 && stats::sd(rng[ok]) > 0 && stats::sd(ab[ok]) > 0) {
      range_corr <- list(pearson = stats::cor(rng[ok], ab[ok]), spearman = stats::cor(rng[ok], ab[ok], method = "spearman"),
                         n = sum(ok), x = I(rng[ok]), y = I(ab[ok]))
    }
  }
  list(trend = trend,
       spearman = list(rho = rho_formula, rho_r = unname(suppressWarnings(stats::cor.test(t, close, method = "spearman", exact = FALSE))$estimate),
                       sum_d2 = sum(d^2), n = n, p_value = suppressWarnings(stats::cor.test(t, close, method = "spearman", exact = FALSE))$p.value),
       lag = lag, multiple = multiple, range_vs_return = range_corr,
       warnings = I(c("Prices on a chart trend and depend on their previous values, so the p-values of the price-on-time regressions are",
                      "optimistic; read the slope as a description of the average change per candle.")))
}

# ---- Entry point ------------------------------------------------------------------------------------
analyse_chart_series <- function(ext, chart_name = NULL, interval = NULL, file_name = NULL) {
  s <- ext$series
  close <- s$close
  r <- compute_returns(close)$log_pct
  require_variation(r, "The analysis of returns")
  if (length(r) < 9) fx_insufficient("At least 10 price points are needed for the analysis.")
  descriptive <- chart_descriptive(close, r)
  probability <- chart_probability(r)
  regression <- chart_regression(close, r, s)
  hypothesis <- chart_hypothesis(close, r, probability, regression$trend)
  n <- length(close)
  roll_w <- if (length(r) >= 40) 10 else 5
  notes <- c(
    "All prices were read from the image, so each is accurate only to about one pixel row of the chart.",
    if (ext$type == "line") paste("This is a line chart: its points are sampled every", CHART_IMAGE$line_step_px, "pixels along the drawn line,",
                                  "not the original data points. Points on the same straight segment move in the same direction, which can",
                                  "make moves look more persistent (momentum) than they were."),
    if (ext$scale$method == "manual") "The price scale comes from the highest and lowest prices you entered, not from the chart's axis.")
  name <- chart_name %||% ext$legend$title %||% "Uploaded chart"
  iv <- interval %||% ext$legend$interval
  list(
    chart = list(name = name, interval = iv, file_name = file_name, type = ext$type, n_points = n,
                 image = ext$image, scale = ext$scale, resolution = ext$resolution, plot_box = ext$plot_box,
                 time_labels = ext$time_axis$labels, detected_title = ext$legend$title, detected_interval = ext$legend$interval,
                 notes = I(notes),
                 unit = sprintf("per %s", if (is.null(iv)) "candle" else paste(iv, "candle")),
                 analysed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    series = list(index = I(s$index), x = I(s$x), open = I(s$open), high = I(s$high), low = I(s$low), close = I(close),
                  colour = I(s$colour), y_close = I(s$y_close), y_high = I(s$y_high), y_low = I(s$y_low)),
    returns = I(r),
    rolling = list(window = roll_w, sd = I(rolling_sd(r, roll_w))),
    overview = list(first = close[1], last = close[n], change = close[n] - close[1], change_pct = 100 * (close[n] / close[1] - 1),
                    high = max(c(s$high, close), na.rm = TRUE), low = min(c(s$low, close), na.rm = TRUE),
                    mean_price = mean(close), sd_return = stats::sd(r), mean_return = mean(r),
                    n_up = probability$n_up, n_down = probability$n_down, n_flat = probability$n_flat,
                    jarque_bera = jarque_bera_test(r)),
    descriptive = descriptive, probability = probability, hypothesis = hypothesis, regression = regression
  )
}

# ---- Requests (POST /chart/analyze) and use from the R console ------------------------------------
# body: image_base64 (required; a data URL or plain base64), file_name, chart_name, interval,
#       manual_high and manual_low (both or neither: the price range when the axis cannot be read).
chart_analyze_request <- function(body) {
  b64 <- body$image_base64
  if (is_missing_value(b64) || !is.character(b64) || length(b64) != 1) {
    fx_abort("'image_base64' is required: the screenshot as base64 text.", field = "image")
  }
  b64 <- sub("^data:[^,]*,", "", b64)
  raw <- tryCatch(jsonlite::base64_dec(gsub("[[:space:]]", "", b64)),
                  error = function(e) fx_abort("'image_base64' is not valid base64.", field = "image"))
  clean_text <- function(x, field, max_len) {
    if (is_missing_value(x)) return(NULL)
    if (!is.character(x) || length(x) != 1) fx_abort(sprintf("'%s' must be text.", field), field = field)
    x <- gsub("[[:cntrl:]]", "", trimws(x))
    if (nchar(x) > max_len) fx_abort(sprintf("'%s' must be at most %d characters.", field, max_len), field = field)
    if (nzchar(x)) x else NULL
  }
  chart_name <- clean_text(body$chart_name, "chart_name", 70)
  interval <- clean_text(body$interval, "interval", 10)
  if (!is.null(interval) && !(interval %in% FXLAB_CONFIG$chart_intervals)) {
    fx_abort(sprintf("'interval' must be one of: %s.", paste(FXLAB_CONFIG$chart_intervals, collapse = ", ")), field = "interval")
  }
  file_name <- clean_text(body$file_name, "file_name", 120)
  hi <- v_number(body$manual_high, "manual_high", min = 0, min_exclusive = TRUE, required = FALSE)
  lo <- v_number(body$manual_low, "manual_low", min = 0, min_exclusive = TRUE, required = FALSE)
  manual <- NULL
  if (!is.null(hi) || !is.null(lo)) {
    if (is.null(hi) || is.null(lo)) fx_abort("Enter both the highest and the lowest price, or neither.", field = if (is.null(hi)) "manual_high" else "manual_low")
    if (hi <= lo) fx_abort("The highest price must be greater than the lowest price.", field = "manual_high")
    manual <- list(high = hi, low = lo)
  }
  out <- chart_analyze_raw(raw, chart_name, interval, file_name, manual)
  .fx_state$chart <- out
  .fx_state$chart_image <- list(raw = raw, type = if (identical(raw[1:3], as.raw(c(0xff, 0xd8, 0xff)))) "image/jpeg" else "image/png")
  out
}

chart_analyze_raw <- function(raw, chart_name = NULL, interval = NULL, file_name = NULL, manual = NULL) {
  ext <- chart_extract(raw, manual)
  analyse_chart_series(ext, chart_name, interval, file_name)
}

# RStudio console: chart_analyze_file("my_screenshot.png")
chart_analyze_file <- function(path, ...) {
  chart_analyze_raw(readBin(path, "raw", file.size(path)), file_name = basename(path), ...)
}
