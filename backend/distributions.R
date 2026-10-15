# =============================================================================
# distributions.R
# -----------------------------------------------------------------------------
# Purpose : Theoretical probability distributions for the Probability
#           Distribution Analysis section: density / mass functions, CDFs,
#           quantiles, probabilities, moments and random samples. R's own
#           d/p/q/r functions do the work (dnorm, pbinom, qpois, rexp, ...).
#
# Supported   Parameters                    Mean        Variance
#   normal      mean, sd > 0                  mean        sd^2
#   uniform     min < max                     (a+b)/2     (b-a)^2 / 12
#   exponential rate > 0                      1/rate      1/rate^2
#   binomial    size n (integer), prob p      n p         n p (1-p)
#   poisson     lambda > 0                    lambda      lambda
# =============================================================================

DISTRIBUTIONS <- list(
  normal = list(
    label = "Normal", discrete = FALSE,
    params = list(mean = list(label = "Mean (mu)", default = 0),
                  sd = list(label = "Standard deviation (sigma)", default = 1, min = 0, min_exclusive = TRUE)),
    d = function(x, p) stats::dnorm(x, p$mean, p$sd),
    p = function(q, p, lower = TRUE) stats::pnorm(q, p$mean, p$sd, lower.tail = lower),
    q = function(pr, p) stats::qnorm(pr, p$mean, p$sd),
    r = function(n, p) stats::rnorm(n, p$mean, p$sd),
    moments = function(p) list(mean = p$mean, variance = p$sd^2, skewness = 0, excess_kurtosis = 0,
                               median = p$mean, mode = p$mean),
    range = function(p) c(p$mean - 4 * p$sd, p$mean + 4 * p$sd),
    formula = "f(x) = 1 / (sigma * sqrt(2*pi)) * exp(-(x - mu)^2 / (2 * sigma^2))"
  ),
  uniform = list(
    label = "Uniform (continuous)", discrete = FALSE,
    params = list(min = list(label = "Lower bound (a)", default = 0),
                  max = list(label = "Upper bound (b)", default = 1)),
    d = function(x, p) stats::dunif(x, p$min, p$max),
    p = function(q, p, lower = TRUE) stats::punif(q, p$min, p$max, lower.tail = lower),
    q = function(pr, p) stats::qunif(pr, p$min, p$max),
    r = function(n, p) stats::runif(n, p$min, p$max),
    moments = function(p) list(mean = (p$min + p$max) / 2, variance = (p$max - p$min)^2 / 12,
                               skewness = 0, excess_kurtosis = -6 / 5,
                               median = (p$min + p$max) / 2, mode = NA_real_),
    range = function(p) { w <- p$max - p$min; c(p$min - 0.15 * w, p$max + 0.15 * w) },
    formula = "f(x) = 1 / (b - a) for a <= x <= b, otherwise 0"
  ),
  exponential = list(
    label = "Exponential", discrete = FALSE,
    params = list(rate = list(label = "Rate (lambda)", default = 1, min = 0, min_exclusive = TRUE)),
    d = function(x, p) stats::dexp(x, p$rate),
    p = function(q, p, lower = TRUE) stats::pexp(q, p$rate, lower.tail = lower),
    q = function(pr, p) stats::qexp(pr, p$rate),
    r = function(n, p) stats::rexp(n, p$rate),
    moments = function(p) list(mean = 1 / p$rate, variance = 1 / p$rate^2, skewness = 2, excess_kurtosis = 6,
                               median = log(2) / p$rate, mode = 0),
    range = function(p) c(0, stats::qexp(0.999, p$rate)),
    formula = "f(x) = lambda * exp(-lambda * x) for x >= 0"
  ),
  binomial = list(
    label = "Binomial", discrete = TRUE,
    params = list(size = list(label = "Number of trials (n)", default = 20, min = 1,
                              max = FXLAB_CONFIG$limits$dist_max_binom_size, integer = TRUE),
                  prob = list(label = "Success probability (p)", default = 0.5, min = 0, max = 1)),
    d = function(x, p) stats::dbinom(x, p$size, p$prob),
    p = function(q, p, lower = TRUE) stats::pbinom(q, p$size, p$prob, lower.tail = lower),
    q = function(pr, p) stats::qbinom(pr, p$size, p$prob),
    r = function(n, p) stats::rbinom(n, p$size, p$prob),
    moments = function(p) {
      v <- p$size * p$prob * (1 - p$prob)
      list(mean = p$size * p$prob, variance = v,
           skewness = if (v > 0) (1 - 2 * p$prob) / sqrt(v) else NA_real_,
           excess_kurtosis = if (v > 0) (1 - 6 * p$prob * (1 - p$prob)) / v else NA_real_,
           median = stats::qbinom(0.5, p$size, p$prob),
           mode = min(p$size, floor((p$size + 1) * p$prob)))
    },
    range = function(p) c(0, p$size),
    formula = "P(X = k) = C(n, k) * p^k * (1 - p)^(n - k), k = 0, 1, ..., n"
  ),
  poisson = list(
    label = "Poisson", discrete = TRUE,
    params = list(lambda = list(label = "Mean rate (lambda)", default = 4, min = 0, min_exclusive = TRUE,
                                max = FXLAB_CONFIG$limits$dist_max_poisson_lambda)),
    d = function(x, p) stats::dpois(x, p$lambda),
    p = function(q, p, lower = TRUE) stats::ppois(q, p$lambda, lower.tail = lower),
    q = function(pr, p) stats::qpois(pr, p$lambda),
    r = function(n, p) stats::rpois(n, p$lambda),
    moments = function(p) list(mean = p$lambda, variance = p$lambda, skewness = 1 / sqrt(p$lambda),
                               excess_kurtosis = 1 / p$lambda, median = stats::qpois(0.5, p$lambda),
                               mode = if (p$lambda == floor(p$lambda)) p$lambda else floor(p$lambda)),
    range = function(p) c(0, max(stats::qpois(0.9999, p$lambda), 5)),
    formula = "P(X = k) = lambda^k * exp(-lambda) / k!, k = 0, 1, 2, ..."
  )
)

# Validate distribution parameters supplied by the user.
validate_distribution_params <- function(dist, params) {
  spec <- DISTRIBUTIONS[[dist]]
  params <- params %||% list()
  if (!is.list(params)) fx_abort("'params' must be an object, for example {\"mean\": 0, \"sd\": 1}.",
                                 field = "params")
  out <- list()
  for (nm in names(spec$params)) {
    ps <- spec$params[[nm]]
    field <- paste0("params.", nm)
    out[[nm]] <- if (isTRUE(ps$integer)) {
      v_integer(params[[nm]], field, min = ps$min %||% -Inf, max = ps$max %||% Inf, default = ps$default)
    } else {
      v_number(params[[nm]], field, min = ps$min %||% -Inf, max = ps$max %||% Inf, default = ps$default,
               min_exclusive = isTRUE(ps$min_exclusive))
    }
  }
  if (dist == "uniform" && out$min >= out$max) {
    fx_abort("For the uniform distribution the lower bound 'min' must be less than 'max'.", field = "params.max")
  }
  if (dist == "normal" && out$sd > 1e6) fx_abort("'params.sd' is too large (maximum 1e6).", field = "params.sd")
  out
}

# Points for the PDF/PMF and CDF chart.
distribution_curve <- function(spec, p) {
  if (!spec$discrete) {
    rg <- spec$range(p)
    x  <- seq(rg[1], rg[2], length.out = 241)
    return(list(x = I(x), density = I(spec$d(x, p)), cdf = I(spec$p(x, p)), thinned = FALSE))
  }
  rg <- spec$range(p)
  lo <- max(rg[1], spec$q(0.0001, p))
  hi <- min(rg[2], spec$q(0.9999, p))
  if (hi - lo < 10) { lo <- max(rg[1], lo - 5); hi <- min(rg[2], max(hi, lo + 10)) }
  k <- seq(lo, hi)
  thinned <- FALSE
  if (length(k) > 600) {                        # very wide supports are thinned for display
    k <- unique(round(seq(lo, hi, length.out = 600)))
    thinned <- TRUE
  }
  list(x = I(k), density = I(spec$d(k, p)), cdf = I(spec$p(k, p)), thinned = thinned)
}

# P(X <= a), P(X > a) or P(a <= X <= b). Discrete cases use exact integer bounds.
probability_query <- function(spec, p, query) {
  if (is.null(query) || length(query) == 0) return(NULL)
  if (!is.list(query)) fx_abort("'query' must be an object such as {\"type\": \"less_equal\", \"a\": 1}.",
                                field = "query")
  type <- v_choice(query$type, "query.type", c("less_equal", "greater", "between"), default = "less_equal")
  a <- v_number(query$a, "query.a")
  fmt <- function(v) format(signif(v, 6))
  if (type == "between") {
    b <- v_number(query$b, "query.b")
    if (b < a) fx_abort("For 'between', 'query.b' must be greater than or equal to 'query.a'.", field = "query.b")
    prob <- if (spec$discrete) {
      if (floor(b) < ceiling(a)) 0 else spec$p(floor(b), p) - spec$p(ceiling(a) - 1, p)
    } else spec$p(b, p) - spec$p(a, p)
    return(list(type = type, a = a, b = b, probability = max(0, prob),
                expression = sprintf("P(%s <= X <= %s)", fmt(a), fmt(b))))
  }
  q <- if (spec$discrete) floor(a) else a
  prob <- if (type == "less_equal") spec$p(q, p) else spec$p(q, p, lower = FALSE)
  out <- list(type = type, a = a, b = NULL, probability = prob,
              expression = sprintf(if (type == "less_equal") "P(X <= %s)" else "P(X > %s)", fmt(a)))
  if (spec$discrete && a == floor(a)) out$point_probability <- spec$d(a, p)
  out
}

# Random sample and its comparison with theory (Law of Large Numbers in action).
distribution_sample <- function(spec, p, n, seed, theory) {
  seed_used <- seed %||% sample.int(1e9, 1)
  set.seed(seed_used, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  x <- spec$r(n, p)
  summary <- list(
    n = n, seed = seed_used, seed_source = if (is.null(seed)) "generated" else "user",
    mean = mean(x), variance = if (n > 1) stats::var(x) else NA_real_,
    sd = if (n > 1) stats::sd(x) else NA_real_, min = min(x), max = max(x),
    skewness = skewness_g1(x),
    standard_error_of_mean = if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
  )
  summary$mean_minus_theory <- summary$mean - theory$mean
  if (spec$discrete) {
    lo <- min(x); hi <- max(x)
    k <- seq(lo, hi)
    if (length(k) > 600) {
      h <- histogram_bins(x, n_bins = 50)
      h$expected_count <- n * (spec$p(floor(h$upper), p) - spec$p(ceiling(h$lower) - 1, p))
      return(list(summary = summary, histogram = h, discrete_frequencies = NULL))
    }
    obs <- tabulate(x - lo + 1, nbins = length(k))
    freq <- data.frame(value = k, observed_count = obs, observed_share = obs / n,
                       expected_count = n * spec$d(k, p), expected_share = spec$d(k, p))
    return(list(summary = summary, histogram = NULL, discrete_frequencies = freq))
  }
  h <- histogram_bins(x, max_bins = 50)
  h$expected_count <- n * (spec$p(h$upper, p) - spec$p(h$lower, p))
  h$theoretical_density <- spec$d(h$mid, p)
  list(summary = summary, histogram = h, discrete_frequencies = NULL)
}

# Entry point used by POST /distribution.
distribution_analysis <- function(distribution, params = list(), query = NULL, quantile_p = NULL,
                                  sample_size = NULL, seed = NULL) {
  distribution <- v_choice(distribution, "distribution", names(DISTRIBUTIONS))
  spec <- DISTRIBUTIONS[[distribution]]
  p <- validate_distribution_params(distribution, params)
  theory <- spec$moments(p)
  theory$sd <- sqrt(theory$variance)

  probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
  out <- list(
    distribution = distribution, label = spec$label, discrete = spec$discrete,
    parameters = p, formula = spec$formula,
    theoretical = theory,
    curve = distribution_curve(spec, p),
    quantiles = data.frame(probability = probs, value = spec$q(probs, p)),
    probability = probability_query(spec, p, query)
  )
  if (!is.null(quantile_p)) {
    pr <- v_number(quantile_p, "quantile_p", min = 0, max = 1, min_exclusive = TRUE, max_exclusive = TRUE)
    out$quantile_query <- list(p = pr, value = spec$q(pr, p),
                               expression = sprintf("Smallest x with P(X <= x) >= %s", format(pr)))
  }
  if (!is.null(sample_size)) {
    n <- v_integer(sample_size, "sample_size", min = 1, max = FXLAB_CONFIG$limits$dist_max_sample)
    out$sample <- distribution_sample(spec, p, n, seed, theory)
  }
  out
}

# Description of every distribution, for the frontend form (GET /defaults).
distribution_catalogue <- function() {
  lapply(names(DISTRIBUTIONS), function(nm) {
    s <- DISTRIBUTIONS[[nm]]
    list(id = nm, label = s$label, discrete = s$discrete, formula = s$formula,
         params = lapply(names(s$params), function(pn) {
           ps <- s$params[[pn]]
           list(name = pn, label = ps$label, default = ps$default, min = ps$min, max = ps$max,
                integer = isTRUE(ps$integer))
         }))
  })
}
