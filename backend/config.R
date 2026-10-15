# =============================================================================
# config.R
# -----------------------------------------------------------------------------
# Purpose : Every setting FXLab uses lives here: server address, data files,
#           statistical conventions, and the safety limits that stop a request
#           from exhausting memory. Change a value here, not inside the modules.
# Note    : Server address and port can also be set with the environment
#           variables FXLAB_HOST and FXLAB_PORT (used by the API tests).
# =============================================================================

FXLAB_CONFIG <- list(
  app_name    = "FXLab",
  app_title   = "Currency Exchange Rate and Volatility Analysis Using R",
  api_version = "1.0.0",

  # ---- Server ---------------------------------------------------------------
  # 127.0.0.1 keeps the API private to this computer.
  host = Sys.getenv("FXLAB_HOST", "127.0.0.1"),
  port = as.integer(Sys.getenv("FXLAB_PORT", "8000")),
  serve_frontend   = TRUE,          # serve frontend/ at /app/ from the same R process
  frontend_mount   = "/app",
  max_request_mb   = 6,             # larger request bodies are rejected (HTTP 413)

  # Browsers may call the API from these origins (cross-origin requests).
  # Pages served by the API itself are same-origin and need no CORS.
  # Allowed: http://localhost:<any port> and http://127.0.0.1:<any port>.
  cors_origin_pattern = "^http://(localhost|127\\.0\\.0\\.1)(:[0-9]{1,5})?$",

  # ---- Data -------------------------------------------------------------------
  # Paths are relative to the project root.
  data_files = list(
    usdinr       = file.path("data", "exchange_rates.csv"),
    dollar_index = file.path("data", "dollar_index.csv")
  ),
  data_sources = list(
    usdinr = list(
      name   = "USD/INR spot exchange rate (Indian rupees per 1 U.S. dollar), noon buying rates in New York",
      source = "FRED, Federal Reserve Bank of St. Louis, series DEXINUS (original source: Board of Governors of the Federal Reserve System, H.10 release)",
      url    = "https://fred.stlouisfed.org/series/DEXINUS",
      unit   = "INR per USD"
    ),
    dollar_index = list(
      name   = "Nominal Broad U.S. Dollar Index (January 2006 = 100)",
      source = "FRED, Federal Reserve Bank of St. Louis, series DTWEXBGS (original source: Board of Governors of the Federal Reserve System, H.10 release)",
      url    = "https://fred.stlouisfed.org/series/DTWEXBGS",
      unit   = "Index, Jan 2006 = 100"
    )
  ),
  # Default analysis window. The end date defaults to the last observation.
  default_start_date = as.Date("2016-01-01"),

  # ---- Statistical conventions ---------------------------------------------------
  trading_days_per_year = 252,       # annualised volatility = daily SD x sqrt(252)
  rolling_window        = 20,        # trading days (about one month)
  quantile_type         = 7,         # R's default quantile definition
  default_alpha         = 0.05,
  allowed_alpha         = c(0.01, 0.05, 0.10),
  ljung_box_default_lag = 10,

  # ---- Simulation defaults (used only when no historical data is available) -------
  # These are illustrative example inputs, NOT market data.
  fallback_simulation = list(start_rate = 85, mu_daily_pct = 0.01, sigma_daily_pct = 0.30),
  default_simulation  = list(days = 180, n_paths = 500, n_display_paths = 20, seed = 2026,
                             model = "gbm_normal", t_df = 5),
  simulation_models = list(
    list(id = "gbm_normal", label = "Normal (Geometric Brownian Motion)",
         description = "Classic GBM: daily log-returns are normal with mean (mu - sigma^2/2) and SD sigma."),
    list(id = "gbm_student_t", label = "Student-t (heavy-tailed GBM)",
         description = "Same drift and volatility, but shocks follow a Student-t distribution rescaled to variance 1, producing more extreme days."),
    list(id = "gbm_bootstrap", label = "Historical bootstrap (GBM)",
         description = "Shocks are resampled from standardised historical USD/INR log-returns, then scaled to the chosen drift and volatility.")
  ),

  # ---- Safety limits -----------------------------------------------------------------
  limits = list(
    sim_max_days          = 1000,      # about four trading years
    sim_max_paths         = 10000,
    sim_max_cells         = 5e6,       # days x paths
    sim_max_display_paths = 500,
    sim_max_display_points = 100000,   # displayed paths x (days + 1) values returned for charts
    sim_min_t_df          = 3,         # the variance of a t distribution needs df > 2
    sim_max_t_df          = 100,
    max_daily_sigma_pct   = 20,
    max_abs_daily_mu_pct  = 5,
    custom_max_values     = 20000,
    custom_min_values     = 2,
    dist_max_sample       = 100000,
    dist_max_binom_size   = 100000,
    dist_max_poisson_lambda = 1e6,
    historical_max_points = 3000,      # points returned for charts (the analysis uses all data)
    chart_max_points      = 1500,      # scatter / residual plots
    upload_max_rows       = 100000,
    upload_max_bytes      = 5 * 1024^2,
    min_valid_observations = 10
  )
)
