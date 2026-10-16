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
  max_request_mb   = 14,            # larger request bodies are rejected (HTTP 413); an 8 MB image is ~11 MB as base64

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

  # ---- Chart screenshots -------------------------------------------------------------
  # Candle intervals the upload panel offers (only used as a label: "per 1D candle").
  chart_intervals = c("1m", "5m", "15m", "30m", "1h", "2h", "4h", "1D", "1W", "1M"),

  # ---- Safety limits -----------------------------------------------------------------
  limits = list(
    chart_max_bytes       = 8 * 1024^2,  # uploaded screenshot (PNG or JPEG)
    chart_store_max       = 60,          # saved charts kept in the Storage library (oldest dropped first)
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
