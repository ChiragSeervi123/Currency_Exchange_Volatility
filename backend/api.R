# =============================================================================
# api.R  -  FXLab HTTP API (R Plumber)
# -----------------------------------------------------------------------------
# This file only routes requests: it reads and validates the request, calls a
# statistical function from the other backend modules, and wraps the result.
# All calculations happen in those modules (sourced by backend/setup.R).
#
# Every response is JSON with the same shape:
#   success: { "success": true,  "data": {...}, "message": "...", "error": null }
#   failure: { "success": false, "data": null,  "message": "...",
#              "error": { "code": "VALIDATION_ERROR", "field": "days", "status": 400 } }
# Status codes: 200 OK, 400 invalid request, 404 data/route not found,
#               413 too large, 422 data cannot support the analysis, 500 unexpected.
# Full request/response documentation: README.md, section "API reference".
#
# Start the API with backend/main.R (not by running this file directly).
#
# POST endpoints use the pass-through parser "fxraw" (registered below) so that
# plumber leaves the JSON body untouched and parse_json_body() (validation.R)
# can answer malformed JSON with a clear 400. (plumber's built-in "none" parser
# fails with the current stringi version; "octet" covers other content types.)
# =============================================================================

plumber::register_parser("fxraw", function(...) function(value, ...) value,
                         fixed = c("application/json", "text/json"), verbose = FALSE)

#* @apiTitle FXLab API
#* @apiDescription Statistical analysis of the USD/INR exchange rate. All computations run in R.
#* @apiVersion 1.0.0

# ---- Response helpers -------------------------------------------------------------------------
api_success <- function(data, message = "OK") {
  list(success = TRUE, data = data, message = message, error = NULL)
}

api_failure <- function(res, status, code, message, field = NULL) {
  res$status <- as.integer(status)
  list(success = FALSE, data = NULL, message = message,
       error = list(code = code, field = field, status = as.integer(status)))
}

# Evaluate 'expr' (lazily, inside the handlers below). Validation and data errors
# become 4xx responses with their message; anything unexpected becomes a generic
# 500 response while the details are printed in the R console only.
api_run <- function(res, expr, message = "OK") {
  r_warnings <- character(0)
  outcome <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      r_warnings <<- c(r_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    fxlab_error = function(e) e,
    error = function(e) e
  )
  if (inherits(outcome, "fxlab_error")) {
    return(api_failure(res, outcome$status, outcome$code, conditionMessage(outcome), outcome$field))
  }
  if (inherits(outcome, "error")) {
    message("[FXLab] Unexpected error: ", conditionMessage(outcome))
    return(api_failure(res, 500L, "INTERNAL_ERROR",
                       "An unexpected error occurred while processing the request. Details are in the R console."))
  }
  if (length(r_warnings) > 0) {
    message("[FXLab] R warnings: ", paste(unique(r_warnings), collapse = " | "))
    if (is.list(outcome)) outcome$r_warnings <- I(unique(r_warnings))
  }
  api_success(outcome, message)
}

query_date <- function(x, field) v_date(if (is.null(x)) NULL else as.character(x), field)

# ---- Router configuration ------------------------------------------------------------------------
#* @plumber
function(pr) {
  pr |>
    plumber::pr_set_serializer(plumber::serializer_unboxed_json(na = "null", null = "null", digits = NA)) |>
    plumber::pr_set_error(function(req, res, err) {
      message("[FXLab] Unhandled error on ", req$PATH_INFO, ": ", conditionMessage(err))
      api_failure(res, 500L, "INTERNAL_ERROR", "An unexpected error occurred. Details are in the R console.")
    }) |>
    plumber::pr_set_404(function(req, res) {
      api_failure(res, 404L, "NOT_FOUND", sprintf("No endpoint matches %s %s.", req$REQUEST_METHOD, req$PATH_INFO))
    }) |>
    plumber::pr_hook("preroute", function(data, req, res) {
      req$fxlab_start <- Sys.time()
      invisible(NULL)
    }) |>
    plumber::pr_hook("postserialize", function(data, req, res, value) {
      if (!startsWith(req$PATH_INFO, FXLAB_CONFIG$frontend_mount)) {
        ms <- if (is.null(req$fxlab_start)) NA else round(1000 * as.numeric(difftime(Sys.time(), req$fxlab_start, units = "secs")))
        message(sprintf("[FXLab] %s %s %s -> %s (%s ms)", format(Sys.time(), "%H:%M:%S"), req$REQUEST_METHOD,
                        req$PATH_INFO, res$status, ms))
      }
      value
    })
}

# ---- CORS ------------------------------------------------------------------------------------------
# Allow pages served from http://localhost:<port> or http://127.0.0.1:<port> (e.g. an
# editor's live-preview server) to call the API. Other origins get no CORS headers,
# so browsers block them. Same-origin requests (frontend served at /app/) need none.
#* @filter cors
function(req, res) {
  origin <- req$HTTP_ORIGIN
  if (!is.null(origin) && grepl(FXLAB_CONFIG$cors_origin_pattern, origin)) {
    res$setHeader("Access-Control-Allow-Origin", origin)
    res$setHeader("Vary", "Origin")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type")
    res$setHeader("Access-Control-Max-Age", "600")
  }
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204L
    res$body <- ""
    return(res)
  }
  plumber::forward()
}

# ---- Frontend redirect -------------------------------------------------------------------------------
#* Redirect to the web interface
#* @get /
#* @serializer html
function(res) {
  res$status <- 302L
  res$setHeader("Location", paste0(FXLAB_CONFIG$frontend_mount, "/"))
  "<a href='/app/'>FXLab</a>"
}

#* @get /app
#* @serializer html
function(res) {
  res$status <- 301L
  res$setHeader("Location", paste0(FXLAB_CONFIG$frontend_mount, "/"))
  "<a href='/app/'>FXLab</a>"
}

# ---- Endpoints -----------------------------------------------------------------------------------------

#* API status, R version and dataset availability
#* @get /health
function(res) {
  api_run(res, list(
    status = "ok",
    app = FXLAB_CONFIG$app_name,
    api_version = FXLAB_CONFIG$api_version,
    r_version = R.version.string,
    plumber_version = as.character(utils::packageVersion("plumber")),
    server_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    statistical_engine = "R",
    datasets = list(usdinr = dataset_status("usdinr"), dollar_index = dataset_status("dollar_index")),
    uploaded_dataset_active = !is.null(.fx_state$uploaded)
  ), "FXLab API is running")
}

#* Default parameters and the options the forms offer
#* @get /defaults
function(res) {
  api_run(res, list(
    simulation = simulation_defaults(),
    simulation_models = FXLAB_CONFIG$simulation_models,
    limits = FXLAB_CONFIG$limits[c("sim_max_days", "sim_max_paths", "sim_max_cells", "sim_max_display_paths", "sim_max_display_points",
                                   "sim_min_t_df", "sim_max_t_df", "max_daily_sigma_pct", "max_abs_daily_mu_pct",
                                   "custom_max_values", "dist_max_sample", "upload_max_bytes")],
    distributions = distribution_catalogue(),
    hypothesis_tests = TEST_TYPES,
    regression_models = c("returns_vs_dollar_index", "time_trend", "custom"),
    alpha_choices = FXLAB_CONFIG$allowed_alpha,
    default_alpha = FXLAB_CONFIG$default_alpha,
    default_start_date = format(FXLAB_CONFIG$default_start_date),
    trading_days_per_year = FXLAB_CONFIG$trading_days_per_year,
    rolling_window = FXLAB_CONFIG$rolling_window
  ))
}

#* Simulate USD/INR paths (GBM) and summarise them by Monte Carlo
#* @post /simulate
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, {
    inp <- validate_simulation_input(parse_json_body(req))
    do.call(simulate_exchange_rate, inp)
  }, "Simulation completed")
}

#* Probability distribution: PDF/PMF, CDF, quantiles, probabilities and a random sample
#* @post /distribution
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, {
    b <- parse_json_body(req)
    distribution_analysis(b$distribution, b$params, b$query, b$quantile_p, b$sample_size, v_seed(b$seed))
  }, "Distribution calculated")
}

#* Descriptive statistics for historical data or an entered sample
#* @post /statistics
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, statistics_request(parse_json_body(req)), "Statistics calculated")
}

#* Historical summary for a date window (?start_date=YYYY-MM-DD&end_date=YYYY-MM-DD)
#* @get /historical/summary
function(req, res, start_date = "", end_date = "") {
  api_run(res, historical_summary(query_date(start_date, "start_date"), query_date(end_date, "end_date")))
}

#* Historical series for charts (?start_date&end_date&max_points)
#* @get /historical/data
function(req, res, start_date = "", end_date = "", max_points = "") {
  api_run(res, {
    mp <- v_integer(max_points, "max_points", min = 50, max = FXLAB_CONFIG$limits$historical_max_points,
                    default = FXLAB_CONFIG$limits$historical_max_points, allow_string = TRUE)
    historical_data(query_date(start_date, "start_date"), query_date(end_date, "end_date"), mp)
  })
}

#* Upload a USD/INR CSV (JSON body: {"csv_text": "...", "file_name": "..."})
#* @post /historical/upload
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, {
    b <- parse_json_body(req)
    upload_usdinr_csv(b$csv_text, b$file_name)
  }, "Dataset uploaded and validated. All analyses now use it.")
}

#* Return to the bundled FRED dataset
#* @post /historical/reset
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, {
    had <- reset_uploaded_dataset()
    list(reset = had, active = dataset_status("usdinr"))
  }, "Using the bundled FRED dataset.")
}

#* Hypothesis test or confidence interval
#* @post /hypothesis-test
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, run_hypothesis_test(parse_json_body(req)), "Test completed")
}

#* Correlation and simple linear regression
#* @post /regression
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, run_regression(parse_json_body(req)), "Regression completed")
}
