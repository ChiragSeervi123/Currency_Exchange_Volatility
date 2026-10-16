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
#* @apiDescription Statistical analysis of a price chart screenshot (and of the bundled USD/INR data). All computations run in R.
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
    uploaded_dataset_active = !is.null(.fx_state$uploaded),
    chart_loaded = !is.null(.fx_state$chart),
    ocr_engine = tryCatch(paste("tesseract", tesseract::tesseract_info()$version), error = function(e) "unavailable")
  ), "FXLab API is running")
}

#* Default parameters and the options the forms offer
#* @get /defaults
function(res) {
  api_run(res, list(
    chart_intervals = FXLAB_CONFIG$chart_intervals,
    limits = FXLAB_CONFIG$limits[c("chart_max_bytes", "custom_max_values", "dist_max_sample", "upload_max_bytes")],
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

#* Analyse a chart screenshot. JSON body: {"image_base64": "...", "file_name": "...", "chart_name": "...",
#* "interval": "1D", "manual_high": 70000, "manual_low": 45000} (only image_base64 is required)
#* @post /chart/analyze
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, chart_analyze_request(parse_json_body(req)), "Chart analysed")
}

#* The analysis of the last uploaded chart (data is null when no chart has been analysed yet)
#* @get /chart/current
function(res) {
  api_run(res, .fx_state$chart, if (is.null(.fx_state$chart)) "No chart has been uploaded yet" else "OK")
}

#* The last uploaded chart image itself
#* @get /chart/image
function(res) {
  img <- .fx_state$chart_image
  if (is.null(img)) return(api_failure(res, 404L, "NOT_FOUND", "No chart has been uploaded yet."))
  res$status <- 200L
  res$setHeader("Content-Type", img$type)
  res$setHeader("Cache-Control", "no-store")
  res$body <- img$raw
  res
}

#* Forget the uploaded chart
#* @post /chart/reset
#* @parser fxraw
#* @parser octet
function(req, res) {
  api_run(res, { had <- !is.null(.fx_state$chart); .fx_state$chart <- NULL; .fx_state$chart_image <- NULL; list(reset = had) },
          "Chart removed")
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
