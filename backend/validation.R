# =============================================================================
# validation.R
# -----------------------------------------------------------------------------
# Purpose : Input checking shared by every module and endpoint.
#           A failed check raises an "fxlab_error" condition that carries an
#           HTTP status, a machine-readable code and the offending field, so the
#           API can return a clear message (and never a stack trace).
# Codes   : VALIDATION_ERROR (400)   - a request value is missing or invalid
#           MALFORMED_JSON (400)     - the request body is not valid JSON
#           DATA_UNAVAILABLE (404)   - a required dataset file is missing
#           INVALID_DATA (422)       - a dataset/CSV fails its quality checks
#           INSUFFICIENT_DATA (422)  - too few observations for a method
#           DEGENERATE_DATA (422)    - e.g. zero variance, so a statistic is undefined
# =============================================================================

# ---- Error condition --------------------------------------------------------------
fx_abort <- function(message, code = "VALIDATION_ERROR", status = 400L, field = NULL) {
  cond <- structure(
    class = c("fxlab_error", "error", "condition"),
    list(message = message, call = NULL, code = code, status = as.integer(status), field = field)
  )
  stop(cond)
}

fx_insufficient <- function(message) fx_abort(message, "INSUFFICIENT_DATA", 422L)
fx_degenerate   <- function(message) fx_abort(message, "DEGENERATE_DATA", 422L)

`%||%` <- function(a, b) if (is.null(a)) b else a

# A value counts as "not supplied" when it is NULL, an empty string or a JSON null.
is_missing_value <- function(x) {
  is.null(x) || length(x) == 0 ||
    (length(x) == 1 && (is.na(x) || (is.character(x) && !nzchar(trimws(x)))))
}

# ---- Scalar validators -------------------------------------------------------------
# Each returns the cleaned value or raises an fxlab_error naming the field.

v_number <- function(x, field, min = -Inf, max = Inf, default = NULL, required = TRUE,
                     min_exclusive = FALSE, max_exclusive = FALSE, allow_string = FALSE) {
  if (is_missing_value(x)) {
    if (!is.null(default)) return(default)
    if (!required) return(NULL)
    fx_abort(sprintf("'%s' is required.", field), field = field)
  }
  # Query-string values arrive as text; JSON bodies must use real numbers.
  if (allow_string && is.character(x) && length(x) == 1) {
    num <- suppressWarnings(as.numeric(x))
    if (is.na(num)) fx_abort(sprintf("'%s' must be a number (received '%s').", field, x), field = field)
    x <- num
  }
  if (!is.numeric(x) || length(x) != 1) {
    fx_abort(sprintf("'%s' must be a single number.", field), field = field)
  }
  if (!is.finite(x)) fx_abort(sprintf("'%s' must be a finite number.", field), field = field)
  below <- if (min_exclusive) x <= min else x < min
  above <- if (max_exclusive) x >= max else x > max
  if (below || above) {
    lo <- if (is.finite(min)) paste0(if (min_exclusive) "greater than " else "at least ", format(min)) else NULL
    hi <- if (is.finite(max)) paste0(if (max_exclusive) "less than " else "at most ", format(max)) else NULL
    fx_abort(sprintf("'%s' must be %s (received %s).", field,
                     paste(c(lo, hi), collapse = " and "), format(x)), field = field)
  }
  as.numeric(x)
}

v_integer <- function(x, field, min = -Inf, max = Inf, default = NULL, required = TRUE,
                      allow_string = FALSE) {
  val <- v_number(x, field, min = min, max = max, default = default, required = required,
                  allow_string = allow_string)
  if (is.null(val)) return(NULL)
  if (abs(val - round(val)) > 1e-9) fx_abort(sprintf("'%s' must be a whole number.", field), field = field)
  as.integer(round(val))
}

v_choice <- function(x, field, choices, default = NULL) {
  if (is_missing_value(x)) {
    if (!is.null(default)) return(default)
    fx_abort(sprintf("'%s' is required. Allowed values: %s.", field, paste(choices, collapse = ", ")),
             field = field)
  }
  if (!is.character(x) || length(x) != 1 || !(x %in% choices)) {
    fx_abort(sprintf("'%s' must be one of: %s.", field, paste(choices, collapse = ", ")), field = field)
  }
  x
}

v_bool <- function(x, field, default = FALSE) {
  if (is_missing_value(x)) return(default)
  if (is.logical(x) && length(x) == 1) return(isTRUE(x))
  if (is.character(x) && tolower(x) %in% c("true", "false")) return(tolower(x) == "true")
  fx_abort(sprintf("'%s' must be true or false.", field), field = field)
}

# Dates must be ISO 8601 (YYYY-MM-DD) and real calendar dates.
v_date <- function(x, field, required = FALSE) {
  if (is_missing_value(x)) {
    if (required) fx_abort(sprintf("'%s' is required (format YYYY-MM-DD).", field), field = field)
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1 || !grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    fx_abort(sprintf("'%s' must be a date in the format YYYY-MM-DD.", field), field = field)
  }
  d <- as.Date(x, format = "%Y-%m-%d")
  if (is.na(d) || format(d) != x) fx_abort(sprintf("'%s' is not a valid calendar date: %s.", field, x),
                                           field = field)
  d
}

v_alpha <- function(x, field = "alpha") {
  a <- v_number(x, field, default = FXLAB_CONFIG$default_alpha, min = 0.001, max = 0.2)
  a
}

v_seed <- function(x, field = "seed") {
  v_integer(x, field, min = 1, max = 2147483647, required = FALSE)
}

# ---- Numeric samples supplied by the user -------------------------------------------
# Accepts a JSON array of numbers, or text with numbers separated by commas,
# spaces, semicolons or new lines. Missing entries (null, NA, blank) are removed
# and COUNTED so the response can report them; any other non-numeric entry is
# rejected, because silently dropping it could hide a typing mistake.
parse_numeric_input <- function(x, field, min_n = FXLAB_CONFIG$limits$custom_min_values,
                                max_n = FXLAB_CONFIG$limits$custom_max_values) {
  if (is_missing_value(x)) fx_abort(sprintf("'%s' is required: supply a list of numbers.", field),
                                    field = field)
  if (is.character(x) && length(x) == 1) {
    tokens <- trimws(unlist(strsplit(x, "[,;[:space:]]+")))
    tokens <- tokens[nzchar(tokens)]
  } else if (is.numeric(x) || is.logical(x) || is.character(x)) {
    tokens <- as.character(x)
  } else {
    fx_abort(sprintf("'%s' must be an array of numbers or text containing numbers.", field), field = field)
  }
  if (length(tokens) > max_n) {
    fx_abort(sprintf("'%s' has %d values; the maximum is %d.", field, length(tokens), max_n), field = field)
  }
  missing <- is.na(tokens) | toupper(tokens) %in% c("NA", "NULL", "")
  values  <- suppressWarnings(as.numeric(tokens[!missing]))
  bad     <- tokens[!missing][is.na(values) | !is.finite(values)]
  if (length(bad) > 0) {
    shown <- paste0("'", utils::head(bad, 5), "'", collapse = ", ")
    fx_abort(sprintf("'%s' contains %d entr%s that %s not a finite number: %s%s.", field, length(bad),
                     if (length(bad) == 1) "y" else "ies", if (length(bad) == 1) "is" else "are", shown,
                     if (length(bad) > 5) ", ..." else ""), field = field)
  }
  if (length(values) < min_n) {
    fx_abort(sprintf("'%s' needs at least %d numeric values (received %d).", field, min_n, length(values)),
             code = "INSUFFICIENT_DATA", status = 422L, field = field)
  }
  list(values = values, n_supplied = length(tokens), n_missing_removed = sum(missing))
}

# Stop with a clear message if a sample is too small for a method.
require_n <- function(x, min_n, what) {
  n <- sum(is.finite(x))
  if (n < min_n) {
    fx_insufficient(sprintf("%s needs at least %d valid observations; only %d are available.", what, min_n, n))
  }
  invisible(n)
}

# Stop if every value is (numerically) identical, which makes SD-based statistics undefined.
require_variation <- function(x, what) {
  x <- x[is.finite(x)]
  if (length(x) < 2 || stats::sd(x) <= 1e-12 * max(1, abs(mean(x)))) {
    fx_degenerate(sprintf("%s is undefined because all values are identical (zero variance).", what))
  }
  invisible(TRUE)
}

# ---- Request body parsing ------------------------------------------------------------------
# The API reads the raw body itself so that malformed JSON gets a clear 400 response.
parse_json_body <- function(req) {
  raw <- req$bodyRaw
  txt <- if (is.null(raw) || length(raw) == 0) "" else rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!nzchar(trimws(txt))) return(list())
  body <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(e) fx_abort("The request body is not valid JSON.", code = "MALFORMED_JSON")
  )
  if (!is.list(body) || (length(body) > 0 && is.null(names(body)))) {
    fx_abort("The request body must be a JSON object, for example {\"start_rate\": 85}.",
             code = "MALFORMED_JSON")
  }
  body
}
