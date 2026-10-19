# =============================================================================
# helper_fxlab.R - loaded automatically by testthat before the test files.
# Loads the Chart Reader modules and provides helpers to start the API in a
# background R process and call it over HTTP.
# =============================================================================

local({
  root <- normalizePath(file.path(getwd(), ".."), winslash = "/")
  if (!file.exists(file.path(root, "FXLab.Rproj"))) root <- normalizePath(getwd(), winslash = "/")
  Sys.setenv(FXLAB_ROOT = root)
})
source(file.path(Sys.getenv("FXLAB_ROOT"), "backend", "setup.R"))

# Expect an fxlab_error with a given code (and optionally a message pattern).
expect_fx_error <- function(expr, code, pattern = NULL) {
  err <- tryCatch({ force(expr); NULL }, fxlab_error = function(e) e)
  testthat::expect_s3_class(err, "fxlab_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    if (!is.null(pattern)) testthat::expect_match(conditionMessage(err), pattern)
  }
  invisible(err)
}

# Start backend/main.R in a separate R process on a free port.
start_fxlab_server <- function(root = Sys.getenv("FXLAB_ROOT"), port = httpuv::randomPort(), timeout = 60) {
  log <- tempfile("fxlab-server-", fileext = ".log")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  proc <- processx::process$new(
    rscript, file.path(root, "backend", "main.R"), wd = root,
    env = c("current", FXLAB_PORT = as.character(port), FXLAB_ROOT = root, FXLAB_HOST = "127.0.0.1"),
    stdout = log, stderr = "2>&1", cleanup = TRUE
  )
  url <- sprintf("http://127.0.0.1:%d", port)
  deadline <- Sys.time() + timeout
  repeat {
    up <- tryCatch(curl::curl_fetch_memory(paste0(url, "/health"))$status_code == 200, error = function(e) FALSE)
    if (up) break
    if (!proc$is_alive() || Sys.time() > deadline) {
      proc$kill()
      stop("The Chart Reader server did not start:\n", paste(readLines(log, warn = FALSE), collapse = "\n"))
    }
    Sys.sleep(0.25)
  }
  list(proc = proc, url = url, port = port, log = log)
}

stop_fxlab_server <- function(server) if (!is.null(server) && server$proc$is_alive()) server$proc$kill()

# HTTP call. 'body' is a list (sent as JSON) or 'raw' is sent verbatim.
api_call <- function(server, method, path, body = NULL, raw = NULL, headers = list()) {
  h <- curl::new_handle()
  curl::handle_setopt(h, customrequest = method)
  payload <- if (!is.null(raw)) raw else if (!is.null(body)) {
    jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA)   # full precision, like a browser
  } else NULL
  if (!is.null(payload)) curl::handle_setopt(h, postfields = as.character(payload))
  if (!is.null(payload) && is.null(headers[["Content-Type"]])) headers[["Content-Type"]] <- "application/json"
  if (length(headers)) do.call(curl::handle_setheaders, c(list(h), headers))
  r <- curl::curl_fetch_memory(paste0(server$url, path), handle = h)
  text <- rawToChar(r$content)
  Encoding(text) <- "UTF-8"
  hdr <- curl::parse_headers_list(r$headers)
  json <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  list(status = r$status_code, headers = hdr, json = json, text = text)
}
