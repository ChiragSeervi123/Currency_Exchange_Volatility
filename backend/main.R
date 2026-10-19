# =============================================================================
# main.R  -  START HERE
# -----------------------------------------------------------------------------
# Starts the Chart Reader R backend: the Plumber API and the web interface.
#
#   RStudio : open FXLab.Rproj, open backend/main.R and click "Source".
#   Terminal: from the project folder run
#               "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" backend/main.R
#             (or double-click start_fxlab.bat)
#
# Then open http://127.0.0.1:8000/  (the API also answers at /health, /chart/analyze, ...)
# Stop the server with Esc in RStudio or Ctrl+C in the terminal.
# Change the port with the environment variable FXLAB_PORT (default 8000).
# =============================================================================

# ---- 1. Packages ------------------------------------------------------------------------
required <- c("plumber", "jsonlite", "png", "jpeg", "tesseract")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing R package(s): ", paste(missing_pkgs, collapse = ", "),
       "\nInstall them once with:  install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))",
       "\nor run:  Rscript scripts/install_packages.R", call. = FALSE)
}

# ---- 2. Configuration, statistical modules and data ---------------------------------------
local({
  # Locate backend/setup.R whether this file is sourced in RStudio or run with Rscript.
  candidates <- c(file.path(getwd(), "backend", "setup.R"), file.path(getwd(), "setup.R"))
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(file_arg) == 1) candidates <- c(file.path(dirname(normalizePath(file_arg)), "setup.R"), candidates)
  if (!is.null(sys.frames()) && !is.null(src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL))) {
    candidates <- c(file.path(dirname(normalizePath(src)), "setup.R"), candidates)
  }
  setup <- candidates[file.exists(candidates)][1]
  if (is.na(setup)) stop("Cannot find backend/setup.R. Open FXLab.Rproj or run from the project folder.", call. = FALSE)
  source(setup, local = globalenv())
})

# ---- 3. Build the API --------------------------------------------------------------------------
options(plumber.maxRequestSize = FXLAB_CONFIG$max_request_mb * 1024^2)
fxlab_api <- plumber::pr(file.path(FXLAB_ROOT, "backend", "api.R"))
frontend_dir <- file.path(FXLAB_ROOT, "frontend")
if (isTRUE(FXLAB_CONFIG$serve_frontend) && dir.exists(frontend_dir)) {
  fxlab_api <- plumber::pr_static(fxlab_api, FXLAB_CONFIG$frontend_mount, frontend_dir)
}

app_url <- sprintf("http://%s:%d/", FXLAB_CONFIG$host, FXLAB_CONFIG$port)
message("\n==============================================================")
message(" Chart Reader - ", FXLAB_CONFIG$app_title)
message(" Web interface : ", app_url)
message(" API health    : ", app_url, "health")
message(" API explorer  : ", app_url, "__docs__/")
message(" Data          : USD/INR ", if (dataset_status("usdinr")$available) "loaded" else "NOT AVAILABLE",
        ", dollar index ", if (dataset_status("dollar_index")$available) "loaded" else "NOT AVAILABLE")
message(" Stop          : Esc (RStudio) or Ctrl+C (terminal)")
message("==============================================================\n")

# In RStudio, open the web interface in the browser once the server is listening.
if (interactive()) {
  fxlab_api <- plumber::pr_set_docs_callback(fxlab_api, function(...) utils::browseURL(app_url))
}

# ---- 4. Run (blocks until stopped) -----------------------------------------------------------------
# docs = TRUE also serves an interactive endpoint explorer at /__docs__/.
plumber::pr_run(fxlab_api, host = FXLAB_CONFIG$host, port = FXLAB_CONFIG$port, docs = TRUE, quiet = TRUE)
