# =============================================================================
# setup.R
# -----------------------------------------------------------------------------
# Purpose : Find the project folder, load the configuration and every
#           statistical module, and read the bundled datasets. Used by
#           backend/main.R (the API), by the tests, and in RStudio when you
#           want to call the functions directly without starting the server:
#
#               source("backend/setup.R")
#               historical_summary()$volatility
#               chart_analyze_file("tests/fixtures/btc_dark_candles.png")$hypothesis$outlook
# =============================================================================

# Project root = the folder that contains FXLab.Rproj (search upwards from the
# working directory and from the running script's location).
find_fxlab_root <- function() {
  env_root <- Sys.getenv("FXLAB_ROOT", "")
  if (nzchar(env_root) && file.exists(file.path(env_root, "backend", "setup.R"))) {
    return(normalizePath(env_root, winslash = "/"))
  }
  starts <- getwd()
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(file_arg) == 1) starts <- c(starts, dirname(normalizePath(file_arg, mustWork = FALSE)))
  for (s in starts) {
    dir <- normalizePath(s, winslash = "/", mustWork = FALSE)
    for (i in 1:5) {
      if (file.exists(file.path(dir, "FXLab.Rproj")) && file.exists(file.path(dir, "backend", "setup.R"))) {
        return(dir)
      }
      parent <- dirname(dir)
      if (identical(parent, dir)) break
      dir <- parent
    }
  }
  stop("Could not find the Chart Reader project folder. Open FXLab.Rproj in RStudio, or run the command ",
       "from the project folder (the one that contains FXLab.Rproj).", call. = FALSE)
}

FXLAB_ROOT <- find_fxlab_root()

local({
  modules <- c("config.R", "validation.R", "statistics.R", "historical_analysis.R", "distributions.R",
               "hypothesis_testing.R", "regression_analysis.R", "chart_image.R", "chart_storage.R", "chart_analysis.R")
  for (m in modules) source(file.path(FXLAB_ROOT, "backend", m), local = globalenv())
})

load_datasets(FXLAB_ROOT)
