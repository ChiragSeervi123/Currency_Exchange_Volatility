# =============================================================================
# run_tests.R - run every automated test (statistics, simulation, data
# validation, hypothesis tests, regression, API, and the browser test).
#
# From the project folder:
#   "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/run_tests.R
# Skip the headless-browser test (needs Chrome + the chromote package):
#   set FXLAB_SKIP_BROWSER=1   (Windows cmd)   then run the command above
# In RStudio: open FXLab.Rproj and run  source("scripts/run_tests.R")
# =============================================================================

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install the test packages first:  Rscript scripts/install_packages.R", call. = FALSE)
}
if (!file.exists("FXLab.Rproj")) stop("Run this script from the FXLab project folder.", call. = FALSE)

results <- testthat::test_dir("tests", reporter = testthat::ProgressReporter$new(show_praise = FALSE),
                              stop_on_failure = FALSE)
df <- as.data.frame(results)
cat(sprintf("\nFiles: %d | Tests: %d | Expectations passed: %d | Failed: %d | Errors: %d (skipped tests are listed above)\n",
            length(unique(df$file)), nrow(df), sum(df$passed), sum(df$failed), sum(df$error)))
if (sum(df$failed) > 0 || sum(df$error) > 0) quit(status = 1)
