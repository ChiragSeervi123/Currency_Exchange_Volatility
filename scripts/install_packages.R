# =============================================================================
# install_packages.R - install the R packages Chart Reader uses (run once).
#   "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/install_packages.R
#
# Required to run the app : plumber (brings jsonlite, httpuv, ...), png and jpeg (read screenshots),
#                           tesseract (reads the price-axis labels; includes the English OCR data)
# Required for the tests  : testthat, curl, processx, withr
# Optional                : moments  (extra cross-checks in the tests)
#                           chromote (headless-browser test; needs Google Chrome)
# =============================================================================

repos <- "https://cloud.r-project.org"
required <- c("plumber", "jsonlite", "png", "jpeg", "tesseract")
testing  <- c("testthat", "curl", "processx", "withr")
optional <- c("moments", "chromote")

install_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, repos = repos)
  }
  invisible(missing)
}

install_missing(required)
install_missing(testing)
tryCatch(install_missing(optional), error = function(e) message("Optional packages not installed: ", conditionMessage(e)))

for (p in c(required, testing, optional)) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NOT INSTALLED")
  message(sprintf("  %-9s %s", p, v))
}
