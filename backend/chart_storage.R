# =============================================================================
# chart_storage.R
# -----------------------------------------------------------------------------
# Purpose : A small local library of every chart you have analysed, so you can
#           come back to an earlier screenshot and its full report without
#           uploading it again.
#
# Every successful POST /chart/analyze is saved here automatically. Saved to
# data/charts/ (created on first use, excluded from version control); it
# survives restarting the backend, because it is single-user and local by
# design (see README, "Known limitations"). Oldest entries are dropped once
# FXLAB_CONFIG$limits$chart_store_max is exceeded.
#
# Per entry <id>: <id>.png or <id>.jpg (the screenshot) and <id>.rds (the exact
# object POST /chart/analyze returned - the whole analysis, saved with saveRDS
# so nothing needs re-serialising). data/charts/index.json lists the saved
# entries (id, name, interval, saved_at, ...) for the Storage page; it is the
# source of truth for GET /storage, rebuilt by chart_store_write_index().
# =============================================================================

chart_store_dir <- function() {
  dir <- file.path(FXLAB_ROOT, "data", "charts")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
chart_store_index_path <- function() file.path(chart_store_dir(), "index.json")

chart_store_read_index <- function() {
  p <- chart_store_index_path()
  if (!file.exists(p)) return(list())
  idx <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
  if (!is.list(idx)) list() else idx
}

chart_store_write_index <- function(idx) {
  jsonlite::write_json(idx, chart_store_index_path(), auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
}

# Digits only (timestamp) + a random suffix: safe to use directly in a file path, and the pattern
# is checked again on every request that takes an id (chart_store_valid_id), never trusting the URL.
chart_store_new_id <- function() {
  ts <- gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS3", tz = "UTC"))
  paste0(ts, "-", sprintf("%03d", sample.int(999, 1)))
}
chart_store_valid_id <- function(id) {
  is.character(id) && length(id) == 1 && grepl("^[0-9]{10,25}-[0-9]{1,6}$", id)
}

chart_store_files <- function(dir, id, ext) file.path(dir, paste0(id, c(paste0(".", ext), ".rds")))

# Save one analysis; never lets a storage problem (e.g. a full disk) break the live analyze
# response that is already on its way to the browser.
chart_store_save <- function(raw, content_type, analysis) {
  tryCatch({
    dir <- chart_store_dir()
    id <- chart_store_new_id()
    ext <- if (identical(content_type, "image/jpeg")) "jpg" else "png"
    writeBin(raw, file.path(dir, paste0(id, ".", ext)))
    saveRDS(analysis, file.path(dir, paste0(id, ".rds")))
    entry <- list(
      id = id, saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),   # ISO 8601 UTC: parses reliably in JS
      name = analysis$chart$name, interval = analysis$chart$interval, type = analysis$chart$type,
      n_points = analysis$chart$n_points, file_name = analysis$chart$file_name, image_ext = ext,
      last_price = unname(utils::tail(as.numeric(analysis$series$close), 1)),
      change_pct = analysis$overview$change_pct, verdict = analysis$hypothesis$outlook$verdict,
      strength = analysis$hypothesis$outlook$strength
    )
    idx <- chart_store_read_index()
    idx[[length(idx) + 1]] <- entry
    max_n <- FXLAB_CONFIG$limits$chart_store_max %||% 60
    while (length(idx) > max_n) {
      unlink(chart_store_files(dir, idx[[1]]$id, idx[[1]]$image_ext))
      idx[[1]] <- NULL
    }
    chart_store_write_index(idx)
    entry
  }, error = function(e) {
    message("[FXLab] Could not save the chart to storage: ", conditionMessage(e))
    NULL
  })
}

chart_store_list <- function() rev(chart_store_read_index())    # most recently saved first

chart_store_find <- function(id) {
  for (e in chart_store_read_index()) if (identical(e$id, id)) return(e)
  NULL
}

chart_store_image <- function(id) {
  entry <- chart_store_find(id)
  if (is.null(entry)) fx_abort(sprintf("No saved chart with id '%s'.", id), "NOT_FOUND", 404L)
  path <- file.path(chart_store_dir(), paste0(id, ".", entry$image_ext))
  if (!file.exists(path)) fx_abort("The saved image is missing.", "NOT_FOUND", 404L)
  list(raw = readBin(path, "raw", file.size(path)), type = if (entry$image_ext == "jpg") "image/jpeg" else "image/png")
}

# Makes a saved chart the CURRENT analysis (same effect as POST /chart/analyze) and returns it,
# so the browser can show it exactly as a freshly analysed chart.
chart_store_load <- function(id) {
  entry <- chart_store_find(id)
  if (is.null(entry)) fx_abort(sprintf("No saved chart with id '%s'.", id), "NOT_FOUND", 404L)
  dir <- chart_store_dir()
  rds <- file.path(dir, paste0(id, ".rds"))
  img <- file.path(dir, paste0(id, ".", entry$image_ext))
  if (!file.exists(rds) || !file.exists(img)) fx_abort("The saved files for this chart are missing.", "NOT_FOUND", 404L)
  analysis <- readRDS(rds)
  .fx_state$chart <- analysis
  .fx_state$chart_image <- list(raw = readBin(img, "raw", file.size(img)),
                                type = if (entry$image_ext == "jpg") "image/jpeg" else "image/png")
  analysis
}

chart_store_delete <- function(id) {
  idx <- chart_store_read_index()
  keep <- Filter(function(e) !identical(e$id, id), idx)
  if (length(keep) == length(idx)) return(FALSE)
  removed <- idx[[which(vapply(idx, function(e) identical(e$id, id), logical(1)))]]
  unlink(chart_store_files(chart_store_dir(), id, removed$image_ext))
  chart_store_write_index(keep)
  TRUE
}

chart_store_clear <- function() {
  idx <- chart_store_read_index()
  dir <- chart_store_dir()
  for (e in idx) unlink(chart_store_files(dir, e$id, e$image_ext))
  chart_store_write_index(list())
  length(idx)
}
