# The Storage library (backend/chart_storage.R): every chart analysed with chart_analyze_request()
# is saved automatically, so it can be listed, previewed, reloaded as the current analysis, deleted,
# or cleared, without re-uploading it.
#
# These tests use the real data/charts/ folder (a local, disposable cache - see its header comment
# and README "Known limitations"; it is excluded from version control). Each test clears it first,
# so tests never depend on charts saved by a previous run or by using the app yourself.
skip_if_not_installed("png")
skip_if_not_installed("tesseract")

fixture_b64 <- function(name) {
  f <- file.path(FXLAB_ROOT, "tests", "fixtures", name)
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
}
clear_store <- function() unlink(chart_store_dir(), recursive = TRUE)
clear_store()
withr::defer(clear_store(), teardown_env())

test_that("analysing a chart saves it to storage automatically", {
  clear_store()
  expect_equal(chart_store_list(), list())
  a <- chart_analyze_request(list(image_base64 = fixture_b64("btc_dark_candles.png"), file_name = "btc.png"))
  lst <- chart_store_list()
  expect_length(lst, 1)
  e <- lst[[1]]
  expect_true(chart_store_valid_id(e$id))
  expect_equal(e$name, a$chart$name)
  expect_equal(e$interval, a$chart$interval)
  expect_equal(e$type, "candles")
  expect_equal(e$n_points, a$chart$n_points)
  expect_equal(e$file_name, "btc.png")
  expect_equal(e$last_price, unname(utils::tail(as.numeric(a$series$close), 1)))
  expect_equal(e$change_pct, a$overview$change_pct)
  expect_equal(e$verdict, a$hypothesis$outlook$verdict)
  expect_match(e$saved_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")        # ISO 8601 UTC
})

test_that("the library lists newest first and each saved image matches the upload", {
  clear_store()
  raw1 <- readBin(file.path(FXLAB_ROOT, "tests", "fixtures", "btc_dark_candles.png"), "raw", file.size(file.path(FXLAB_ROOT, "tests", "fixtures", "btc_dark_candles.png")))
  raw2 <- readBin(file.path(FXLAB_ROOT, "tests", "fixtures", "nifty_light_candles.png"), "raw", file.size(file.path(FXLAB_ROOT, "tests", "fixtures", "nifty_light_candles.png")))
  chart_analyze_request(list(image_base64 = paste0("data:image/png;base64,", jsonlite::base64_enc(raw1))))
  Sys.sleep(1.01)   # make sure the two ids' timestamps differ, so string order matches save order
  chart_analyze_request(list(image_base64 = paste0("data:image/png;base64,", jsonlite::base64_enc(raw2))))
  lst <- chart_store_list()
  expect_length(lst, 2)
  expect_true(lst[[1]]$id > lst[[2]]$id)                       # newest (nifty) listed first
  expect_equal(lst[[1]]$name, "NIFTY 50 · 1D · NSE")
  img1 <- chart_store_image(lst[[2]]$id)
  expect_identical(img1$raw, raw1)
  expect_equal(img1$type, "image/png")
})

test_that("a saved chart can be reloaded as the current analysis", {
  clear_store()
  .fx_state$chart <- NULL; .fx_state$chart_image <- NULL
  a <- chart_analyze_request(list(image_base64 = fixture_b64("btc_dark_candles.png")))
  id <- chart_store_list()[[1]]$id
  .fx_state$chart <- NULL; .fx_state$chart_image <- NULL      # simulate a fresh server / cleared current chart
  reloaded <- chart_store_load(id)
  expect_equal(reloaded, a)
  expect_equal(.fx_state$chart, a)
  expect_equal(.fx_state$chart_image$type, "image/png")
  expect_fx_error(chart_store_load("99999999999999-999"), "NOT_FOUND")
})

test_that("deleting and clearing remove the saved files, not just the index entry", {
  clear_store()
  chart_analyze_request(list(image_base64 = fixture_b64("btc_dark_candles.png")))
  chart_analyze_request(list(image_base64 = fixture_b64("nifty_light_candles.png")))
  id <- chart_store_list()[[1]]$id
  expect_gt(length(list.files(chart_store_dir())), 1)
  expect_true(chart_store_delete(id))
  expect_false(chart_store_delete(id))                          # already gone
  expect_length(chart_store_list(), 1)
  expect_false(any(grepl(id, list.files(chart_store_dir()), fixed = TRUE)))   # its files are gone too
  n <- chart_store_clear()
  expect_equal(n, 1)
  expect_equal(chart_store_list(), list())
  expect_equal(list.files(chart_store_dir()), "index.json")       # only the (now empty) index remains
})

test_that("the oldest entries are dropped once the library exceeds its configured size", {
  clear_store()
  old_config <- FXLAB_CONFIG
  on.exit(assign("FXLAB_CONFIG", old_config, envir = globalenv()), add = TRUE)
  patched <- FXLAB_CONFIG; patched$limits$chart_store_max <- 2
  assign("FXLAB_CONFIG", patched, envir = globalenv())
  for (i in 1:4) chart_analyze_request(list(image_base64 = fixture_b64("btc_dark_candles.png"), file_name = paste0("x", i, ".png")))
  lst <- chart_store_list()
  expect_length(lst, 2)
  expect_setequal(vapply(lst, `[[`, "", "file_name"), c("x3.png", "x4.png"))      # the two most recent survive
  expect_equal(length(list.files(chart_store_dir())), 2 * 2 + 1)                  # 2 charts x (.png + .rds) + index.json
})

test_that("only a well-formed id is accepted, closing off path traversal", {
  expect_false(chart_store_valid_id("../../etc/passwd"))
  expect_false(chart_store_valid_id("abc"))
  expect_false(chart_store_valid_id(""))
  expect_false(chart_store_valid_id(c("20261016065123118-328", "x")))
  expect_true(chart_store_valid_id("20261016065123118-328"))
})
