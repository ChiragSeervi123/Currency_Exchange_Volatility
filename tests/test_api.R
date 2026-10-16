# API and integration tests: start backend/main.R in a separate R process and call it over HTTP.
skip_if_not_installed("plumber")
skip_if_not_installed("curl")
skip_if_not_installed("processx")

server <- start_fxlab_server()
withr::defer(stop_fxlab_server(server), teardown_env())

fixture_b64 <- function(name) {
  f <- file.path(FXLAB_ROOT, "tests", "fixtures", name)
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
}

test_that("integration: health -> chart upload -> current chart -> image -> reset", {
  h <- api_call(server, "GET", "/health")
  expect_equal(h$status, 200)
  expect_match(h$headers[["content-type"]], "application/json")
  expect_true(h$json$success)
  expect_equal(h$json$data$status, "ok")
  expect_equal(h$json$data$statistical_engine, "R")
  expect_match(h$json$data$ocr_engine, "^tesseract")
  expect_false(h$json$data$chart_loaded)
  expect_null(h$json$error)

  d <- api_call(server, "GET", "/defaults")
  expect_equal(d$status, 200)
  expect_true("1D" %in% unlist(d$json$data$chart_intervals))
  expect_null(d$json$data$simulation)                       # the simulation was removed

  none <- api_call(server, "GET", "/chart/current")
  expect_equal(none$status, 200); expect_null(none$json$data)
  expect_equal(api_call(server, "GET", "/chart/image")$status, 404)

  # Upload a screenshot; R reads it and returns every analysis.
  a <- api_call(server, "POST", "/chart/analyze", list(image_base64 = fixture_b64("nifty_light_candles.png"),
                                                       file_name = "nifty.png"))
  expect_equal(a$status, 200)
  expect_true(a$json$success)
  ad <- a$json$data
  expect_equal(ad$chart$type, "candles")
  expect_equal(ad$chart$n_points, 180)
  expect_equal(ad$chart$file_name, "nifty.png")
  expect_equal(ad$chart$interval, "1D")
  expect_true(all(c("descriptive", "probability", "hypothesis", "regression", "overview") %in% names(ad)))
  expect_true(ad$hypothesis$outlook$verdict %in% c("up", "down", "sideways"))
  # The API returns exactly what the R functions compute.
  f <- file.path(FXLAB_ROOT, "tests", "fixtures", "nifty_light_candles.png")
  direct <- chart_analyze_raw(readBin(f, "raw", file.size(f)))
  expect_equal(ad$descriptive$returns$mean, direct$descriptive$returns$mean, tolerance = 1e-12)
  expect_equal(ad$hypothesis$tests[[1]]$p_value, direct$hypothesis$tests[[1]]$p_value, tolerance = 1e-12)

  # The analysis and the image are kept for the other pages (and after a page refresh).
  cur <- api_call(server, "GET", "/chart/current")
  expect_equal(cur$json$data$chart$n_points, 180)
  img <- curl::curl_fetch_memory(paste0(server$url, "/chart/image"))
  expect_equal(img$status_code, 200)
  expect_identical(img$content, readBin(f, "raw", file.size(f)))
  expect_true(api_call(server, "GET", "/health")$json$data$chart_loaded)

  # Optional settings override what was read from the chart.
  o <- api_call(server, "POST", "/chart/analyze", list(image_base64 = fixture_b64("btc_dark_candles.png"),
                                                       chart_name = "My BTC", interval = "4h"))
  expect_equal(o$json$data$chart$name, "My BTC")
  expect_equal(o$json$data$chart$interval, "4h")
  expect_equal(o$json$data$chart$detected_interval, "1D")

  r <- api_call(server, "POST", "/chart/reset")
  expect_equal(r$status, 200); expect_true(r$json$data$reset)
  expect_null(api_call(server, "GET", "/chart/current")$json$data)
})

test_that("invalid chart uploads are rejected with a clear error naming the field", {
  r <- api_call(server, "POST", "/chart/analyze", list(file_name = "x.png"))
  expect_equal(r$status, 400); expect_equal(r$json$error$field, "image")
  r <- api_call(server, "POST", "/chart/analyze", list(image_base64 = jsonlite::base64_enc(charToRaw("Date,Price\n2025-01-01,86\n"))))
  expect_equal(r$status, 400); expect_equal(r$json$error$code, "INVALID_IMAGE")
  expect_match(r$json$message, "PNG and JPEG")
  r <- api_call(server, "POST", "/chart/analyze", list(image_base64 = fixture_b64("btc_dark_candles.png"), interval = "3D"))
  expect_equal(r$status, 400); expect_equal(r$json$error$field, "interval")
  r <- api_call(server, "POST", "/chart/analyze", list(image_base64 = fixture_b64("btc_dark_candles.png"), manual_high = 10))
  expect_equal(r$status, 400); expect_equal(r$json$error$field, "manual_low")
  r <- api_call(server, "POST", "/chart/analyze", list(image_base64 = fixture_b64("btc_dark_candles.png"), manual_high = 10, manual_low = 20))
  expect_equal(r$status, 400); expect_equal(r$json$error$field, "manual_high")
  r <- api_call(server, "POST", "/chart/analyze", raw = '{"image_base64": "abc",')
  expect_equal(r$status, 400); expect_equal(r$json$error$code, "MALFORMED_JSON")
  expect_false(grepl("Error in|backend/|\\.R:", r$text))   # no R internals or paths leak
  expect_equal(api_call(server, "POST", "/simulate", list(start_rate = 85))$status, 404)   # removed
})

test_that("statistical endpoints return structured results", {
  st <- api_call(server, "POST", "/statistics", list(source = "historical", variable = "log_return",
                                                     start_date = "2016-01-01", end_date = "2025-12-31"))
  expect_equal(st$status, 200)
  expect_equal(st$json$data$statistics$n, 2496)
  expect_equal(st$json$data$statistics$mean, 0.0120162866152738, tolerance = 1e-10)
  expect_gt(length(st$json$data$histogram), 5)

  dist <- api_call(server, "POST", "/distribution", list(distribution = "binomial", params = list(size = 10, prob = 0.5),
                                                         query = list(type = "less_equal", a = 3)))
  expect_equal(dist$status, 200)
  expect_equal(dist$json$data$probability$probability, pbinom(3, 10, 0.5))

  ht <- api_call(server, "POST", "/hypothesis-test", list(test = "normality", source = "custom",
                                                          values = c(1.2, 2.3, 1.9, 2.8, 3.1, 2.2, 1.7, 2.6, 2.4, 2.0)))
  expect_equal(ht$status, 200)
  expect_true(ht$json$data$decision %in% c("Reject H0", "Fail to reject H0"))

  rg <- api_call(server, "POST", "/regression", list(model = "returns_vs_dollar_index"))
  expect_equal(rg$status, 200)
  expect_gt(rg$json$data$n, 2000)

  bad <- api_call(server, "POST", "/regression", list(model = "custom", x = c(1, 1, 1, 1), y = c(1, 2, 3, 4)))
  expect_equal(bad$status, 422); expect_equal(bad$json$error$code, "DEGENERATE_DATA")
  bad <- api_call(server, "POST", "/statistics", list(source = "custom", values = c(1)))
  expect_equal(bad$status, 422); expect_equal(bad$json$error$code, "INSUFFICIENT_DATA")
})

test_that("historical endpoints validate dates and bound their output", {
  s <- api_call(server, "GET", "/historical/summary?start_date=2024-01-01&end_date=2024-12-31")
  expect_equal(s$status, 200)
  first <- as.Date(s$json$data$window$first_observation)        # 1 January has no published rate
  expect_true(first >= as.Date("2024-01-01") && first <= as.Date("2024-01-05"))
  d <- api_call(server, "GET", "/historical/data?max_points=100")
  expect_equal(d$status, 200)
  expect_lte(length(d$json$data$series$date), 100)
  expect_equal(api_call(server, "GET", "/historical/summary?start_date=2024-02-31")$status, 400)
  expect_equal(api_call(server, "GET", "/historical/data?max_points=999999")$status, 400)
  expect_equal(api_call(server, "GET", "/historical/summary?start_date=1950-01-01&end_date=1950-06-01")$status, 422)
})

test_that("CSV upload is validated, used by every analysis, and can be reset", {
  dates <- format(seq(as.Date("2025-01-01"), by = "day", length.out = 40))
  good <- paste(c("Date,USDINR", paste0(dates, ",", 86 + seq_along(dates) / 50)), collapse = "\n")
  bad <- api_call(server, "POST", "/historical/upload", list(csv_text = "Date,Price\n2025-01-01,86", file_name = "x.csv"))
  expect_equal(bad$status, 422); expect_equal(bad$json$error$code, "INVALID_DATA")
  ok <- api_call(server, "POST", "/historical/upload", list(csv_text = good, file_name = "mine.csv"))
  expect_equal(ok$status, 200)
  expect_equal(ok$json$data$observations, 40)
  expect_equal(api_call(server, "GET", "/historical/summary")$json$data$dataset$kind, "uploaded")
  expect_equal(api_call(server, "GET", "/health")$json$data$uploaded_dataset_active, TRUE)
  reset <- api_call(server, "POST", "/historical/reset")
  expect_equal(reset$status, 200)
  expect_true(reset$json$data$reset)
  expect_equal(api_call(server, "GET", "/historical/summary")$json$data$dataset$kind, "bundled")
})

test_that("unknown routes, CORS and the web interface behave correctly", {
  nf <- api_call(server, "GET", "/does-not-exist")
  expect_equal(nf$status, 404); expect_equal(nf$json$error$code, "NOT_FOUND")
  pre <- api_call(server, "OPTIONS", "/chart/analyze", headers = list(Origin = "http://localhost:5500"))
  expect_equal(pre$status, 204)
  expect_equal(pre$headers[["access-control-allow-origin"]], "http://localhost:5500")
  foreign <- api_call(server, "GET", "/health", headers = list(Origin = "http://example.com"))
  expect_null(foreign$headers[["access-control-allow-origin"]])
  page <- api_call(server, "GET", "/app/")
  expect_equal(page$status, 200)
  expect_match(page$text, "<title>FXLab")
  expect_equal(api_call(server, "GET", "/app/js/api.js")$status, 200)
  root <- curl::curl_fetch_memory(paste0(server$url, "/"), handle = curl::new_handle(followlocation = FALSE))
  expect_equal(root$status_code, 302)
})

test_that("without data files the API still runs and reports missing data clearly", {
  tmp <- file.path(tempdir(), paste0("fxlab-nodata-", Sys.getpid()))
  dir.create(file.path(tmp, "backend"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(FXLAB_ROOT, "FXLab.Rproj"), tmp)
  file.copy(list.files(file.path(FXLAB_ROOT, "backend"), full.names = TRUE), file.path(tmp, "backend"))
  empty <- start_fxlab_server(root = normalizePath(tmp, winslash = "/"))
  on.exit({ stop_fxlab_server(empty); unlink(tmp, recursive = TRUE) }, add = TRUE)

  h <- api_call(empty, "GET", "/health")
  expect_equal(h$status, 200)
  expect_false(h$json$data$datasets$usdinr$available)
  expect_match(h$json$data$datasets$usdinr$message, "data/exchange_rates.csv")
  expect_false(grepl(tmp, h$text, fixed = TRUE))          # no server paths in responses
  s <- api_call(empty, "GET", "/historical/summary")
  expect_equal(s$status, 404); expect_equal(s$json$error$code, "DATA_UNAVAILABLE")
  r <- api_call(empty, "POST", "/regression", list(model = "time_trend"))
  expect_equal(r$status, 404)
  ch <- api_call(empty, "POST", "/chart/analyze", list(image_base64 = fixture_b64("btc_dark_candles.png")))
  expect_equal(ch$status, 200)                              # chart analysis needs no data files
})
