# API and integration tests: start backend/main.R in a separate R process and call it over HTTP.
skip_if_not_installed("plumber")
skip_if_not_installed("curl")
skip_if_not_installed("processx")

server <- start_fxlab_server()
withr::defer(stop_fxlab_server(server), teardown_env())

test_that("integration: health -> defaults -> simulate -> invalid input", {
  # 1-2. The API is running and reports its components.
  h <- api_call(server, "GET", "/health")
  expect_equal(h$status, 200)
  expect_match(h$headers[["content-type"]], "application/json")
  expect_true(h$json$success)
  expect_equal(h$json$data$status, "ok")
  expect_equal(h$json$data$statistical_engine, "R")
  expect_true(h$json$data$datasets$usdinr$available)
  expect_null(h$json$error)

  # 3. Default parameters, estimated from the data.
  d <- api_call(server, "GET", "/defaults")
  expect_equal(d$status, 200)
  def <- d$json$data$simulation
  expect_equal(def$source, "historical")
  expect_setequal(vapply(d$json$data$simulation_models, `[[`, "", "id"), c("gbm_normal", "gbm_student_t", "gbm_bootstrap"))

  # 4-5. Simulation with the defaults; check structure and values.
  body <- list(start_rate = def$start_rate, mu_daily_pct = def$mu_daily_pct, sigma_daily_pct = def$sigma_daily_pct,
               days = 50, n_paths = 400, seed = 42, threshold = def$threshold, n_display_paths = 7)
  s <- api_call(server, "POST", "/simulate", body)
  expect_equal(s$status, 200)
  expect_true(s$json$success)
  sd <- s$json$data
  expect_equal(sd$seed, 42)
  expect_length(sd$paths$values, 7)
  expect_length(sd$paths$values[[1]], 51)
  expect_equal(sd$paths$values[[1]][[1]], def$start_rate)
  expect_true(all(unlist(sd$paths$values) > 0))
  expect_equal(sd$terminal$n_paths, 400)
  # The API returns exactly what the R function computes.
  direct <- do.call(simulate_exchange_rate, validate_simulation_input(body))
  expect_equal(sd$terminal$mean, direct$terminal$mean, tolerance = 1e-12)
  expect_equal(sd$threshold$prob_above, direct$threshold$prob_above)
  # Same seed, same answer.
  s2 <- api_call(server, "POST", "/simulate", body)
  expect_identical(s2$json$data$terminal, sd$terminal)

  # 6-7. Invalid input gives a structured 400 naming the field.
  bad <- api_call(server, "POST", "/simulate", modifyList(body, list(sigma_daily_pct = -1)))
  expect_equal(bad$status, 400)
  expect_false(bad$json$success)
  expect_null(bad$json$data)
  expect_equal(bad$json$error$code, "VALIDATION_ERROR")
  expect_equal(bad$json$error$field, "sigma_daily_pct")
  expect_match(bad$json$message, "sigma_daily_pct")
})

test_that("malformed and oversized simulation requests are rejected", {
  r <- api_call(server, "POST", "/simulate", raw = '{"start_rate": 85,')
  expect_equal(r$status, 400); expect_equal(r$json$error$code, "MALFORMED_JSON")
  r <- api_call(server, "POST", "/simulate", raw = "[1, 2, 3]")
  expect_equal(r$status, 400); expect_equal(r$json$error$code, "MALFORMED_JSON")
  r <- api_call(server, "POST", "/simulate", list(start_rate = 85, mu_daily_pct = 0, sigma_daily_pct = 0.3,
                                                  days = 1000, n_paths = 10000))
  expect_equal(r$status, 400); expect_match(r$json$message, "exceeds the limit")
  r <- api_call(server, "POST", "/simulate", list(start_rate = "eighty", mu_daily_pct = 0, sigma_daily_pct = 0.3,
                                                  days = 10, n_paths = 10))
  expect_equal(r$status, 400); expect_equal(r$json$error$field, "start_rate")
  expect_false(grepl("Error in|backend/|\\.R:", r$text))   # no R internals or paths leak
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
  pre <- api_call(server, "OPTIONS", "/simulate", headers = list(Origin = "http://localhost:5500"))
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
  d <- api_call(empty, "GET", "/defaults")
  expect_equal(d$json$data$simulation$source, "example")    # labelled as example inputs, not market data
  sim <- api_call(empty, "POST", "/simulate", list(start_rate = 85, mu_daily_pct = 0, sigma_daily_pct = 0.3,
                                                   days = 10, n_paths = 10, seed = 1))
  expect_equal(sim$status, 200)                             # simulation needs no data
  boot <- api_call(empty, "POST", "/simulate", list(start_rate = 85, mu_daily_pct = 0, sigma_daily_pct = 0.3,
                                                    days = 10, n_paths = 10, model = "gbm_bootstrap"))
  expect_equal(boot$status, 404)                            # bootstrap needs historical returns
})
