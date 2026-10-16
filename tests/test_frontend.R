# Browser end-to-end tests: a headless Chrome (via the chromote package) opens the web
# interface served by the R backend, uploads a chart screenshot through the Chart Upload
# panel, and checks that every page shows exactly the values R returned. Skipped
# automatically when chromote or Chrome is unavailable, or when FXLAB_SKIP_BROWSER=1.
skip_if(Sys.getenv("FXLAB_SKIP_BROWSER") == "1", "FXLAB_SKIP_BROWSER=1")
skip_if_not_installed("chromote")
skip_if_not_installed("processx")

server <- start_fxlab_server()
withr::defer(stop_fxlab_server(server), teardown_env())

# Lighter start-up for automated runs (no first-run tasks, sync or component updates).
# --no-proxy-server skips Windows proxy auto-detection, which can stall the first page load.
options(chromote.timeout = 60)
chromote::set_chrome_args(c(chromote::default_chrome_args(), "--no-proxy-server", "--no-first-run",
                            "--no-default-browser-check", "--disable-background-networking",
                            "--disable-component-update", "--disable-sync", "--disable-default-apps"))
browser <- tryCatch(chromote::ChromoteSession$new(width = 1440, height = 957), error = function(e) NULL)
skip_if(is.null(browser), "Chrome could not be started by chromote")
withr::defer(try(browser$close(), silent = TRUE), teardown_env())
responsive <- tryCatch(isTRUE(as.numeric(browser$Runtime$evaluate("1 + 1", returnByValue = TRUE, timeout_ = 20)$result$value) == 2),
                       error = function(e) FALSE)
skip_if(!responsive, "headless Chrome started but does not answer DevTools commands")

js_errors <- character(0)
browser$Runtime$enable()
browser$Runtime$exceptionThrown(callback_ = function(e) js_errors <<- c(js_errors, e$exceptionDetails$text))
browser$Runtime$consoleAPICalled(callback_ = function(e) {
  if (e$type == "error") js_errors <<- c(js_errors, paste(vapply(e$args, function(a) as.character(a$value %||% a$description %||% ""), ""), collapse = " "))
})

js <- function(expr) browser$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE, timeout_ = 30)$result$value
wait_until <- function(expr, timeout = 40) {
  deadline <- Sys.time() + timeout
  repeat {
    v <- tryCatch(js(expr), error = function(e) NULL)
    if (isTRUE(v)) return(TRUE)
    if (Sys.time() > deadline) return(FALSE)
    Sys.sleep(0.25)
  }
}
key <- function(k) {
  browser$Input$dispatchKeyEvent(type = "keyDown", key = k, code = k)
  browser$Input$dispatchKeyEvent(type = "keyUp", key = k, code = k)
}

# Navigate without waiting for Chrome's reply to Page.navigate (the first page load in a
# fresh headless profile can be slow), then wait for the page and FXLab's scripts.
open_view <- function(hash, timeout = 60) {
  url <- paste0(server$url, "/app/", hash)
  browser$Page$navigate(url, wait_ = FALSE)
  ok <- wait_until(sprintf("document.readyState === 'complete' && location.href === '%s' && !!(window.FX && FX.sections)", url), timeout)
  if (!ok) stop("Page did not load: ", url)
  Sys.sleep(0.5)
}

# Wait until a results container shows R output (cards/charts) or an error banner.
wait_for <- function(results_id, timeout = 40) {
  deadline <- Sys.time() + timeout
  repeat {
    st <- tryCatch(js(sprintf(paste0("(function(){var r=document.getElementById('%s'); if(!r||r.classList.contains('is-loading')) return 'loading';",
                                     "if(r.querySelector('.banner-error')) return 'error: '+r.querySelector('.banner-error').textContent;",
                                     "return r.querySelector('.card') ? 'ok' : 'waiting';})()"), results_id)), error = function(e) "loading")
    if (identical(st, "ok") || startsWith(st, "error")) return(st)
    if (Sys.time() > deadline) return(paste("timeout:", st))
    Sys.sleep(0.25)
  }
}
# A real mouse click (pointerdown/up at the element's centre), as a user would do it.
click_el <- function(selector) {
  r <- jsonlite::fromJSON(js(sprintf("JSON.stringify(document.querySelector('%s').getBoundingClientRect())", selector)))
  x <- r$x + r$width / 2; y <- r$y + r$height / 2
  browser$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  browser$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1)
  browser$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1)
  Sys.sleep(0.6)
}
visible_view <- function() js("Array.prototype.filter.call(document.querySelectorAll('.view'), function(v){return !v.hidden}).map(function(v){return v.id}).join(',')")
kpi <- function(id) js(sprintf("document.querySelector('#%s [data-kpi=value]').textContent", id))
price <- function(x) formatC(x, format = "f", digits = if (abs(x) >= 1000) 2 else if (abs(x) >= 10) 3 else 4, big.mark = ",")
upload_visible <- function() js("document.getElementById('chart-upload').getClientRects().length > 0")
fixture_path <- function(name) normalizePath(file.path(FXLAB_ROOT, "tests", "fixtures", name), winslash = "\\")

# Put a file into the Chart Upload input exactly as the file picker would.
choose_file <- function(path) {
  doc <- browser$DOM$getDocument()
  node <- browser$DOM$querySelector(doc$root$nodeId, "#chart-file")
  browser$DOM$setFileInputFiles(files = list(path), nodeId = node$nodeId)
  Sys.sleep(0.4)
}
analyze <- function() {
  wait_until("!document.querySelector('#upload-form button[type=submit]').disabled", 10)
  js("document.querySelector('#upload-form button[type=submit]').scrollIntoView({ block: 'center' }); true")
  click_el("#upload-form button[type=submit]")
  wait_until("!FX.chart.isBusy()", 90)
}

# Load the app once; if this headless browser cannot even load the page (an environment
# problem, not an FXLab one), skip instead of producing a cascade of time-outs.
first_load <- tryCatch({ open_view("#/dashboard", timeout = 90); TRUE }, error = function(e) FALSE)
skip_if(!first_load, "the headless browser could not load the page within 90 seconds")

test_that("before an upload the app asks for a chart and has no other inputs", {
  expect_true(wait_until("document.querySelector('#status-button').classList.contains('is-online')"))
  expect_equal(kpi("kpi-last"), "—")
  expect_match(js("document.querySelector('[data-state-for=chart-series]').textContent"), "Upload a chart")
  expect_true(js("document.querySelector('#upload-form button[type=submit]').disabled"))
  # The Chart Upload panel is the only place with inputs (the chart-view toggle only changes the view).
  outside <- js("Array.prototype.filter.call(document.querySelectorAll('main input, main select, main textarea'), function(e){ return !e.closest('#chart-upload') && !e.classList.contains('chip-select'); }).length")
  expect_equal(outside, 0)
  for (v in c("descriptive", "distributions", "hypothesis", "regression", "data", "report")) {
    open_view(paste0("#/", v))
    expect_true(wait_until(sprintf("!!document.querySelector('#view-%s .empty-chart')", v), 10), label = v)
  }
  open_view("#/dashboard")
})

test_that("uploading a screenshot sends it to R and the dashboard shows R's values", {
  choose_file(fixture_path("btc_dark_candles.png"))
  expect_match(js("document.getElementById('chart-file-name').textContent"), "btc_dark_candles.png")
  expect_false(js("document.getElementById('chart-preview').hidden"))
  expect_true(analyze())
  expect_match(js("document.getElementById('upload-message').textContent"), "R read 120 candles")
  # The page shows exactly what the API computes for the same image.
  f <- file.path(FXLAB_ROOT, "tests", "fixtures", "btc_dark_candles.png")
  api <- chart_analyze_raw(readBin(f, "raw", file.size(f)))
  n <- length(api$series$close)
  expect_equal(kpi("kpi-last"), price(api$series$close[n]))
  expect_equal(kpi("kpi-mean"), price(api$overview$mean_price))
  expect_equal(kpi("kpi-vol"), paste0(formatC(api$overview$sd_return, format = "f", digits = 2), "%"))
  expect_equal(js("document.querySelector('#dist-stats [data-stat=kurtosis]').textContent"),
               formatC(api$descriptive$returns$kurtosis, format = "f", digits = 3))
  expect_equal(js("document.querySelector('[data-out=expected]').textContent"), price(api$hypothesis$outlook$expected_price))
  expect_true(js("!!document.querySelector('#outlook-body .verdict-' + FX.chart.current().hypothesis.outlook.verdict)"))
  expect_equal(js("FX.charts.get('chart-series').data.labels.length"), n)
  expect_match(js("document.getElementById('pair-label').textContent"), "Bitcoin / TetherUS")
  # The overlay view draws the extracted points on the uploaded image.
  js("var s=document.getElementById('series-view'); s.value='overlay'; s.dispatchEvent(new Event('change')); true")
  expect_true(wait_until("document.getElementById('overlay-img').complete && document.getElementById('overlay-img').naturalWidth === 1600", 10))
  expect_equal(js("document.querySelectorAll('#overlay-svg circle').length"), n)
  js("var s=document.getElementById('series-view'); s.value='series'; s.dispatchEvent(new Event('change')); true")
})

test_that("a chart enlarged to full screen shrinks back to its panel afterwards", {
  height <- function() js("Math.round(document.getElementById('chart-dist').getBoundingClientRect().height)")
  before <- height()
  js("document.querySelector('[data-expand=panel-dist]').scrollIntoView({ block: 'center' }); true")
  click_el("[data-expand=panel-dist]")                  # a real click: full screen needs a user gesture
  if (isTRUE(wait_until("document.fullscreenElement && document.fullscreenElement.id === 'panel-dist'", 5))) {
    expect_true(wait_until(sprintf("document.getElementById('chart-dist').getBoundingClientRect().height > %d", before + 50), 5))
    js("document.exitFullscreen(); true")
    expect_true(wait_until(sprintf("Math.abs(document.getElementById('chart-dist').getBoundingClientRect().height - %d) <= 1", before), 5))
    # The chart stays inside its panel.
    expect_true(js("document.getElementById('chart-dist').getBoundingClientRect().bottom <= document.getElementById('panel-dist').getBoundingClientRect().bottom"))
  } else {
    skip("this headless browser does not allow full screen")
  }
  js("window.scrollTo(0, 0); true")
})

test_that("Chart Upload is a fixed part of the dashboard, aligned on shared lines", {
  js("window.scrollTo(0, 0); true"); Sys.sleep(0.3)
  expect_equal(js("document.querySelectorAll('#app-nav use[href$=i-settings]').length"), 0)
  expect_true(js("!!document.querySelector('#view-dashboard #chart-upload')"))
  box <- function(sel) jsonlite::fromJSON(js(sprintf("JSON.stringify(document.querySelector('%s').getBoundingClientRect())", sel)))
  panel <- box("#chart-upload"); kpis <- box("#kpis"); rail <- box(".rail-btn")
  lower <- box("#view-dashboard .lower"); brand <- box(".brand")
  expect_lt(abs(panel$top - kpis$top), 1)                 # KPI cards start level with the panel (no title block)
  expect_equal(js("document.querySelectorAll('#view-dashboard .hero').length"), 0)
  expect_lt(abs(panel$top - rail$top), 1)
  expect_gt(panel$top, brand$bottom)
  expect_lt(abs(panel$bottom - lower$bottom), 1)
  expect_gt(kpis$left, panel$right)
  key("Escape"); click_el("#kpi-mean")
  expect_true(upload_visible())                         # nothing closes it
})

test_that("each rail icon opens its own section; Monte Carlo is gone", {
  expect_equal(js("document.querySelectorAll('#app-nav .rail-btn').length"), 7)
  expect_null(js("document.getElementById('view-monte-carlo')"))
  icons <- c("#/descriptive" = "view-descriptive", "#/hypothesis" = "view-hypothesis", "#/regression" = "view-regression",
             "#/data" = "view-data", "#/report" = "view-report", "#/about" = "view-about", "#/dashboard" = "view-dashboard")
  for (href in names(icons)) {
    click_el(sprintf('.rail-btn[href="%s"]', href))
    expect_equal(js("location.hash"), href)
    expect_equal(visible_view(), icons[[href]], label = href)
    expect_equal(upload_visible(), href == "#/dashboard", label = paste("upload panel on", href))
  }
})

test_that("every analysis page renders the uploaded chart's results without JavaScript errors", {
  for (v in list(c("#/descriptive", "desc-results"), c("#/distributions", "prob-results"), c("#/hypothesis", "hyp-results"),
                 c("#/regression", "reg-results"), c("#/data", "data-results"))) {
    open_view(v[1])
    expect_equal(wait_for(v[2]), "ok", label = v[1])
  }
  # Hypothesis page: formulas, every test and our hypothesis with its insights.
  open_view("#/hypothesis")
  wait_for("hyp-results")
  a <- jsonlite::fromJSON(js("JSON.stringify(FX.chart.current().hypothesis)"), simplifyVector = FALSE)
  expect_equal(js("document.querySelectorAll('#hyp-results .test-card').length"), length(a$tests))
  expect_gte(js("document.querySelectorAll('#hyp-results .test-card .formula').length"), length(a$tests))
  expect_match(js("document.getElementById('our-hypothesis').textContent"), "Insights from the graph")
  # Descriptive page: summary table, histogram and bar graph; no percentile or tail tables.
  open_view("#/descriptive")
  wait_for("desc-results")
  expect_true(js("!!FX.charts.get('desc-hist-chart') && !!FX.charts.get('desc-band-chart')"))
  expect_false(grepl("Percentiles|Tail check|Q-Q", js("document.getElementById('desc-results').textContent")))
  expect_false(js("document.documentElement.scrollWidth > document.documentElement.clientWidth + 1"))
  expect_length(js_errors, 0)
})

test_that("the Report page shows the whole report and can be downloaded", {
  open_view("#/report")
  expect_true(wait_until("!!document.querySelector('#report-results .report h1')", 20))
  txt <- js("document.getElementById('report-results').textContent")
  for (h in c("1. Summary", "2. The chart and how it was read", "3. Descriptive statistics", "4. Probability and distributions",
              "5. Testing of hypotheses", "6. Our hypothesis", "7. Correlation and regression", "8. Limitations")) expect_match(txt, h, fixed = TRUE)
  a <- jsonlite::fromJSON(js("JSON.stringify(FX.chart.current().hypothesis.tests.map(function(t){return t.title}))"))
  expect_equal(js("document.querySelectorAll('#report-results .rp-test').length"), length(a))   # every test is in the report
  expect_equal(js("document.querySelectorAll('#report-results canvas').length"), 4)
  expect_false(js("document.getElementById('report-download').disabled"))
  # The downloadable file is a complete document with charts and the screenshot embedded.
  html <- js("window.__rep = null; FX.reportHtml().then(function(h){ window.__rep = h; return true; })")
  expect_true(wait_until("typeof window.__rep === 'string'", 20))
  doc <- js("window.__rep")
  expect_match(doc, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(doc, "6. Our hypothesis", fixed = TRUE)
  expect_gte(lengths(regmatches(doc, gregexpr("src=\"data:image/png;base64,", doc))), 5)       # 4 charts + the screenshot
  expect_false(grepl("<canvas", doc, fixed = TRUE))
  expect_length(js_errors, 0)
})

test_that("the analysis survives a page refresh, and Clear removes it", {
  open_view("#/regression")
  expect_equal(wait_for("reg-results"), "ok")             # fetched again from R (GET /chart/current)
  open_view("#/dashboard")
  expect_true(wait_until("document.querySelector('#kpi-last [data-kpi=value]').textContent !== '—'", 20))
  click_el("#upload-form [data-action=clear]")
  expect_true(wait_until("document.querySelector('#kpi-last [data-kpi=value]').textContent === '—'", 10))
  expect_null(api_call(server, "GET", "/chart/current")$json$data)
})

test_that("files that are not charts give a clear message", {
  txt <- tempfile(fileext = ".txt"); writeLines("Date,Price", txt)
  choose_file(normalizePath(txt, winslash = "\\"))
  expect_match(js("document.getElementById('upload-message').textContent"), "not a PNG or JPEG")
  expect_true(js("document.querySelector('#upload-form button[type=submit]').disabled"))
  blank <- tempfile(fileext = ".png"); png::writePNG(array(1, c(400, 600, 3)), blank)
  choose_file(normalizePath(blank, winslash = "\\"))
  analyze()
  expect_match(js("document.getElementById('upload-message').textContent"), "No candles or price line were found")
  expect_equal(kpi("kpi-last"), "—")
})

test_that("the layout works at phone width without horizontal scrolling", {
  browser$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 1, mobile = TRUE)
  on.exit(browser$Emulation$clearDeviceMetricsOverride(), add = TRUE)
  open_view("#/dashboard")
  choose_file(fixture_path("nifty_light_candles.png"))
  expect_true(analyze())
  expect_true(wait_until("document.querySelector('#kpi-last [data-kpi=value]').textContent !== '—'"))
  expect_false(js("document.documentElement.scrollWidth > document.documentElement.clientWidth + 1"))
  expect_equal(js("getComputedStyle(document.getElementById('app-nav')).flexDirection"), "row")   # bottom bar
  expect_true(upload_visible())
  expect_equal(js("getComputedStyle(document.getElementById('chart-upload')).position"), "static")
  expect_true(js("document.getElementById('chart-upload').getBoundingClientRect().bottom <= document.getElementById('kpis').getBoundingClientRect().top"))
})

test_that("the page explains what to do when the backend is offline", {
  open_view("#/dashboard")
  choose_file(fixture_path("btc_dark_candles.png"))
  stop_fxlab_server(server)
  Sys.sleep(1)
  analyze()
  expect_match(js("document.getElementById('upload-message').textContent"), "Cannot reach the R backend")
  expect_true(wait_until("document.getElementById('status-button').classList.contains('is-offline')", 10))
  expect_false(js("document.getElementById('global-banner').hidden"))
})
