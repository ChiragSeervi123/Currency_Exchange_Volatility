# Browser end-to-end tests: a headless Chrome (via the chromote package) opens the web
# interface served by the R backend, drives the dashboard and its Simulation Parameters
# panel, and checks that the page shows exactly the values R returned. Skipped
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
rupee <- function(x) paste0("₹", formatC(x, format = "f", digits = 2, big.mark = ","))
params_visible <- function() js("document.getElementById('sim-params').getClientRects().length > 0")

# Load the app once; if this headless browser cannot even load the page (an environment
# problem, not an FXLab one), skip instead of producing a cascade of time-outs.
first_load <- tryCatch({ open_view("#/dashboard", timeout = 90); TRUE }, error = function(e) FALSE)
skip_if(!first_load, "the headless browser could not load the page within 90 seconds")

test_that("the dashboard runs the default simulation in R and shows R's values", {
  expect_true(wait_until("document.querySelector('#kpi-mean [data-kpi=value]').textContent !== '—'"))
  expect_true(wait_until("document.querySelector('#status-button').classList.contains('is-online')"))
  # KPI 1 is the last observation in the dataset (not a live quote).
  expect_equal(kpi("kpi-rate"), rupee(historical_summary()$latest$rate))
  # KPIs 2-4 and the side statistics equal the API response for the very same request.
  body <- js("JSON.stringify(FX.simParams.requestBody())")
  api <- api_call(server, "POST", "/simulate", raw = body)$json$data
  expect_equal(kpi("kpi-mean"), rupee(api$terminal$mean))
  expect_equal(kpi("kpi-vol"), paste0(formatC(api$daily_returns$sd_pct, format = "f", digits = 2), "%"))
  expect_equal(kpi("kpi-return"), paste0(if (api$terminal$mean_return_pct >= 0) "+" else "",
                                         formatC(api$terminal$mean_return_pct, format = "f", digits = 2), "%"))
  expect_equal(js("document.querySelector('[data-mc=mean]').textContent"), rupee(api$terminal$mean))
  expect_equal(js("document.querySelector('[data-mc=low]').textContent"), rupee(api$terminal$min))
  expect_equal(js("document.querySelector('#dist-stats [data-stat=kurtosis]').textContent"),
               formatC(api$daily_returns$kurtosis, format = "f", digits = 3))
  # Charts drawn from the response; "All Paths" shows every returned path plus the mean path.
  expect_equal(js("FX.charts.get('chart-paths').data.datasets.length"), length(api$paths$values) + 1)
  expect_true(js("!!FX.charts.get('chart-dist') && !!FX.charts.get('chart-mc')"))
  expect_equal(js("document.getElementById('paths-select').value"), as.character(length(api$paths$values)))
})

test_that("Simulation Parameters is a fixed part of the dashboard, not a pop-up", {
  # No gear in the navigation rail, no toggle, no close button.
  expect_equal(js("document.querySelectorAll('#app-nav use[href$=i-settings]').length"), 0)
  expect_equal(js("document.querySelectorAll('#app-nav .rail-btn').length"), 7)
  expect_null(js("document.getElementById('drawer-toggle')"))
  expect_equal(js("document.querySelectorAll('#sim-params [data-action=close-drawer], #sim-params [aria-label*=Close]').length"), 0)
  # It belongs to the Exchange Rate Dashboard page and is shown there.
  expect_true(js("!!document.querySelector('#view-dashboard #sim-params')"))
  expect_true(params_visible())
  where <- function() js("JSON.stringify(document.getElementById('sim-params').getBoundingClientRect())")
  before <- where()
  key("Escape")
  click_el(".hero-title")
  expect_true(params_visible())                        # nothing closes it
  expect_equal(where(), before)                        # and nothing moves it
})

test_that("desktop dashboard is aligned on shared lines", {
  box <- function(sel) jsonlite::fromJSON(js(sprintf("JSON.stringify(document.querySelector('%s').getBoundingClientRect())", sel)))
  params <- box("#sim-params"); eyebrow <- box("#view-dashboard .eyebrow"); rail <- box(".rail-btn")
  lower <- box("#view-dashboard .lower"); brand <- box(".brand")
  # One top line: first rail icon, Simulation Parameters panel and "Explore · Analyze · Simulate".
  expect_lt(abs(params$top - eyebrow$top), 1)
  expect_lt(abs(params$top - rail$top), 1)
  expect_gt(params$top, brand$bottom)                  # clear of the FXLab brand block
  # One bottom line: the panel ends with the Distribution / Monte Carlo panels, without inner scrolling.
  expect_lt(abs(params$bottom - lower$bottom), 1)
  expect_lte(js("(function(p){ return p.scrollHeight - p.clientHeight; })(document.getElementById('sim-params'))"), 0)
  # The title block sits right of the panel; its subtitle is a single line.
  expect_gt(eyebrow$left, params$right)
  expect_equal(js("(function(e){ return Math.round(e.getBoundingClientRect().height / parseFloat(getComputedStyle(e).lineHeight)); })(document.querySelector('.hero-sub'))"), 1)
})

test_that("each rail icon opens its own section; Simulation Parameters appears only on the dashboard", {
  icons <- c("#/descriptive" = "view-descriptive", "#/monte-carlo" = "view-monte-carlo", "#/hypothesis" = "view-hypothesis",
             "#/regression" = "view-regression", "#/historical" = "view-historical", "#/about" = "view-about",
             "#/dashboard" = "view-dashboard")
  for (href in names(icons)) {
    click_el(sprintf('.rail-btn[href="%s"]', href))
    expect_equal(js("location.hash"), href)
    expect_equal(visible_view(), icons[[href]], label = href)
    expect_equal(js("document.querySelector('.rail-btn[aria-current=page]').getAttribute('href')"), href)
    expect_equal(params_visible(), href == "#/dashboard", label = paste("parameters panel on", href))
  }
  # The dashboard panel is the only simulation-parameter form in the app.
  expect_equal(js("document.querySelectorAll('[name=sigma_daily_pct], [name=mu_daily_pct], [name=start_rate]').length"), 3)
  expect_equal(js("document.querySelectorAll('#sim-params [name=sigma_daily_pct]').length"), 1)
})

test_that("Run Simulation sends the panel's parameters to R and updates the dashboard", {
  js("var f=document.getElementById('params-form'); f.querySelector('[name=start_rate]').value='83';
      f.querySelector('[name=mu_daily_pct]').value='0'; f.querySelector('[name=sigma_daily_pct]').value='1';
      f.querySelector('[name=period]').value='6'; f.querySelector('[name=period_unit]').value='21'; true")
  js("document.querySelector('#params-form button[type=submit]').click(); true")
  expect_true(wait_until("/completed/.test(document.getElementById('params-message').textContent)"))
  body <- jsonlite::fromJSON(js("JSON.stringify(FX.simParams.requestBody())"))
  expect_equal(body$days, 126)                         # 6 months x 21 trading days
  api <- api_call(server, "POST", "/simulate", raw = jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA))$json$data
  expect_equal(kpi("kpi-mean"), rupee(api$terminal$mean))
  expect_equal(kpi("kpi-vol"), paste0(formatC(api$daily_returns$sd_pct, format = "f", digits = 2), "%"))
  expect_equal(js("FX.charts.get('chart-paths').scales.x.max"), 126)
  js("document.querySelector('#params-form [data-action=reset]').click(); true")
  expect_equal(js("document.querySelector('#params-form [name=period]').value"), "180")
})

test_that("invalid parameters show validation messages instead of results", {
  js("var f=document.getElementById('params-form'); f.querySelector('[name=n_paths]').value='0'; f.querySelector('button[type=submit]').click(); true")
  expect_match(js("(document.querySelector('#params-form .field-error')||{}).textContent||''"), "at least 1")
  # A value only the server can reject (days x paths limit) is reported by R next to the field.
  js("var f=document.getElementById('params-form'); f.querySelector('[name=n_paths]').value='10000';
      f.querySelector('[name=period]').value='3'; f.querySelector('[name=period_unit]').value='252';
      f.querySelector('button[type=submit]').click(); true")
  expect_true(wait_until("/exceeds the limit/.test(document.getElementById('params-message').textContent)"))
  js("document.querySelector('#params-form [data-action=reset]').click(); true")
})

test_that("the Monte Carlo page analyses the current simulation without its own parameter form", {
  click_el('.rail-btn[href="#/monte-carlo"]')
  expect_false(params_visible())                       # the parameters panel belongs to the dashboard only
  expect_equal(js("Array.prototype.map.call(document.querySelectorAll('#view-monte-carlo [name]'), function(e){return e.name}).join(',')"),
               "paths_drawn,threshold")
  expect_true(wait_until("!!FX.charts.get('mc-fan-chart')"))
  sim <- jsonlite::fromJSON(js("JSON.stringify(FX.simulation.current().inputs)"))
  seed <- js("FX.simulation.current().seed")
  body <- list(start_rate = sim$start_rate, mu_daily_pct = sim$mu_daily_pct, sigma_daily_pct = sim$sigma_daily_pct,
               days = sim$days, n_paths = sim$n_paths, model = sim$model, seed = seed, n_display_paths = sim$n_display_paths)
  api <- api_call(server, "POST", "/simulate", body)$json$data
  card <- function(start) js(sprintf("(function(){var cs=document.querySelectorAll('#mc-results .card');for(var i=0;i<cs.length;i++){if(cs[i].querySelector('.card-label').textContent.indexOf('%s')===0) return cs[i].querySelector('.card-value').textContent;} return null;})()", start))
  expect_equal(card("Expected final rate"), formatC(api$terminal$mean, format = "f", digits = 4, big.mark = ","))
  expect_match(js("document.getElementById('mc-params').textContent"), paste0("seed ", seed))
  # "Paths drawn" only changes how many of the returned paths are drawn.
  n_drawn <- function() js("FX.charts.get('mc-fan-chart').data.datasets.filter(function(d){return /Sample path/.test(d.label)}).length")
  js("var s=document.getElementById('mc-paths-drawn'); s.value='50'; s.dispatchEvent(new Event('change')); true")
  expect_true(wait_until("FX.charts.get('mc-fan-chart').data.datasets.filter(function(d){return /Sample path/.test(d.label)}).length === 50", 10))
  # Threshold probabilities are computed by R for the same simulation (same inputs and seed).
  js("document.getElementById('mc-threshold').value='90'; document.querySelector('#mc-form button[type=submit]').click(); true")
  expect_equal(wait_for("mc-results"), "ok")
  expect_true(wait_until("document.getElementById('mc-results').textContent.indexOf('P(S_T > 90.00)') >= 0"))
  api_t <- api_call(server, "POST", "/simulate", c(body, list(threshold = 90)))$json$data
  expect_equal(card("P(S_T > 90.00)"), paste0(formatC(100 * api_t$threshold$prob_above, format = "f", digits = 2), "%"))
  expect_equal(card("Expected final rate"), formatC(api$terminal$mean, format = "f", digits = 4, big.mark = ","))
  expect_equal(js("FX.simulation.current().seed"), seed)
  expect_equal(js("document.querySelectorAll('#mc-results canvas').length"), 3)   # charts replaced, not stacked
  # A threshold is required for this action.
  js("document.getElementById('mc-threshold').value=''; document.querySelector('#mc-form button[type=submit]').click(); true")
  expect_match(js("(document.querySelector('#mc-form .field-error')||{}).textContent||''"), "required")
})

test_that("every analysis page renders results from R without JavaScript errors", {
  for (v in list(c("#/historical", "hist-results"), c("#/descriptive", "desc-results"),
                 c("#/distributions", "dist-results"), c("#/hypothesis", "hyp-results"),
                 c("#/regression", "reg-results"))) {
    open_view(v[1])
    expect_equal(wait_for(v[2]), "ok", label = v[1])
  }
  expect_false(params_visible())
  expect_false(js("document.documentElement.scrollWidth > document.documentElement.clientWidth + 1"))
  expect_length(js_errors, 0)
})

test_that("the layout works at phone width without horizontal scrolling", {
  browser$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 1, mobile = TRUE)
  on.exit(browser$Emulation$clearDeviceMetricsOverride(), add = TRUE)
  open_view("#/dashboard")
  expect_true(wait_until("document.querySelector('#kpi-mean [data-kpi=value]').textContent !== '—'"))
  expect_false(js("document.documentElement.scrollWidth > document.documentElement.clientWidth + 1"))
  expect_equal(js("getComputedStyle(document.getElementById('app-nav')).flexDirection"), "row")   # bottom bar
  # The parameters panel is part of the page flow, under the title and above the KPI cards.
  expect_true(params_visible())
  expect_equal(js("getComputedStyle(document.getElementById('sim-params')).position"), "static")
  expect_true(js("document.getElementById('sim-params').getBoundingClientRect().bottom <= document.getElementById('kpis').getBoundingClientRect().top"))
  expect_true(js("document.getElementById('sim-params').getBoundingClientRect().right <= document.documentElement.clientWidth"))
})

test_that("the page explains what to do when the backend is offline", {
  open_view("#/descriptive")
  expect_equal(wait_for("desc-results"), "ok")
  stop_fxlab_server(server)
  Sys.sleep(1)
  js("document.querySelector('#desc-form button[type=submit]').click()")
  msg <- wait_for("desc-results")
  expect_match(msg, "Cannot reach the R backend")
  expect_true(wait_until("document.getElementById('status-button').classList.contains('is-offline')", 10))
  expect_false(js("document.getElementById('global-banner').hidden"))
})
