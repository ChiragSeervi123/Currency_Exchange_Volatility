# =============================================================================
# make_chart_fixtures.R - build the test screenshots in tests/fixtures/ (developer tool).
#
# Each fixture is a chart drawn by TradingView's open-source charting library
# "Lightweight Charts" (Apache-2.0) in headless Chrome, styled like a TradingView
# screenshot (price axis on the right, dates below, coloured last-price line, volume
# bars, a legend with the symbol and OHLC values). The prices are SYNTHETIC: a seeded
# random walk, saved next to the image as <name>.csv so the tests can measure how
# accurately backend/chart_image.R reads the chart back.
#
#   "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/make_chart_fixtures.R <path to lightweight-charts.js>
#
# The library file: https://unpkg.com/lightweight-charts@4.2.0/dist/lightweight-charts.standalone.production.js
# Needs the chromote package and Google Chrome.
# =============================================================================

args <- commandArgs(TRUE)
lib_path <- args[1]
if (is.na(lib_path) || !file.exists(lib_path)) stop("Pass the path of lightweight-charts.standalone.production.js")
root <- normalizePath(".", winslash = "/")
out_dir <- file.path(root, "tests", "fixtures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Seeded random walk of OHLC candles (log returns ~ N(drift, vol)).
make_ohlc <- function(n, start, drift, vol, seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  close <- start * exp(cumsum(stats::rnorm(n, drift, vol)))
  open <- c(start, close[-n])
  hi <- pmax(open, close) * exp(abs(stats::rnorm(n, 0, vol * 0.6)))
  lo <- pmin(open, close) * exp(-abs(stats::rnorm(n, 0, vol * 0.6)))
  days <- seq(as.Date("2025-01-06"), by = "day", length.out = n * 2)
  days <- days[!format(days, "%u") %in% c("6", "7")][seq_len(n)]
  data.frame(time = format(days), open = open, high = hi, low = lo, close = close,
             volume = round(stats::rlnorm(n, 10, 0.5)))
}

page_html <- function(lib, spec, d) {
  data_json <- jsonlite::toJSON(d, dataframe = "rows", digits = NA)
  sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
html,body{margin:0;height:100%%;background:%s;font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif}
#c{position:absolute;inset:0}
#legend{position:absolute;left:12px;top:10px;z-index:5;font-size:14px;color:%s;white-space:nowrap}
#legend b{font-weight:600} #legend .v{margin-left:8px}
</style></head><body><div id="c"></div><div id="legend"></div>
<script>%s</script>
<script>
var spec = %s, d = %s;
var chart = LightweightCharts.createChart(document.getElementById("c"), {
  width: window.innerWidth, height: window.innerHeight,
  layout: { background: { color: spec.bg }, textColor: spec.text, fontSize: 12 },
  grid: { vertLines: { color: spec.grid }, horzLines: { color: spec.grid } },
  rightPriceScale: { borderColor: spec.border, scaleMargins: { top: 0.12, bottom: 0.22 } },
  timeScale: { borderColor: spec.border }
});
var main, last = d[d.length - 1];
if (spec.type === "line") {
  main = chart.addAreaSeries({ lineColor: "#2962FF", topColor: "rgba(41,98,255,0.28)", bottomColor: "rgba(41,98,255,0.02)", lineWidth: 2 });
  main.setData(d.map(function (r) { return { time: r.time, value: r.close }; }));
} else {
  main = chart.addCandlestickSeries({ upColor: spec.up, downColor: spec.down, wickUpColor: spec.up, wickDownColor: spec.down, borderVisible: false });
  main.setData(d);
}
if (spec.volume) {
  var vol = chart.addHistogramSeries({ priceScaleId: "", priceFormat: { type: "volume" }, lastValueVisible: false, priceLineVisible: false });
  vol.priceScale().applyOptions({ scaleMargins: { top: 0.82, bottom: 0 } });
  vol.setData(d.map(function (r) { return { time: r.time, value: r.volume, color: r.close >= r.open ? spec.upVol : spec.downVol }; }));
}
chart.timeScale().fitContent();
var col = last.close >= last.open ? spec.up : spec.down, f = function (x) { return x.toLocaleString("en-US", { minimumFractionDigits: spec.digits, maximumFractionDigits: spec.digits }); };
document.getElementById("legend").innerHTML = "<b>" + spec.title + "</b>" +
  (spec.type === "line" ? "<span class=v style=\\"color:" + col + "\\">" + f(last.close) + "</span>" :
  ["O", "H", "L", "C"].map(function (k, i) { return "<span class=v>" + k + "<span style=\\"color:" + col + "\\">" + f(last[["open","high","low","close"][i]]) + "</span></span>"; }).join(""));
window.READY = true;
</script></body></html>', spec$bg, spec$text, lib, jsonlite::toJSON(spec, auto_unbox = TRUE), data_json)
}

dark  <- list(bg = "#131722", text = "#d1d4dc", grid = "#1e222d", border = "#2a2e39")
light <- list(bg = "#ffffff", text = "#131722", grid = "#f0f3fa", border = "#e0e3eb")
tv <- list(up = "#089981", down = "#F23645", upVol = "rgba(8,153,129,0.5)", downVol = "rgba(242,54,69,0.5)")

fixtures <- list(
  list(name = "btc_dark_candles", n = 120, start = 60000, drift = 0.003, vol = 0.025, seed = 11, digits = 2,
       spec = c(dark, tv, list(type = "candles", volume = TRUE, title = "Bitcoin / TetherUS \u00b7 1D \u00b7 Binance"))),
  list(name = "nifty_light_candles", n = 180, start = 24000, drift = -0.0006, vol = 0.009, seed = 22, digits = 2,
       spec = c(light, tv, list(type = "candles", volume = FALSE, title = "NIFTY 50 \u00b7 1D \u00b7 NSE"))),
  list(name = "usdinr_dark_line", n = 250, start = 83, drift = 0.0001, vol = 0.0025, seed = 33, digits = 3,
       spec = c(dark, tv, list(type = "line", volume = FALSE, title = "U.S. Dollar / Indian Rupee \u00b7 1D \u00b7 FX_IDC"))),
  list(name = "eth_dark_dense", n = 400, start = 3000, drift = 0.0005, vol = 0.03, seed = 44, digits = 2,
       spec = c(dark, tv, list(type = "candles", volume = TRUE, title = "Ethereum / U.S. Dollar \u00b7 4h \u00b7 Coinbase")))
)

lib <- paste(readLines(lib_path, warn = FALSE), collapse = "\n")
chromote::set_chrome_args(c(chromote::default_chrome_args(), "--no-proxy-server", "--no-first-run",
                            "--disable-background-networking", "--disable-component-update", "--disable-sync"))
b <- chromote::ChromoteSession$new(width = 1600, height = 900)
b$Emulation$setDeviceMetricsOverride(width = 1600, height = 900, deviceScaleFactor = 1, mobile = FALSE)
for (fx in fixtures) {
  d <- make_ohlc(fx$n, fx$start, fx$drift, fx$vol, fx$seed)
  fx$spec$digits <- fx$digits
  html <- file.path(tempdir(), paste0(fx$name, ".html"))
  writeLines(page_html(lib, fx$spec, d), html, useBytes = TRUE)
  b$Page$navigate(paste0("file:///", normalizePath(html, winslash = "/")), wait_ = FALSE)
  for (i in 1:80) { if (isTRUE(tryCatch(b$Runtime$evaluate("window.READY === true")$result$value, error = function(e) FALSE))) break; Sys.sleep(0.25) }
  Sys.sleep(1)
  b$screenshot(file.path(out_dir, paste0(fx$name, ".png")), wait_ = TRUE)
  utils::write.csv(d[c("time", "open", "high", "low", "close")], file.path(out_dir, paste0(fx$name, ".csv")), row.names = FALSE)
  message("wrote ", fx$name)
}
b$close()
