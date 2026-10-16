# Reading charts from screenshots (backend/chart_image.R) and the analysis built on them
# (backend/chart_analysis.R). The fixtures in tests/fixtures/ are charts drawn by TradingView's
# open-source Lightweight Charts library from SYNTHETIC prices saved next to each image
# (scripts/make_chart_fixtures.R), so the extracted values can be compared with the truth.
skip_if_not_installed("png")
skip_if_not_installed("tesseract")

fixture_raw <- function(name) {
  f <- file.path(FXLAB_ROOT, "tests", "fixtures", paste0(name, ".png"))
  readBin(f, "raw", file.size(f))
}
fixture_truth <- function(name) utils::read.csv(file.path(FXLAB_ROOT, "tests", "fixtures", paste0(name, ".csv")))

extracted <- list()
for (nm in c("btc_dark_candles", "nifty_light_candles", "eth_dark_dense", "usdinr_dark_line")) {
  extracted[[nm]] <- chart_extract(fixture_raw(nm))
}

test_that("candlestick charts are read accurately (dark, light and dense charts)", {
  for (nm in c("btc_dark_candles", "nifty_light_candles", "eth_dark_dense")) {
    e <- extracted[[nm]]; truth <- fixture_truth(nm); s <- e$series
    expect_equal(e$type, "candles", label = nm)
    expect_equal(nrow(s), nrow(truth), label = paste(nm, "candle count"))
    expect_equal(e$scale$method, "ocr")
    expect_gte(e$scale$n_labels, 10)
    expect_false(e$scale$log)
    rel <- function(a, b) mean(abs(a / b - 1))
    expect_lt(rel(s$close, truth$close), 0.001, label = paste(nm, "close error"))   # < 0.1 % on average
    expect_lt(rel(s$high, truth$high), 0.001); expect_lt(rel(s$low, truth$low), 0.001); expect_lt(rel(s$open, truth$open), 0.001)
    expect_gte(mean((s$colour == "up") == (truth$close >= truth$open)), 0.99, label = paste(nm, "colours"))
    rt <- diff(log(truth$close)); rs <- diff(log(s$close))
    expect_gt(cor(rt, rs), 0.995)
    expect_equal(sd(rs), sd(rt), tolerance = 0.02)                                  # volatility within 2 %
    expect_true(all(s$low <= pmin(s$open, s$close) + 1e-9) && all(s$high >= pmax(s$open, s$close) - 1e-9))
  }
  expect_equal(extracted$btc_dark_candles$image$theme, "dark")
  expect_equal(extracted$nifty_light_candles$image$theme, "light")
})

test_that("the legend title and interval are read from the chart", {
  expect_equal(extracted$btc_dark_candles$legend$title, "Bitcoin / TetherUS · 1D · Binance")
  expect_equal(extracted$btc_dark_candles$legend$interval, "1D")
  expect_equal(extracted$nifty_light_candles$legend$title, "NIFTY 50 · 1D · NSE")
  expect_equal(extracted$eth_dark_dense$legend$interval, "4h")
  expect_true(length(extracted$btc_dark_candles$time_axis$labels) >= 5)
})

test_that("line charts are followed along the line", {
  e <- extracted$usdinr_dark_line; truth <- fixture_truth("usdinr_dark_line"); s <- e$series
  expect_equal(e$type, "line")
  expect_true(all(is.na(s$open)))
  # The chart spaces the points evenly, so the truth can be interpolated at each sampled x.
  pos <- (s$x - min(s$x)) / (max(s$x) - min(s$x)) * (nrow(truth) - 1) + 1
  expected <- stats::approx(seq_len(nrow(truth)), truth$close, xout = pos)$y
  expect_lt(mean(abs(s$close / expected - 1)), 0.002)
  expect_gt(cor(s$close, expected), 0.99)
})

test_that("a chart without readable axis labels needs the manual price range", {
  img <- png::readPNG(fixture_raw("btc_dark_candles"))
  cropped <- png::writePNG(img[, 1:1500, ])                  # the price axis is cut off
  expect_fx_error(chart_extract(cropped), "AXIS_NOT_FOUND", "highest and lowest price")
  truth <- fixture_truth("btc_dark_candles")
  m <- chart_extract(cropped, manual = list(high = max(truth$high), low = min(truth$low)))
  expect_equal(m$scale$method, "manual")
  expect_lt(mean(abs(m$series$close / truth$close[seq_len(nrow(m$series))] - 1)), 0.01)
})

test_that("files that are not usable charts are rejected clearly", {
  expect_fx_error(chart_decode_image(charToRaw("Date,USDINR\n2025-01-01,86\n2025-01-02,87\n")), "INVALID_IMAGE", "PNG and JPEG")
  expect_fx_error(chart_decode_image(raw(0)), "INVALID_IMAGE")
  expect_fx_error(chart_decode_image(png::writePNG(array(1, c(50, 80, 3)))), "INVALID_IMAGE", "larger screenshot")
  blank <- png::writePNG(array(1, c(400, 600, 3)))
  expect_fx_error(chart_extract(blank), "NO_CHART_FOUND")
  expect_fx_error(chart_analyze_request(list(image_base64 = "%%%")), "INVALID_IMAGE")
})

# ---- Analysis -----------------------------------------------------------------------------------
a <- analyse_chart_series(extracted$btc_dark_candles)
r <- as.numeric(a$returns)
find_test <- function(id) { for (t in a$hypothesis$tests) if (t$id == id) return(t); NULL }

test_that("descriptive statistics and moments agree with base R", {
  d <- a$descriptive$returns
  expect_equal(d$mean, mean(r)); expect_equal(d$sd, sd(r)); expect_equal(d$median, median(r))
  expect_equal(as.numeric(d$moments$central)[1], 0)
  expect_equal(as.numeric(d$moments$central)[2], mean((r - mean(r))^2))
  expect_equal(as.numeric(d$moments$raw)[2], mean(r^2))
  expect_equal(d$moments$beta2, d$kurtosis)
  expect_equal(d$mode$value >= d$mode$modal_class[1] && d$mode$value <= d$mode$modal_class[2], TRUE)
  expect_equal(sum(a$descriptive$sigma_bands$observed), length(r))
  expect_equal(sum(a$descriptive$sigma_bands$normal_share), 1)
})

test_that("probability results are consistent", {
  p <- a$probability
  expect_equal(p$p_up + p$p_down, 1)
  expect_equal(sum(p$joint$probabilities), 1)
  expect_equal(p$binomial$mean, 10 * p$p_up)
  expect_equal(p$binomial$variance, 10 * p$p_up * (1 - p$p_up))
  expect_equal(sum(as.numeric(p$binomial$pmf)), 1)
  # Bayes' theorem reproduces the reverse conditional probability from the joint table.
  j <- unclass(p$joint$probabilities)
  expect_equal(p$bayes$result, j[1, 1] / sum(j[, 1]))
  expect_equal(p$poisson$mean, p$poisson$variance)
})

test_that("hypothesis tests match the textbook formulas and R's own functions", {
  t1 <- find_test("mean_two")
  expect_equal(t1$statistic, mean(r) / (sd(r) / sqrt(length(r))))
  expect_equal(t1$p_value, 2 * pnorm(-abs(t1$statistic)))
  expect_equal(as.numeric(t1$critical), c(-1, 1) * qnorm(0.975))
  ind <- find_test("chisq_independence")
  expect_equal(ind$statistic, unname(suppressWarnings(chisq.test(a$probability$joint$counts, correct = FALSE))$statistic))
  an <- find_test("anova")
  grp <- factor(ceiling(seq_along(r) * 4 / length(r)))
  expect_equal(an$statistic, summary(aov(r ~ grp))[[1]][["F value"]][1])
  ft <- find_test("variance_f")
  h <- floor(length(r) / 2); v <- c(var(r[1:h]), var(r[(h + 1):length(r)]))
  expect_equal(ft$statistic, max(v) / min(v))
  pr <- find_test("proportion")
  nd <- a$probability$n_up + a$probability$n_down
  expect_equal(pr$statistic, (a$probability$p_up - 0.5) / sqrt(0.25 / nd))
  expect_true(all(vapply(a$hypothesis$tests, function(t) t$decision == if (t$p_value < 0.05) "Reject H0" else "Fail to reject H0", logical(1))))
  e <- a$hypothesis$estimation
  expect_true(e$ci_mean[1] < e$mean && e$mean < e$ci_mean[2])
})

test_that("correlation and regression are computed on the chart prices", {
  g <- a$regression
  cl <- a$series$close
  expect_equal(g$trend$correlation$pearson_r, cor(seq_along(cl), cl))
  expect_equal(g$spearman$rho, cor(seq_along(cl), cl, method = "spearman"), tolerance = 1e-3)
  expect_equal(g$multiple$r_squared, summary(lm(cl ~ seq_along(cl) + I(seq_along(cl)^2)))$r.squared)
  expect_equal(g$lag$correlation$pearson_r, cor(r[-length(r)], r[-1]))
})

find_test_in <- function(an, id) { for (t in an$hypothesis$tests) if (t$id == id) return(t); NULL }

# Synthetic price paths to check the reasoning of "our hypothesis".
synthetic <- function(close) {
  list(series = data.frame(index = seq_along(close), x = seq_along(close), open = NA_real_, high = NA_real_, low = NA_real_,
                           close = close, colour = NA_character_, y_open = NA_real_, y_high = NA_real_, y_low = NA_real_,
                           y_close = seq_along(close)),
       type = "line", image = list(width = 100, height = 100), scale = list(method = "ocr", note = "test"),
       resolution = 0.01, plot_box = list(), legend = list(title = "Synthetic", interval = NULL), time_axis = NULL)
}

test_that("our hypothesis follows the significant drift, or the mean line when there is none", {
  set.seed(1)
  up <- analyse_chart_series(synthetic(100 * exp(cumsum(rnorm(200, 0.4, 1) / 100))))$hypothesis$outlook
  expect_equal(up$verdict, "up")
  expect_gt(up$p_higher, 0.5)
  down <- analyse_chart_series(synthetic(100 * exp(cumsum(rnorm(200, -0.4, 1) / 100))))$hypothesis$outlook
  expect_equal(down$verdict, "down")
  # No drift at all: every move is followed by the opposite move of the same size.
  x <- abs(rnorm(100))
  flat <- analyse_chart_series(synthetic(100 * exp(cumsum(c(rbind(x, -x)) / 100))))
  o <- flat$hypothesis$outlook
  one <- flat$hypothesis$tests[[2]]
  expect_true(one$p_value >= 0.05)
  expect_equal(o$verdict == "sideways", !find_test_in(flat, "recent_t")$reject)
  expect_true(o$range_low < o$expected_price && o$expected_price < o$range_high)
  expect_length(o$projection$median, o$horizon + 1)
  expect_match(o$caveat, "not a forecast guarantee")
})

test_that("the analysis needs enough points with some variation", {
  expect_fx_error(analyse_chart_series(synthetic(rep(100, 50))), "DEGENERATE_DATA")
})

test_that("legend titles stop before icons and OHLC values, and intervals are read", {
  r <- legend_title(c("Reliance", "Industries", "Limited", "1", "NSE", "8)", "H", "L", "Cc"))
  expect_equal(r$title, "Reliance Industries Limited · 1 · NSE")
  expect_equal(r$interval, "1m")
  expect_equal(r$n_words, 5)
  r <- legend_title(c("Reliance", "Industries", "Limited", "·", "1", "·", "NSE", "O1,211.5"))
  expect_equal(r$title, "Reliance Industries Limited · 1 · NSE")
  expect_equal(legend_title(c("NIFTY", "50-1D-NSE"))$title, "NIFTY 50 · 1D · NSE")
  expect_equal(legend_title(c("NIFTY", "50", "1D", "NSE"))$interval, "1D")
  expect_equal(legend_title(c("BTCUSD", "·", "240", "·", "BINANCE"))$interval, "4h")
  expect_null(legend_title(c("My", "chart"))$interval)
})
