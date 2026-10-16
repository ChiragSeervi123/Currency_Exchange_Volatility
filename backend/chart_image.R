# =============================================================================
# chart_image.R
# -----------------------------------------------------------------------------
# Purpose : Read a price series out of a chart screenshot (for example from
#           TradingView): candlesticks (open, high, low, close per candle) or a
#           line / area chart (one value per sampled point).
#
# How it works (all in R):
#   1. The PNG/JPEG is decoded into red/green/blue pixel matrices.
#   2. The background is the most common colour; dark or light theme follows.
#   3. Text is read with OCR (tesseract): the price axis labels (a column of
#      numbers at the right, or else at the left), the time axis labels at the
#      bottom and the legend line at the top (symbol and interval, e.g. "1D").
#   4. The price axis: a straight line price = a + b * y is fitted through the
#      labels (robustly, so a misread label is ignored). A logarithmic axis is
#      detected when log(price) fits better. Pixel rows become prices.
#   5. Candles: strongly coloured green (up) and red (down) pixels inside the
#      plot area. Faint copies of those colours (volume bars) and full-width
#      coloured lines (the last-price line) are ignored. Each group of adjacent
#      coloured columns is one candle: its highest and lowest pixel are the high
#      and low; the wide part (the body) gives the open and close.
#   6. Line charts: the dominant strong colour is followed column by column and
#      sampled every 6 pixels (TradingView's default bar spacing).
#
# Accuracy: a price can only be read to the nearest pixel row, so each value has
# a rounding error of about (price range of the axis) / (plot height in pixels).
# The result reports this resolution. Values are READ FROM AN IMAGE: they are
# approximations of the prices shown, not exchange or market data.
# =============================================================================

CHART_IMAGE <- list(
  max_bytes        = 8 * 1024^2,     # upload limit
  max_pixels       = 25e6,           # larger images are rejected
  target_pixels    = 4.5e6,          # larger images are reduced by block averaging first
  min_candles      = 10,             # fewer extracted points cannot support the analysis
  line_step_px     = 6,              # sampling step for line / area charts
  ocr_scale        = 4               # text crops are enlarged 4x (bilinear) before OCR
)

# ---- 1. Decoding -------------------------------------------------------------------------------
chart_decode_image <- function(raw) {
  if (!is.raw(raw) || length(raw) < 16) {
    fx_abort("The uploaded file is empty or not an image.", "INVALID_IMAGE", 400L, field = "image")
  }
  if (length(raw) > CHART_IMAGE$max_bytes) {
    fx_abort(sprintf("The image is %.1f MB; the maximum is %.0f MB.", length(raw) / 1024^2, CHART_IMAGE$max_bytes / 1024^2),
             "TOO_LARGE", 413L, field = "image")
  }
  png_sig <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  is_png <- identical(raw[1:8], png_sig)
  is_jpg <- identical(raw[1:3], as.raw(c(0xff, 0xd8, 0xff)))
  if (!is_png && !is_jpg) {
    fx_abort("Only PNG and JPEG screenshots can be analysed. Save the chart as PNG (TradingView: camera icon, 'Download image').",
             "INVALID_IMAGE", 400L, field = "image")
  }
  img <- tryCatch(if (is_png) png::readPNG(raw) else jpeg::readJPEG(raw),
                  error = function(e) fx_abort(paste("The image could not be decoded:", conditionMessage(e)),
                                               "INVALID_IMAGE", 422L, field = "image"))
  if (length(dim(img)) == 2) img <- array(img, c(dim(img), 1))
  h <- dim(img)[1]; w <- dim(img)[2]; ch <- dim(img)[3]
  if (h * w > CHART_IMAGE$max_pixels) {
    fx_abort(sprintf("The image is %d x %d pixels; images up to 25 megapixels are supported.", w, h), "TOO_LARGE", 413L, field = "image")
  }
  if (w < 300 || h < 200) {
    fx_abort(sprintf("The image is only %d x %d pixels. Upload a larger screenshot of the chart (at least 300 x 200).", w, h),
             "INVALID_IMAGE", 422L, field = "image")
  }
  # Grey or grey + alpha images are expanded to RGB; transparency is shown on white.
  rgb <- if (ch <= 2) list(img[, , 1], img[, , 1], img[, , 1]) else list(img[, , 1], img[, , 2], img[, , 3])
  if (ch == 2 || ch == 4) {
    a <- img[, , ch]
    rgb <- lapply(rgb, function(m) m * a + (1 - a))
  }
  scale <- 1
  if (h * w > CHART_IMAGE$target_pixels) {
    scale <- ceiling(sqrt(h * w / CHART_IMAGE$target_pixels))
    rgb <- lapply(rgb, block_average, k = scale)
  }
  list(r = rgb[[1]], g = rgb[[2]], b = rgb[[3]], h = nrow(rgb[[1]]), w = ncol(rgb[[1]]),
       original_width = w, original_height = h, scale = scale, format = if (is_png) "PNG" else "JPEG")
}

# Reduce a matrix by averaging k x k blocks (for very large screenshots, e.g. 4K / retina).
block_average <- function(m, k) {
  h <- floor(nrow(m) / k) * k; w <- floor(ncol(m) / k) * k
  m <- m[seq_len(h), seq_len(w)]
  rows <- rowsum(m, rep(seq_len(h / k), each = k), reorder = FALSE)
  t(rowsum(t(rows), rep(seq_len(w / k), each = k), reorder = FALSE)) / (k * k)
}

# ---- 2. Background -----------------------------------------------------------------------------
chart_background <- function(img) {
  q <- function(m) pmin(31L, as.integer(m * 32))
  key <- q(img$r) * 1024L + q(img$g) * 32L + q(img$b)
  mode <- as.integer(names(which.max(table(key))))
  sel <- key == mode
  bg <- c(mean(img$r[sel]), mean(img$g[sel]), mean(img$b[sel]))
  lum <- sum(bg * c(0.299, 0.587, 0.114))
  list(rgb = bg, dark = lum < 0.5, share = mean(sel))
}

# ---- 3. Text (OCR) -----------------------------------------------------------------------------
# Grey version of the image prepared for OCR: dark text on a light background, with
# strongly coloured pixels (candles, coloured lines and boxes) replaced by background.
ocr_canvas <- function(img, bg, drop_colour = TRUE) {
  gray <- 0.299 * img$r + 0.587 * img$g + 0.114 * img$b
  if (bg$dark) gray <- 1 - gray
  if (drop_colour) {
    chroma <- pmax(img$r, img$g, img$b) - pmin(img$r, img$g, img$b)
    gray[chroma > 0.2] <- stats::median(gray)
  }
  gray
}

# Enlarge a grey image k times with bilinear interpolation (OCR reads small digits far
# more reliably from a smooth enlargement than from blocky pixels).
upscale_bilinear <- function(m, k) {
  h <- nrow(m); w <- ncol(m)
  yi <- pmin(pmax((seq_len(h * k) - 0.5) / k + 0.5, 1), h); xi <- pmin(pmax((seq_len(w * k) - 0.5) / k + 0.5, 1), w)
  y0 <- floor(yi); y1 <- pmin(y0 + 1, h); fy <- yi - y0
  x0 <- floor(xi); x1 <- pmin(x0 + 1, w); fx <- matrix(xi - x0, length(yi), length(xi), byrow = TRUE)
  top <- m[y0, x0, drop = FALSE] * (1 - fx) + m[y0, x1, drop = FALSE] * fx
  bottom <- m[y1, x0, drop = FALSE] * (1 - fx) + m[y1, x1, drop = FALSE] * fx
  top * (1 - fy) + bottom * fy
}

ocr_region <- function(canvas, rows, cols, psm = "11", digits_only = FALSE) {
  rows <- rows[rows >= 1 & rows <= nrow(canvas)]; cols <- cols[cols >= 1 & cols <= ncol(canvas)]
  empty <- data.frame(word = character(0), confidence = numeric(0), x1 = numeric(0), y1 = numeric(0),
                      x2 = numeric(0), y2 = numeric(0), stringsAsFactors = FALSE)
  if (length(rows) < 5 || length(cols) < 5) return(empty)
  k <- CHART_IMAGE$ocr_scale
  sub <- canvas[rows, cols, drop = FALSE]
  up <- upscale_bilinear(sub, k)
  tf <- tempfile(fileext = ".png")
  on.exit(unlink(tf), add = TRUE)
  png::writePNG(up, tf)
  opts <- list(tessedit_pageseg_mode = psm)
  if (digits_only) opts$tessedit_char_whitelist <- "0123456789.,-KMB"
  eng <- tesseract::tesseract("eng", options = opts)
  d <- tryCatch(tesseract::ocr_data(tf, engine = eng), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(empty)
  bb <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric)) / k
  data.frame(word = d$word, confidence = d$confidence,
             x1 = bb[, 1] + min(cols) - 1, y1 = bb[, 2] + min(rows) - 1,
             x2 = bb[, 3] + min(cols) - 1, y2 = bb[, 4] + min(rows) - 1, stringsAsFactors = FALSE)
}

# Parse OCR tokens such as "64,174.19", "24 800.00", "1.2K", "-0.35" into numbers (NA otherwise).
parse_axis_number <- function(tokens) {
  t <- gsub("[\u2212\u2013\u2014]", "-", tokens)          # unicode minus signs
  t <- gsub("^[^0-9-]+|[^0-9KMBkmb.]+$", "", t)           # stray punctuation at the ends
  mult <- ifelse(grepl("[Kk]$", t), 1e3, ifelse(grepl("[Mm]$", t), 1e6, ifelse(grepl("[Bb]$", t), 1e9, 1)))
  t <- sub("[KkMmBb]$", "", t)
  ok <- grepl("^-?[0-9]{1,3}([, ][0-9]{3})+(\\.[0-9]+)?$", t) | grepl("^-?[0-9]+(\\.[0-9]+)?$", t)
  v <- suppressWarnings(as.numeric(gsub("[, ]", "", t)))
  v[!ok] <- NA
  v * mult
}

# Fit price = a + b * y (or log(price) = a + b * y) through axis labels, ignoring misread
# labels: every pair of labels proposes a line; the line agreeing with most labels wins.
fit_price_axis <- function(y, v) {
  best <- NULL
  for (use_log in c(FALSE, TRUE)) {
    if (use_log && any(v <= 0)) next
    z <- if (use_log) log(v) else v
    n <- length(z)
    if (n < 3) next
    tol <- 0.004 * diff(range(z)) + 1e-12
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      if (abs(y[j] - y[i]) < 3 || z[j] == z[i]) next
      b <- (z[j] - z[i]) / (y[j] - y[i])
      if (b >= 0) next                                    # prices increase upwards
      a <- z[i] - b * y[i]
      inl <- abs(z - (a + b * y)) <= max(tol, abs(b) * 2.5)   # within ~2.5 pixels
      score <- sum(inl)
      if (score >= 3 && (is.null(best) || score > best$score ||
                         (score == best$score && !use_log && best$log))) {
        best <- list(score = score, inliers = inl, log = use_log)
      }
    }
  }
  if (is.null(best)) return(NULL)
  inl <- best$inliers
  z <- if (best$log) log(v[inl]) else v[inl]
  fit <- stats::lm(z ~ y[inl])
  # A log axis is only chosen when it clearly fits better than a linear one.
  if (best$log) {
    lin <- stats::lm(v[inl] ~ y[inl])
    rel_lin <- max(abs(stats::residuals(lin))) / diff(range(v[inl]))
    rel_log <- max(abs(stats::residuals(fit))) / diff(range(z))
    if (rel_lin <= rel_log * 1.5) { fit <- lin; best$log <- FALSE }
  }
  co <- unname(stats::coef(fit))
  list(a = co[1], b = co[2], log = best$log, n_labels = sum(inl), inliers = inl,
       r_squared = summary(fit)$r.squared)
}

# Find the price axis: OCR candidate strips (right edge, then left edge) and keep the
# column of numbers that fits a straight line best.
find_price_axis <- function(img, canvas) {
  w <- img$w; h <- img$h
  strips <- list(list(side = "right", cols = seq(floor(w * 0.82), w)),
                 list(side = "right", cols = seq(floor(w * 0.62), w)),
                 list(side = "left", cols = seq_len(ceiling(w * 0.16))))
  for (s in strips) {
    words <- ocr_region(canvas, seq_len(h), s$cols, digits_only = TRUE)
    if (nrow(words) == 0) next
    words$value <- parse_axis_number(words$word)
    num <- words[is.finite(words$value), , drop = FALSE]
    if (nrow(num) < 3) next
    # Group the numbers into columns by their left edge (labels of one axis line up).
    num <- num[order(num$x1), , drop = FALSE]
    group <- cumsum(c(1, diff(num$x1) > 12))
    best <- NULL
    for (gid in unique(group)) {
      g <- num[group == gid, , drop = FALSE]
      if (nrow(g) < 3) next
      fit <- fit_price_axis((g$y1 + g$y2) / 2, g$value)
      if (!is.null(fit) && (is.null(best) || fit$n_labels > best$fit$n_labels)) best <- list(fit = fit, labels = g)
    }
    if (!is.null(best)) {
      lab <- best$labels[best$fit$inliers, , drop = FALSE]
      return(c(best$fit, list(side = s$side, x_left = min(best$labels$x1), x_right = max(best$labels$x2),
                              labels = data.frame(text = lab$word, value = lab$value, y = (lab$y1 + lab$y2) / 2))))
    }
  }
  NULL
}

# Horizontal grid lines are drawn exactly at the label prices, while the label text can sit a
# pixel or two off. When most labels have a grid line within 4 pixels, those rows are used.
snap_axis_to_grid <- function(axis, dist, box) {
  rows <- box$y1:box$y2; cols <- box$x1:box$x2
  faint <- dist[rows, cols, drop = FALSE]
  share <- rowMeans(faint > 0.012 & faint < 0.25)
  grid <- rows[share > 0.5]
  if (length(grid) < 3) return(axis)
  runs <- split(grid, cumsum(c(1, diff(grid) > 1)))
  centres <- vapply(runs, mean, numeric(1))
  lab <- axis$labels
  snapped <- vapply(lab$y, function(y) { d <- abs(centres - y); if (min(d) <= 4) centres[which.min(d)] else NA_real_ }, numeric(1))
  ok <- is.finite(snapped)
  if (sum(ok) < 3 || sum(ok) < 0.6 * nrow(lab)) return(axis)
  y <- snapped[ok]; z <- if (axis$log) log(lab$value[ok]) else lab$value[ok]
  fit <- stats::lm(z ~ y)
  co <- unname(stats::coef(fit))
  axis$a <- co[1]; axis$b <- co[2]; axis$r_squared <- summary(fit)$r.squared
  axis$n_labels <- sum(ok); axis$grid_snapped <- TRUE
  axis$labels <- data.frame(text = lab$text[ok], value = lab$value[ok], y = y)
  axis
}

# Labels along the bottom (dates / times). Returns the row where the time axis starts.
find_time_axis <- function(img, canvas, x_max) {
  h <- img$h
  rows <- seq(floor(h * 0.86), h)
  words <- ocr_region(canvas, rows, seq_len(x_max))
  words <- words[words$confidence > 50 & nchar(words$word) <= 8, , drop = FALSE]
  if (nrow(words) < 3) return(NULL)
  yc <- round((words$y1 + words$y2) / 2)
  line_y <- as.numeric(names(which.max(table(round(yc / 4) * 4))))
  on_line <- abs(yc - line_y) <= 6
  if (sum(on_line) < 3) return(NULL)
  lab <- words[on_line, , drop = FALSE]
  lab <- lab[order(lab$x1), , drop = FALSE]
  list(top = min(lab$y1), labels = lab$word, x = (lab$x1 + lab$x2) / 2)
}

# Title and interval from the OCR words of the legend line. TradingView writes
# "Name · Interval · Exchange" ("NIFTY 50 · 1D · NSE", "Reliance Industries Limited · 1 · NSE"),
# followed by icons and the O/H/L/C values. OCR may drop the dots, turn them into "-" or ".",
# or glue them to the words. A bare number as interval is minutes (1, 5, 15; 60 = 1h, 240 = 4h).
legend_title <- function(words) {
  words <- words[nzchar(trimws(words))]
  if (!length(words)) return(list(title = "", interval = NULL, n_words = 0L))
  # The title ends where icons, the O/H/L/C letters or their values start ("... NSE 8) H L C").
  stop_at <- which(seq_along(words) > 1 &
                   (grepl("^[OHLC]$|^[OHLC][0-9]", words) | grepl("[^[:alnum:]/&.,'_:!\u00b7\u2022-]", words)))
  n_words <- if (length(stop_at)) stop_at[1] - 1L else length(words)
  words <- words[seq_len(n_words)]
  iv_pattern <- "^([0-9]{1,3}[smhHDWMR]?|[DWM])$"
  as_interval <- function(x) {
    if (!grepl("^[0-9]+$", x)) return(x)
    n <- as.integer(x)
    if (n %% 60 == 0) paste0(n / 60, "h") else paste0(n, "m")
  }
  text <- paste(words, collapse = " ")
  parts <- trimws(unlist(strsplit(text, "\\s*[\u00b7\u2022]\\s*|\\s+[-.]\\s+|(?<=[[:alnum:]])-(?=[[:alnum:]])", perl = TRUE)))
  parts <- parts[nzchar(parts)]
  k <- which(seq_along(parts) > 1 & grepl(iv_pattern, parts))
  if (length(k)) {
    k <- k[1]
    exchange <- if (k < length(parts)) strsplit(parts[k + 1], "\\s+")[[1]][1] else NULL
    title <- paste(c(paste(parts[seq_len(k - 1)], collapse = " "), parts[k], exchange), collapse = " \u00b7 ")
    return(list(title = gsub("\\s+", " ", title), interval = as_interval(parts[k]), n_words = n_words))
  }
  # No separators were read: an interval-like word right before a final exchange code (NSE, BINANCE, FX_IDC).
  w <- length(words)
  if (w >= 3 && grepl(iv_pattern, words[w - 1]) && grepl("^[A-Z][A-Za-z0-9_]{1,}$", words[w])) {
    title <- paste(c(paste(words[seq_len(w - 2)], collapse = " "), words[w - 1], words[w]), collapse = " \u00b7 ")
    return(list(title = title, interval = as_interval(words[w - 1]), n_words = n_words))
  }
  list(title = gsub("\\s+", " ", text), interval = NULL, n_words = n_words)
}

# Legend line at the top-left (TradingView: "Bitcoin / TetherUS · 1D · Binance  O… H… L… C…").
# The title is read with coloured text removed (the OHLC values are coloured); the legend
# area that is excluded from the price series extends over the coloured values after it.
find_legend <- function(img, bg, canvas, x_max) {
  rows <- seq_len(max(40, floor(img$h * 0.09)))
  words <- ocr_region(canvas, rows, seq_len(x_max), psm = "11")
  words <- words[words$confidence > 50 & (grepl("[[:alnum:]]", words$word) | words$word %in% c("/", "-", "·", "•")), , drop = FALSE]
  if (nrow(words) == 0) return(NULL)
  yc <- (words$y1 + words$y2) / 2
  line <- words[abs(yc - min(yc)) <= 8, , drop = FALSE]
  line <- line[order(line$x1), , drop = FALSE]
  gap <- c(0, line$x1[-1] - line$x2[-nrow(line)])
  line <- line[cumsum(gap > 60) == 0, , drop = FALSE]        # the title is one compact group of words
  parsed <- legend_title(line$word)
  line <- line[seq_len(parsed$n_words), , drop = FALSE]
  title <- parsed$title; interval <- parsed$interval

  box <- c(x1 = min(line$x1), y1 = min(line$y1), x2 = max(line$x2), y2 = max(line$y2))
  br <- max(1, floor(box[["y1"]]) - 2):min(img$h, ceiling(box[["y2"]]) + 2)
  chroma <- pmax(img$r[br, , drop = FALSE], img$g[br, , drop = FALSE], img$b[br, , drop = FALSE]) -
            pmin(img$r[br, , drop = FALSE], img$g[br, , drop = FALSE], img$b[br, , drop = FALSE])
  coloured <- which(colSums(chroma > 0.16) > 0)
  right <- box[["x2"]]
  for (x in coloured[coloured > right]) { if (x - right <= 40) right <- x else break }
  box[["x2"]] <- right
  list(title = if (nzchar(title)) substr(title, 1, 70) else NULL, interval = interval, box = box)
}

# ---- 4. Coloured pixels ------------------------------------------------------------------------
# Logical masks of strong up (green) and down (red) pixels and of the dominant line colour.
colour_masks <- function(img, bg, box, exclude = list()) {
  rows <- box$y1:box$y2; cols <- box$x1:box$x2
  r <- img$r[rows, cols]; g <- img$g[rows, cols]; b <- img$b[rows, cols]
  chroma <- pmax(r, g, b) - pmin(r, g, b)
  sat <- which(chroma >= 0.16 & pmax(r, g, b) >= 0.2)
  dist <- sqrt((r - bg$rgb[1])^2 + (g - bg$rgb[2])^2 + (b - bg$rgb[3])^2)
  hue <- rep(NA_real_, length(r))
  if (length(sat)) hue[sat] <- grDevices::rgb2hsv(rbind(r[sat], g[sat], b[sat]), maxColorValue = 1)[1, ]
  # Regions with text (the legend) are not part of the price series.
  keep <- matrix(TRUE, length(rows), length(cols))
  for (e in exclude) {
    rr <- which(rows >= e[["y1"]] - 3 & rows <= e[["y2"]] + 3); cc <- which(cols >= e[["x1"]] - 3 & cols <= e[["x2"]] + 3)
    if (length(rr) && length(cc)) keep[rr, cc] <- FALSE
  }
  strong <- function(sel) {
    m <- matrix(FALSE, length(rows), length(cols))
    if (!any(sel, na.rm = TRUE)) return(m)
    sel[is.na(sel)] <- FALSE
    q <- stats::quantile(dist[sel], 0.99, names = FALSE)
    m[sel & dist >= 0.7 * q & keep] <- TRUE
    m
  }
  up   <- strong(!is.na(hue) & hue >= 70 / 360 & hue <= 200 / 360)
  down <- strong(!is.na(hue) & (hue <= 25 / 360 | hue >= 330 / 360))
  # Remove full-width coloured lines (the dotted last-price line, horizontal drawings).
  wide <- rowSums(up | down) > 0.3 * length(cols)
  up[wide, ] <- FALSE; down[wide, ] <- FALSE
  list(up = up, down = down, hue = hue, dist = dist, keep = keep, x0 = box$x1, y0 = box$y1)
}

# ---- 5. Candles --------------------------------------------------------------------------------
extract_candles <- function(m) {
  any_col <- colSums(m$up | m$down)
  runs <- rle(any_col > 0)
  ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
  seg <- data.frame(start = starts[runs$values], end = ends[runs$values])
  if (nrow(seg) == 0) return(NULL)
  seg$width <- seg$end - seg$start + 1
  # Pieces several times wider than a typical candle are touching candles: split them at colour
  # changes and then into equal parts of typical width.
  typical <- stats::median(seg$width[seg$width <= stats::quantile(seg$width, 0.75)])
  pitch <- max(1, typical)
  pieces <- list()
  for (i in seq_len(nrow(seg))) {
    cols <- seg$start[i]:seg$end[i]
    if (seg$width[i] <= max(2, 1.8 * pitch)) { pieces[[length(pieces) + 1]] <- cols; next }
    colour <- ifelse(colSums(m$up[, cols, drop = FALSE]) >= colSums(m$down[, cols, drop = FALSE]), 1, -1)
    rr <- rle(colour); ce <- cumsum(rr$lengths); cs <- ce - rr$lengths + 1
    for (j in seq_along(cs)) {
      sub <- cols[cs[j]:ce[j]]
      k <- max(1, min(length(sub), round(length(sub) / (pitch + 1))))
      parts <- split(sub, ceiling(seq_along(sub) * k / length(sub)))
      pieces <- c(pieces, unname(parts))
    }
  }
  out <- lapply(pieces, function(cols) {
    u <- m$up[, cols, drop = FALSE]; d <- m$down[, cols, drop = FALSE]
    nu <- sum(u); nd <- sum(d)
    if (nu + nd < 3) return(NULL)
    is_up <- nu >= nd
    px <- if (is_up) u else d
    rows_any <- which(rowSums(px) > 0)
    hi <- min(rows_any); lo <- max(rows_any)
    width <- length(cols)
    cover <- rowSums(px)
    body <- if (width >= 3) which(cover >= max(2, ceiling(0.5 * width))) else rows_any
    if (length(body) == 0) body <- rows_any
    top <- min(body); bottom <- max(body)
    # Wide and flat shapes are labels or boxes, not candles.
    if (width > 4 * pitch && (lo - hi + 1) < width) return(NULL)
    data.frame(x = mean(cols) + m$x0 - 1, high_y = hi + m$y0 - 1, low_y = lo + m$y0 - 1,
               open_y = (if (is_up) bottom else top) + m$y0 - 1, close_y = (if (is_up) top else bottom) + m$y0 - 1,
               up = is_up, width = width, pixels = nu + nd)
  })
  out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
  if (is.null(out)) return(NULL)
  out[order(out$x), , drop = FALSE]
}

# ---- 6. Line / area charts -----------------------------------------------------------------------
extract_line <- function(m, step = CHART_IMAGE$line_step_px) {
  ok <- !is.na(m$hue) & m$keep
  if (sum(ok) < 50) return(NULL)
  bins <- floor(m$hue[ok] * 36)
  top_bin <- as.integer(names(which.max(table(bins))))
  sel <- ok & abs(floor(m$hue * 36) - top_bin) <= 1
  sel[is.na(sel)] <- FALSE
  q <- stats::quantile(m$dist[sel], 0.995, names = FALSE)     # the line itself, not a fainter area fill
  line <- sel & m$dist >= 0.6 * q
  line[rowSums(line) > 0.3 * ncol(line), ] <- FALSE        # horizontal price lines
  # The line is the top-most run of line-coloured pixels in each column (an area fill or a
  # second line further down is ignored).
  ys <- apply(line, 2, function(col) {
    r <- which(col)
    if (!length(r)) return(NA_real_)
    run <- r[seq_len(which(c(diff(r) > 1, TRUE))[1])]
    mean(run)
  })
  cols <- which(is.finite(ys))
  if (length(cols) < 20) return(NULL)
  pick <- cols[seq(1, length(cols), by = step)]
  data.frame(x = pick + m$x0 - 1, y = ys[pick] + m$y0 - 1)
}

# ---- 7. Whole extraction -----------------------------------------------------------------------------
# manual: optional list(high =, low =) - the highest and lowest price on the chart, used when the
# price axis cannot be read (the extracted series is then scaled to that range).
chart_extract <- function(raw, manual = NULL) {
  img <- chart_decode_image(raw)
  bg <- chart_background(img)
  canvas <- ocr_canvas(img, bg)
  dist <- sqrt((img$r - bg$rgb[1])^2 + (img$g - bg$rgb[2])^2 + (img$b - bg$rgb[3])^2)
  axis <- find_price_axis(img, canvas)
  x_max <- img$w; x_min <- 1
  if (!is.null(axis)) {
    # The plot ends at the axis border line (a column that differs from the background almost
    # everywhere) between the candles and the labels; otherwise a little before the labels.
    if (axis$side == "right") {
      near <- rev(seq(max(1, floor(axis$x_left) - 30), floor(axis$x_left) - 1))
      border <- near[colMeans(dist[, near, drop = FALSE] > 0.02) > 0.9]
      x_max <- if (length(border)) border[1] - 1 else floor(axis$x_left) - 10
    } else {
      near <- seq(ceiling(axis$x_right) + 1, min(img$w, ceiling(axis$x_right) + 30))
      border <- near[colMeans(dist[, near, drop = FALSE] > 0.02) > 0.9]
      x_min <- if (length(border)) border[1] + 1 else ceiling(axis$x_right) + 10
    }
  }
  time_axis <- find_time_axis(img, canvas, x_max)
  legend <- find_legend(img, bg, canvas, x_max)
  y_max <- if (!is.null(time_axis)) floor(time_axis$top) - 3 else img$h
  y_min <- 1
  if (!is.null(axis) && is.null(manual)) {
    # Toolbars above and indicator panes below lie outside the span of the price labels.
    ly <- sort(axis$labels$y)
    spacing <- if (length(ly) > 1) stats::median(diff(ly)) else 40
    y_min <- max(1, floor(min(ly) - 1.5 * spacing))
    y_max <- min(y_max, ceiling(max(ly) + 1.5 * spacing))
  }
  box <- list(x1 = x_min, x2 = x_max, y1 = y_min, y2 = y_max)
  if (!is.null(axis)) axis <- snap_axis_to_grid(axis, dist, box)
  exclude <- if (!is.null(legend)) list(legend$box) else list()
  masks <- colour_masks(img, bg, box, exclude)

  candles <- extract_candles(masks)
  n_candle_px <- sum(masks$up) + sum(masks$down)
  type <- if (!is.null(candles) && nrow(candles) >= CHART_IMAGE$min_candles && n_candle_px > 200) "candles" else "line"
  points <- if (type == "line") extract_line(masks) else NULL
  if (type == "line" && is.null(points)) {
    fx_abort(paste("No candles or price line were found in the image. Upload a screenshot of a candlestick, line or",
                   "area chart with the price axis visible."), "NO_CHART_FOUND", 422L, field = "image")
  }

  # Pixel rows -> prices
  if (!is.null(manual)) {
    ys <- if (type == "candles") c(candles$high_y, candles$low_y) else points$y
    top <- min(ys); bottom <- max(ys)
    if (bottom - top < 5) fx_abort("The chart spans too few pixel rows to be scaled.", "NO_CHART_FOUND", 422L)
    to_price <- function(y) manual$high + (y - top) * (manual$low - manual$high) / (bottom - top)
    scale_info <- list(method = "manual", log = FALSE, n_labels = 0,
                       note = sprintf("Scaled to the highest (%s) and lowest (%s) price you entered.",
                                      format(manual$high), format(manual$low)))
  } else if (!is.null(axis)) {
    to_price <- if (axis$log) function(y) exp(axis$a + axis$b * y) else function(y) axis$a + axis$b * y
    scale_info <- list(method = "ocr", log = axis$log, n_labels = axis$n_labels, r_squared = axis$r_squared,
                       side = axis$side, labels = axis$labels,
                       note = sprintf("Price axis read automatically from %d labels on the %s (%s scale).",
                                      axis$n_labels, axis$side, if (axis$log) "logarithmic" else "linear"))
  } else {
    fx_abort(paste("The price axis labels could not be read. Open 'Optional settings', enter the highest and lowest",
                   "price shown on the chart, and analyse again."), "AXIS_NOT_FOUND", 422L, field = "manual_high")
  }

  if (type == "candles") {
    series <- data.frame(index = seq_len(nrow(candles)), x = candles$x,
                         open = to_price(candles$open_y), high = to_price(candles$high_y),
                         low = to_price(candles$low_y), close = to_price(candles$close_y),
                         colour = ifelse(candles$up, "up", "down"),
                         y_open = candles$open_y, y_high = candles$high_y, y_low = candles$low_y, y_close = candles$close_y)
  } else {
    series <- data.frame(index = seq_len(nrow(points)), x = points$x, open = NA_real_, high = NA_real_, low = NA_real_,
                         close = to_price(points$y), colour = NA_character_,
                         y_open = NA_real_, y_high = NA_real_, y_low = NA_real_, y_close = points$y)
  }
  if (any(!is.finite(series$close)) || any(series$close <= 0)) {
    fx_abort("The price scale read from the image gives zero or negative prices, so returns cannot be calculated.",
             "INVALID_DATA", 422L)
  }
  if (nrow(series) < CHART_IMAGE$min_candles) {
    fx_insufficient(sprintf("Only %d price points were found in the image; at least %d are needed.",
                            nrow(series), CHART_IMAGE$min_candles))
  }
  rows_span <- if (type == "candles") diff(range(c(candles$high_y, candles$low_y))) else diff(range(points$y))
  price_span <- diff(range(c(series$high, series$low, series$close), na.rm = TRUE))
  list(
    series = series, type = type,
    image = list(width = img$original_width, height = img$original_height, analysed_width = img$w,
                 analysed_height = img$h, scale = img$scale, format = img$format,
                 theme = if (bg$dark) "dark" else "light"),
    plot_box = box, scale = scale_info,
    resolution = if (rows_span > 0) price_span / rows_span else NA_real_,
    legend = legend[c("title", "interval")],
    time_axis = if (!is.null(time_axis)) list(labels = I(time_axis$labels)) else NULL
  )
}
