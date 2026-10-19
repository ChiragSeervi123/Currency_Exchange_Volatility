# Chart Reader — Probability & Statistics of Uploaded Graphs Using R

Chart Reader is a Probability and Statistics project with a working web application: a **graph reader**. You **upload a screenshot
of a price chart** (for example USD/INR, Nifty 50 or Bitcoin from TradingView) in the Chart Upload panel;
the **R backend** (an R Plumber API) reads the candles and the price axis from the image and then applies
every syllabus topic to that price series — descriptive statistics, probability and distributions,
hypothesis tests and regression — and finishes with **our own hypothesis**: is the price more likely to
go up, go down or stay close to its mean line? **R performs every calculation**, including reading the
image; JavaScript only handles the interface and never computes statistics.

```
Browser (HTML/CSS/JS, Chart.js)  --screenshot (base64 JSON)-->  R Plumber API (backend/api.R)
        ^                                                              |
        |                                                              v
        +------ JSON results ------  backend/chart_image.R     reads candles + price axis (OCR)
                                     backend/chart_analysis.R  statistics of the extracted prices
```

---

## 1. Features

| Section | What R computes from the uploaded chart |
|---|---|
| **Chart Analysis Dashboard** (with its **Chart Upload** panel, the only input in the app) | Upload (browse, drag-and-drop or paste a screenshot), optional chart name / candle interval / manual price range. KPI cards (last price and its change, mean price, volatility σ per candle, total return over the chart), the price series read from the chart (candles or line, with the mean line) or an overlay of the extracted points on the screenshot, the distribution of returns with a normal curve, summary statistics and a normality check, and **Our Hypothesis** (up / down / close to the mean line, expected price and 95% range after 10 candles, P(higher)) |
| **Descriptive Statistics** | Summary statistics of the returns per candle: mean, median, grouped mode, quartiles, range, sample and population variance, SD, SE, raw and central moments μ′₁…μ′₄ and μ₁…μ₄, β₁, β₂, skewness, kurtosis; price mean/median/mode/SD/CV. Charts: histogram with the fitted normal curve and a bar graph of observed vs normal frequencies |
| **Probability Distributions** | P(up), P(down), conditional probabilities P(up \| previous up/down), independence check, Bayes' theorem; the joint (bivariate) distribution of consecutive candles with marginal and conditional distributions; binomial model (up candles among the next 10: np, npq, skewness, kurtosis, observed vs model), Poisson model (extreme moves per 20 candles: λ, mean = variance, observed vs model) and normal model (μ, σ², MGF, event probabilities vs observed) |
| **Hypothesis Testing** | The general procedure, point and interval estimates (μ and P), and nine tests at α = 0.05, each with its formula, H₀/H₁, calculated and critical values, p-value, decision and conclusion: Z-test for the mean (two- and one-tailed), Z-test for the proportion of up candles, small-sample t-test on the last 15 candles, difference of means (first vs second half), F-test for variances, χ² goodness of fit (normal), χ² test of independence (consecutive candles), one-way ANOVA (four periods). Then **Our hypothesis**: the verdict, the reasons from those results, insights from the graph and a projection chart |
| **Correlation & Regression** | Scatter diagram of price against candle number with the least-squares line, Pearson r, Spearman rank correlation (1 − 6Σd²/n(n²−1)), multiple regression on t and t², regression of each return on the previous return, coefficient tables with HC1 robust errors, residual diagnostics |
| **Chart Data** | How the chart was read (axis labels used, resolution, title, time labels), the extracted points drawn on the screenshot, closing prices with the high–low band, returns and rolling volatility, and a table of every extracted candle |
| **Report** | The whole analysis as one document: summary and verdict, the chart and how it was read, descriptive statistics, probability, every hypothesis test with its formula, our hypothesis, regression and limitations, with the screenshot and four charts. **Download report** saves a standalone HTML file (charts and screenshot embedded, opens in any browser); **Print / Save as PDF** prints just the report (white paper layout) |
| **Storage** | A local library of every chart you have uploaded (saved automatically — see section 6). Each card shows a thumbnail, name, interval, last price, change and verdict; **Open** makes it the current analysis again without re-uploading it, **Report** opens its report directly, and **Delete** / **Clear all** remove saved charts |
| **About Project** | Methods, data, assumptions and limitations |

**Interface.** The design follows `frontend/assets/fxlab-reference.png`: a blurred mountain-at-sunset scene,
frosted-glass panels, a floating circular navigation rail and the **Chart Upload** panel on the dashboard.
The rail has one icon per page (Dashboard, Distribution Analysis, Hypothesis Testing, Correlation &
Regression, Chart Data, Storage, Report, About). Chart Upload is a fixed part of the dashboard (not a pop-up). On desktop
(1200 px and wider) it is the dashboard's left column, laid out on shared lines: its top is level with the
first rail icon and the KPI cards (the dashboard has no title block), and its bottom with the lower panels. On tablets
and phones it appears under the title and the rail becomes a bottom bar. No other page has input fields:
every page shows the analysis of the uploaded chart (and explains how to upload one until you do). The
analysis stays in the R process, so refreshing the page does not lose it — including the Chart Upload panel's own
preview of the screenshot, restored from R (not just the KPIs and charts). **Clear** removes the *current*
analysis only; the chart itself stays in the Storage library (section 6) until you delete it there.
Charts can be downloaded as PNG and expanded to full screen. Nothing is ever presented as a live quote.

---

## 2. Technology stack

* **R 4.x** (developed with R 4.6.1) — all statistics; RStudio project `FXLab.Rproj`.
* **plumber** — HTTP API (also serves the web page). **jsonlite** — JSON. **png**, **jpeg** — read the
  screenshot. **tesseract** — OCR of the axis and legend text (the Windows/macOS package includes the
  English language data; free and offline).
* **HTML, CSS, vanilla JavaScript** — interface. **Chart.js 4.4.7**, **Lucide** icons (ISC) and the
  **Inter** font (SIL OFL) are stored locally in `frontend/assets/`, so no internet connection is needed.
  The background (`assets/background.svg`) is original generated artwork.
* **testthat, curl, processx, withr** — tests. Optional: **moments** (extra cross-checks) and
  **chromote** + Google Chrome (headless-browser test).
* No database, no Node.js/Python backend, no paid or cloud services.

---

## 3. Project structure

```
pns2/                          (project folder; open FXLab.Rproj)
├── FXLab.Rproj                RStudio project
├── start_fxlab.bat            double-click to start the app on Windows
├── backend/
│   ├── main.R                 START HERE: loads everything and runs the API + web page
│   ├── setup.R                finds the project folder, sources the modules, loads the data
│   ├── config.R               all settings: port, data files, limits, conventions
│   ├── api.R                  Plumber routes only (no statistics)
│   ├── validation.R           input validation, error codes, JSON body parsing
│   ├── statistics.R           returns, descriptive statistics, volatility, histogram/Q-Q data
│   ├── chart_image.R          reads a chart screenshot: candles / line, price axis (OCR), legend
│   ├── chart_analysis.R       every analysis of the extracted prices + "our hypothesis"
│   ├── chart_storage.R        the Storage library: save / list / reload / delete saved charts
│   ├── historical_analysis.R  CSV loading + validation, dataset state, historical summaries (bundled data)
│   ├── distributions.R        probability distributions (API)
│   ├── hypothesis_testing.R   tests and confidence intervals (API, and helpers reused for charts)
│   └── regression_analysis.R  correlation and regression (fit_simple_regression is reused for charts)
├── frontend/
│   ├── index.html             the single-page interface: top bar, rail, dashboard (with Chart Upload), analysis pages
│   ├── style.css              glass theme; design tokens at the top (colours, glass, layout)
│   ├── js/config.js           API base URL (the one place to change it)
│   ├── js/api.js              API client (errors, timeouts, no duplicate requests)
│   ├── js/ui.js, charts.js, format.js   shared helpers (display only)
│   ├── report.css             styles of the Report page and of the downloaded report file
│   ├── js/chart-upload.js     the Chart Upload panel and the shared analysis (FX.chart)
│   ├── js/app.js              navigation, rail, backend status, chart popover, clock
│   ├── js/sections/*.js       one file per page (dashboard.js = the main dashboard)
│   └── assets/                background.svg, icons.svg (Lucide sprite), fonts/ (Inter),
│                              vendor/chart.umd.min.js (Chart.js), fxlab-reference.png (design reference)
├── data/
│   ├── exchange_rates.csv     USD/INR, Date,USDINR (FRED DEXINUS, 1973-01-02 to 2026-10-09)
│   ├── dollar_index.csv       Date,DollarIndex (FRED DTWEXBGS, 2006-01-02 to 2026-10-09)
│   ├── raw/                   untouched FRED downloads + download_log.csv (time, MD5)
│   └── charts/                the Storage library (created on first upload; not in version control)
├── scripts/
│   ├── install_packages.R     installs the R packages
│   ├── prepare_data.R         rebuilds data/*.csv from data/raw (optionally re-downloads FRED)
│   ├── make_chart_fixtures.R  builds the test screenshots in tests/fixtures (developer tool)
│   └── run_tests.R            runs all tests
└── tests/                     testthat tests (chart reading, statistics, distributions, historical
    │                          data, hypothesis tests, regression, API, browser)
    └── fixtures/              test screenshots (synthetic prices) + their true prices (.csv)
```

---

## 4. Installation (once)

1. Install **R** (4.2 or newer) from <https://cran.r-project.org/>. RStudio is optional but recommended.
2. Install the packages. In a terminal opened in the project folder:

   ```bat
   "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/install_packages.R
   ```

   or in the RStudio console: `source("scripts/install_packages.R")`
   (minimum to run the app: `install.packages(c("plumber", "png", "jpeg", "tesseract"))`).

> If `Rscript` is on your PATH you can type `Rscript` instead of the full path. Adjust `R-4.6.1` to your
> installed version (look in `C:\Program Files\R\`).

---

## 5. Running Chart Reader

The R backend serves both the API and the web page, so **one process is enough**.

**Option A — RStudio**
1. Open `FXLab.Rproj`.
2. Open `backend/main.R` and click **Source**.
3. The browser opens <http://127.0.0.1:8000/>. Stop the server with **Esc** (or the red stop sign) in the console.

**Option B — Windows terminal (Command Prompt or PowerShell), from the project folder**
```bat
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" backend/main.R
```
Then open <http://127.0.0.1:8000/>. Stop with **Ctrl+C**.

**Option C — double-click `start_fxlab.bat`.** It finds Rscript, starts the backend and opens the browser.

Useful addresses while it runs:

| URL | Purpose |
|---|---|
| <http://127.0.0.1:8000/> | the web interface (redirects to `/app/`) |
| <http://127.0.0.1:8000/health> | API status and dataset availability |
| <http://127.0.0.1:8000/__docs__/> | interactive endpoint explorer generated by Plumber |

**Another port:** set `FXLAB_PORT` first, e.g. `set FXLAB_PORT=8001` (cmd) or `$env:FXLAB_PORT=8001`
(PowerShell), then start as above.

**Serving the page from another local server (optional).** If you open `frontend/` with an editor's
live-preview server (any `http://localhost:<port>` or `http://127.0.0.1:<port>` address), keep the backend
running; CORS allows these local origins and `frontend/js/config.js` points the page at
`http://127.0.0.1:8000`. Opening `index.html` directly from disk (`file://`) is not supported — the page
says so and tells you which URL to open.

---

## 6. How a chart is read (and what to upload)

**What to upload.** A PNG or JPEG (up to 8 MB) of a candlestick, line or area chart with its **price axis
visible**. TradingView: the camera icon → *Download image* gives the cleanest picture; an ordinary
screenshot (Win + Shift + S) works too and can be pasted straight onto the dashboard with Ctrl + V. Dark
and light themes are both supported. Keep the whole price axis in the picture and avoid covering the
candles with drawings or pop-ups.

**What R does** (`backend/chart_image.R`):
1. Decodes the image (very large screenshots, e.g. 4K, are reduced by block averaging first).
2. Finds the background colour (dark or light theme).
3. Reads text with OCR: the column of numbers on the price axis (right, or left), the time axis labels and
   the legend (symbol and interval, e.g. "Bitcoin / TetherUS · 1D · Binance").
4. Fits price = a + b·y through the axis labels, robustly, so a misread label is ignored; a logarithmic
   axis is detected when log(price) fits clearly better. Labels are snapped to the horizontal grid lines
   when those are visible.
5. Candles: strongly coloured green (up) and red (down) pixels inside the plot area. Faint copies of those
   colours (volume bars), full-width lines (the last-price line), the legend and anything outside the span
   of the axis labels (toolbars, indicator panes) are ignored. Each group of coloured columns is a candle:
   its top and bottom pixel give the high and low, its body gives the open and close.
6. Line / area charts: the dominant strong colour is followed column by column and sampled every 6 pixels.

**Accuracy.** Each price is read to about one pixel row of the chart (the resolution is shown on the
Chart Data page and in the dashboard footnote). On the test screenshots the average error of the closing
prices is 0.01–0.03 % and the volatility is within 2 % of the truth. Line charts are sampled points, not
the original data points. If the axis cannot be read, enter the highest and lowest price on the chart in
*Optional settings*; the extracted series is then scaled to that range.

**Bundled data.** The FRED USD/INR series (`data/exchange_rates.csv`, DEXINUS) and the Broad Dollar Index
(`data/dollar_index.csv`, DTWEXBGS) from the original project are still included and served by the
`/historical`, `/statistics`, `/hypothesis-test` and `/regression` endpoints (useful from R or the API
explorer), but the website now analyses only the uploaded chart. No live data is ever fetched.

**The Storage library.** Every chart analysed with `POST /chart/analyze` — whether from the Chart Upload
panel or re-opened from Storage — is saved automatically by `backend/chart_storage.R` to `data/charts/`
(created on first use, excluded from version control): the screenshot plus the exact object the endpoint
returned, so reopening a saved chart needs no re-reading of the image. It survives restarting the backend
(the app is single-user and local by design; see "Known limitations"). The library keeps the most recent
`FXLAB_CONFIG$limits$chart_store_max` charts (default 60), dropping the oldest automatically. The Storage
page lists them newest first; **Open** and **Report** make a saved chart the current analysis again
(`POST /storage/<id>/load`) without re-uploading it, and **Delete** / **Clear all** remove them for good.

---

## 7. Statistical formulas and assumptions

All analyses use the closing prices P₁…Pₙ read from the chart; one observation is one candle (or one
sampled point of a line chart). α = 0.05 throughout.

| Quantity | Definition used in R |
|---|---|
| Return per candle | r_t = 100 · ln(P_t / P_(t−1)) % |
| Sample variance / SD | s² = Σ(x − x̄)² / (n − 1); population variance (÷ n) also shown |
| Mode (grouped) | L + (f₁ − f₀) / (2f₁ − f₀ − f₂) · h from the histogram's modal class |
| Moments | raw μ′_k = mean(x^k); central μ_k = mean((x − x̄)^k); β₁ = μ₃²/μ₂³, β₂ = μ₄/μ₂² |
| Skewness / kurtosis | γ₁ = μ₃ / μ₂^(3/2); β₂ (normal = 3); excess γ₂ = β₂ − 3 |
| Conditional probability, Bayes | P(A \| B) = P(A∩B)/P(B); P(B \| A) = P(A \| B)·P(B)/P(A), from consecutive candles |
| Binomial | X = up candles in the next 10 ~ B(10, p̂): mean np, variance npq, skewness (q−p)/√npq, kurtosis 3 + (1−6pq)/npq |
| Poisson | X = moves beyond 2 SD per 20 candles ~ P(λ̂): mean = variance = λ, skewness 1/√λ, kurtosis 3 + 1/λ |
| Normal | R ~ N(x̄, s²); MGF M(t) = exp(μt + σ²t²/2) |
| Z-test (mean, n ≥ 30) | Z = x̄ / (s/√n); t with n − 1 df for small samples |
| Z-test (proportion) | Z = (p̂ − 0.5) / √(P₀Q₀/n) |
| Difference of means | Z = (x̄₂ − x̄₁) / √(s₁²/n₁ + s₂²/n₂) (Welch t for small halves) |
| F-test | F = larger s² / smaller s², two-tailed |
| χ² goodness of fit | Σ(O − E)²/E over z-classes, df = k − 1 − 2; classes merged until E ≥ 5 |
| χ² independence | 2 × 2 table (previous × current direction), df = 1, no continuity correction |
| ANOVA | F = MSB / MSW for four consecutive periods |
| Regression | least squares price = b₀ + b₁t; multiple regression price = b₀ + b₁t + b₂t²; HC1 robust SEs |
| Spearman | ρ = 1 − 6Σd² / n(n² − 1) (average ranks for ties) |
| Outlook | "up"/"down" when the one-tailed drift test is significant (strong when the recent t-test agrees); "up"/"down" with weak evidence when only the recent t-test is significant; otherwise "close to the mean line". Projection P_last · exp(hμ ± 1.96σ√h), h = 10; P(higher) = Φ(μ√h/σ) |

**Assumptions and caveats** (also shown on the pages):
* Prices are approximate (read from pixels). Line charts are sampled every 6 pixels, which can make
  consecutive moves look more alike than they were (shown as a note).
* The tests treat returns as independent observations; prices themselves trend, so p-values of
  price-on-time regressions are optimistic and are presented as descriptive.
* "Fail to reject H₀" is never presented as proof that H₀ is true.
* The outlook describes what the statistics of the chart suggest under a random-walk model. It is not a
  forecast guarantee or investment advice.

---

## 8. API reference

All responses share one envelope:

```json
{ "success": true,  "data": { ... }, "message": "Chart analysed", "error": null }
{ "success": false, "data": null, "message": "'interval' must be one of: 1m, 5m, 15m, 30m, 1h, 2h, 4h, 1D, 1W, 1M.",
  "error": { "code": "VALIDATION_ERROR", "field": "interval", "status": 400 } }
```

| Status | Code(s) | Meaning |
|---|---|---|
| 200 | — | success |
| 400 | `VALIDATION_ERROR`, `MALFORMED_JSON`, `INVALID_IMAGE` | a value is missing/invalid, the body is not a JSON object, or the file is not a PNG/JPEG |
| 404 | `DATA_UNAVAILABLE`, `NOT_FOUND` | data file missing, no chart uploaded yet, or unknown route |
| 413 | `TOO_LARGE`, `PAYLOAD_TOO_LARGE` | image above 8 MB / 25 megapixels, CSV above 5 MB |
| 422 | `NO_CHART_FOUND`, `AXIS_NOT_FOUND`, `INVALID_DATA`, `INSUFFICIENT_DATA`, `DEGENERATE_DATA` | no candles/line in the image, unreadable price axis (enter the price range), or data that cannot support the analysis |
| 500 | `INTERNAL_ERROR` | unexpected; details only in the R console, never in the response |

POST bodies are JSON (`Content-Type: application/json`). Numbers must be JSON numbers. Optional fields can
be omitted. Dates are `YYYY-MM-DD`. Custom samples (`values`, `x`, `y`) may be a JSON array or a text string
of numbers separated by commas/spaces/new lines (max 20,000).

### `GET /health`
`data`: `status` ("ok"), `api_version`, `r_version`, `plumber_version`, `server_time`,
`statistical_engine` ("R"), `datasets.usdinr` / `datasets.dollar_index` (`available`, `kind`, `observations`,
`first_date`, `last_date`, or `message` when unavailable), `uploaded_dataset_active`, `chart_loaded`, `ocr_engine`.

### `GET /defaults`
`data`: `chart_intervals`, `limits`, `distributions` (parameter catalogue), `hypothesis_tests`,
`regression_models`, `alpha_choices`, `default_alpha`, `default_start_date`, `trading_days_per_year`,
`rolling_window`.

### `POST /chart/analyze`
| Field | Type | Rule |
|---|---|---|
| `image_base64` | string | required: the PNG/JPEG as base64 or a data URL (≤ 8 MB, ≥ 300 × 200 px) |
| `file_name` | string | optional, ≤ 120 characters (shown on the pages) |
| `chart_name` | string | optional, ≤ 70 characters; otherwise read from the chart legend |
| `interval` | string | optional: `1m`, `5m`, `15m`, `30m`, `1h`, `2h`, `4h`, `1D`, `1W`, `1M`; otherwise read from the legend |
| `manual_high`, `manual_low` | numbers | optional, both or neither, high > low > 0: the price range when the axis cannot be read |

`data`: `chart` (name, interval, type `candles`/`line`, `n_points`, image size and theme, `scale` with the
method, axis labels used and linear/log, `resolution` per pixel row, `plot_box`, detected title/interval,
time labels, `notes`), `series` (index, x, open, high, low, close, colour, pixel rows), `returns`,
`rolling`, `overview`, `descriptive`, `probability`, `hypothesis` (`estimation`, `tests[]` with formula,
hypotheses, statistic, critical values, p-value, decision, conclusion and the null-distribution curve, and
`outlook` with the verdict, reasons, insights, projection and caveat) and `regression`. The analysis and the
image are kept in the R process for the next endpoints.

### `GET /chart/current`, `GET /chart/image`, `POST /chart/reset`
The last analysis (`data` is `null` before the first upload), the uploaded image itself (404 before), and
removing both (the chart's saved copy in Storage, if any, is not affected).

### `GET /storage`, `GET /storage/<id>/image`, `POST /storage/<id>/load`, `POST /storage/<id>/delete`, `POST /storage/clear`
The Storage library: every chart analysed with `POST /chart/analyze` is saved here automatically (see
section 6). `GET /storage` → `data`: `charts[]` (`id`, `saved_at` ISO 8601 UTC, `name`, `interval`, `type`,
`n_points`, `file_name`, `last_price`, `change_pct`, `verdict`, `strength`), newest first, and `max` (how
many are kept). `GET /storage/<id>/image` serves that chart's screenshot (404 for an unknown id).
`POST /storage/<id>/load` makes it the current analysis again — the response has the same shape as
`POST /chart/analyze` — without re-reading the image. `POST /storage/<id>/delete` → `{"deleted": true|false}`;
`POST /storage/clear` → `{"removed": <count>}`. An id that is not exactly `<digits>-<digits>` is rejected
with a 400 before any file is touched.

### `POST /distribution`
`{"distribution": "binomial", "params": {"size": 20, "prob": 0.3}, "query": {"type": "between", "a": 4, "b": 8}, "quantile_p": 0.95, "sample_size": 1000, "seed": 1}`
* `distribution`: `normal` (`mean`, `sd` > 0), `uniform` (`min` < `max`), `exponential` (`rate` > 0),
  `binomial` (`size` 1…100,000 integer, `prob` 0…1), `poisson` (`lambda` > 0).
* `query.type`: `less_equal` P(X ≤ a), `greater` P(X > a), `between` P(a ≤ X ≤ b). Optional.
* `data`: `theoretical` (mean, variance, sd, skewness, excess kurtosis, median, mode), `curve` (`x`,
  `density`, `cdf`), `quantiles[]`, `probability`, `quantile_query`, `sample` (summary vs theory, histogram or
  discrete frequencies).

### `POST /statistics`
* Historical: `{"source": "historical", "variable": "log_return", "start_date": "2016-01-01", "end_date": "2025-12-31"}`
  (`variable`: `log_return`, `simple_return`, `level`).
* Custom: `{"source": "custom", "values": "83.1, 83.4, 83.2", "data_type": "sample"}` (`data_type`
  `prices` derives returns and volatility from positive prices in time order).
* `data`: `statistics` (all descriptive measures + `quantiles[]` + `warnings`), `shape_interpretation`,
  `histogram[]`, `normal_fit`, `qq`, `tails[]`, `volatility` (returns), `data_description`.

### `GET /historical/summary?start_date=YYYY-MM-DD&end_date=YYYY-MM-DD`
`data`: `dataset` (source, file, observations, date range, full `quality` report), `window`, `latest` (rate,
date, previous observation, change and % change, "not a live quote" note), `recent` (last 60 observations),
`change`, `level_statistics`, `return_statistics`, `volatility`, `annual[]`,
`largest_rises[]`, `largest_falls[]`, `warnings`.

### `GET /historical/data?start_date=&end_date=&max_points=`
`max_points` 50 … 3000 (default 3000). `data.series`: `date`, `rate`, `log_return_pct`,
`rolling_ann_vol_pct`, `cumulative_return_pct` (evenly thinned for charts when longer; statistics always use
every observation), plus `n_total`, `n_returned`, `thinned`, `note`.

### `POST /historical/upload` and `POST /historical/reset`
Upload: `{"csv_text": "Date,USDINR\n2025-01-01,85.6\n...", "file_name": "my_rates.csv"}` → dataset info and
validation report (422 with the reason if rejected). Reset: `{}` → back to the bundled FRED data.

### `POST /hypothesis-test`
`{"test": "one_sample_t", "alpha": 0.05, "source": "historical", "variable": "log_return", "start_date": "2016-01-01", "end_date": "2025-12-31", "mu0": 0, "alternative": "two.sided"}`
* `test`: `one_sample_t`, `confidence_interval`, `normality`, `two_sample_t`, `variance_equality`, `autocorrelation`.
* `alpha`: 0.001 … 0.2 (the form offers 0.01, 0.05, 0.10). `alternative`: `two.sided`, `less`, `greater`.
* Two-group tests: `split_date` (historical; group 2 starts on that date) or `values` + `values2` (custom).
* `lag` for Ljung-Box (1 … 50, default 10).
* `data`: `test_name`, `hypotheses` (null/alternative), `n`, `statistic_name`, `statistic`, `df`, `p_value`,
  `alpha`, `decision`, `reject_null`, `confidence_interval`, `results[]` (one row per test), `groups[]`,
  `intervals[]`, `interpretation`, `assumptions[]`, `warnings[]`, `chart` (null distribution, Q-Q or ACF data).

### `POST /regression`
* `{"model": "returns_vs_dollar_index", "start_date": "2016-01-01", "end_date": "2025-12-31", "alpha": 0.05}`
* `{"model": "time_trend"}` or `{"model": "custom", "x": [1,2,3,4,5], "y": [2.1,3.9,6.2,8.1,9.8], "x_label": "x", "y_label": "y"}`
* `data`: `n`, `equation`, `correlation` (Pearson r with CI/t/p, Spearman ρ/p, strength), `coefficients[]`
  (estimate, SE, t, p, CI, robust SE/p/CI), `fit` (R², adjusted R², residual SE, F, p), `diagnostics[]`,
  `interpretation`, `warnings[]`, `chart` (points, fitted line, residuals; thinned to ≤ 1,500 points).

### CORS
Requests from `http://localhost:<port>` and `http://127.0.0.1:<port>` receive CORS headers; other origins do
not (browsers block them). The API listens on 127.0.0.1 only, so it is not reachable from other computers.

---

## 9. Using the R functions directly (RStudio console)

```r
source("backend/setup.R")                       # loads modules and data, no server
historical_summary()$volatility
describe_sample(historical_variable("log_return")$values)
a <- chart_analyze_file("tests/fixtures/btc_dark_candles.png")   # or your own screenshot
a$hypothesis$outlook$headline; a$descriptive$returns$kurtosis
run_hypothesis_test(list(test = "normality"))$results
run_regression(list(model = "returns_vs_dollar_index"))$coefficients
chart_store_list()                               # the Storage library (chart_analyze_file() does not save to it;
                                                  # only chart_analyze_request(), used by POST /chart/analyze, does)
```

---

## 10. Tests

```bat
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/run_tests.R
```
(In RStudio: `source("scripts/run_tests.R")`.) To skip the browser test: `set FXLAB_SKIP_BROWSER=1` first
(PowerShell: `$env:FXLAB_SKIP_BROWSER=1`).

| File | Covers |
|---|---|
| `test_chart_image.R` | reading the four fixture screenshots (dark/light/dense candles, line chart): candle count, OHLC error < 0.1 %, colours, volatility within 2 %, legend title and interval; the manual price range for a chart without its axis; rejected files; every analysis against base R (test formulas, `chisq.test`, `aov`, `lm`, `cor`); the outlook's reasoning on synthetic up/down/flat paths |
| `test_chart_storage.R` | saving on analyse, newest-first listing, image bytes match the upload, reload-as-current, delete (files removed, not just the index entry), clear, the oldest-first size cap, and id validation (path traversal rejected) |
| `test_statistics.R` | returns, mean/median/variance/SD/quantiles against known values, moments, missing/non-finite values, zero variance, rolling volatility, histograms; reproduces the original report (n = 2,496, mean, SD, skewness, kurtosis) |
| `test_distributions.R` | probabilities/quantiles vs R's d/p/q functions, exact discrete bounds, moments, reproducible samples, invalid parameters |
| `test_historical.R` | CSV validation rules, bundled-data quality figures, missing file, windows, upload/reset, alignment with the dollar index |
| `test_hypothesis.R` | t-tests vs `t.test`, CIs, JB/Shapiro-Wilk, Welch/F/Brown-Forsythe, Ljung-Box — reproducing the original report — and invalid input |
| `test_regression.R` | OLS vs `lm`/`cor.test`, dollar-index and trend regressions vs the report, constant/mismatched/short input |
| `test_api.R` | starts the real server: health → chart upload → current chart → image → reset; optional settings; invalid uploads (not an image, bad interval, incomplete price range, malformed JSON); every Storage endpoint over HTTP (list, image, load, delete, clear, invalid/missing id); the statistics endpoints; CSV upload/reset; 404; CORS; the web page; and a server with **no data files** |
| `test_frontend.R` | headless Chrome: before an upload every page asks for a chart and no page except Chart Upload has inputs; a real file upload through the file input → the KPIs, statistics and outlook equal R's values and the overlay draws one point per extracted candle; Chart Upload is part of the dashboard only and aligned on shared lines; each rail icon opens its page (Monte Carlo is gone); every page renders without JS errors (no percentile/tail/Q-Q tables on Descriptive Statistics, one card with a formula per test on Hypothesis Testing); the Report page contains every section and test, and its downloadable file is a complete document with embedded charts; the Storage page lists saved charts newest-first and its Open / Report / Delete / Clear all actions work end to end; a **true browser reload** restores the analysis *and* the Chart Upload panel's preview thumbnail (not just the KPIs); Clear removes the current analysis but not its saved copy in Storage; non-images and blank images give clear messages; phone layout; offline message. Skipped if Chrome/chromote is unavailable or unresponsive |
---

## 11. Troubleshooting

| Problem | Fix |
|---|---|
| Status bar says **"R backend offline"** | Start the backend (section 5) and click *Retry*. Check the console for errors. |
| `Missing R package(s): ...` | Run `scripts/install_packages.R` (it installs plumber, png, jpeg and tesseract). |
| "The price axis labels could not be read" | Upload a screenshot that includes the price axis, or enter the highest and lowest price in *Optional settings*. |
| "No candles or price line were found" | Upload a candlestick, line or area chart; very faint or custom colours may not be recognised. |
| Points in the Chart Data overlay are off the candles | Crop away pop-ups and drawings over the chart, keep the default candle colours, and upload a larger screenshot. |
| `Could not find the Chart Reader project folder` | Open `FXLab.Rproj` in RStudio, or run the command from the folder containing it. |
| Port 8000 already in use | Stop the other program, or set `FXLAB_PORT=8001` and open `http://127.0.0.1:8001/`. |
| Page opened from disk shows a red banner | Open `http://127.0.0.1:8000/` instead of double-clicking `index.html`. |
| "USD/INR data NOT AVAILABLE" | Restore `data/exchange_rates.csv` (run `scripts/prepare_data.R`) or upload a CSV. |
| Upload rejected | Read the message: it names the missing column, bad lines or conflicting dates. |
| `'Rscript' is not recognized` | Use the full path `"C:\Program Files\R\R-x.y.z\bin\Rscript.exe"` or `start_fxlab.bat`. |
| Browser test skipped/hangs | It needs Google Chrome (or Edge: `set CHROMOTE_CHROME=C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe`) and the `chromote` package. A fresh headless profile can take 15–90 s for its first page load on some Windows machines; if it hangs, close stray headless browsers (or restart Windows) and retry, or set `FXLAB_SKIP_BROWSER=1` to skip it. |

---

## 12. Known limitations

* Prices come from pixels: each is approximate to one pixel row, and tiny candles or very zoomed-out charts
  are read less precisely. Line charts are sampled every 6 pixels, not at their original data points.
* Up/down candles are recognised by colour (green-ish and red-ish hues); unusual custom colour schemes,
  hollow candles with very thin outlines, or heavy drawings over the candles can confuse the reader.
* Dates are not reconstructed: candles are numbered 1…n; the time-axis labels are only shown.
* The outlook is a statistical description under a random-walk model, not a forecast.
* The *current* analysis (what the Dashboard and other pages show right now) lives in the R process memory;
  restarting the backend clears it, though it is still in Storage if it was saved. The Storage library itself
  is a local folder (`data/charts/`), not a database: it is single-user, has no backup/sync, and a chart you
  delete (or that ages out past the `chart_store_max` limit) cannot be recovered.
* The background is a generated illustration, not a photograph. The glass effect needs a browser with
  `backdrop-filter` (current Chrome, Edge, Firefox, Safari); older browsers show plain translucent panels.
* The top-right round button shows the R backend status (it is not a user account).
* Headless Chrome captures can show canvases blank when `backdrop-filter` is used; real browsers draw them
  normally (the browser tests check the chart data, not screenshots).

---

## 13. Relationship to the original project

The earlier report-oriented analysis lives in
`C:\Projects\Pns Project\Currency_Exchange_Volatility_Project` (scripts `R/00`–`08`, report and viva notes)
and was **not modified**. Chart Reader includes its FRED data (still served by the API) and ports its tested helpers (Newey-West test,
Brown-Forsythe test, HC1 standard errors, Breusch-Pagan, Durbin-Watson). The test suite checks that Chart Reader
reproduces that report's key numbers for 2016–2025.
