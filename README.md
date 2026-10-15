# FXLab — Currency Exchange Rate and Volatility Analysis Using R

FXLab is a Probability and Statistics project that analyses the **USD/INR exchange rate** with a working
web application. You choose an analysis in the browser; the page sends the request to an **R backend**
(an R Plumber API); **R performs every statistical calculation** and returns JSON; the page shows the
results as tables and charts. JavaScript only handles the interface — it never computes statistics.

```
Browser (HTML/CSS/JS, Chart.js)  --JSON request-->  R Plumber API (backend/api.R)
        ^                                                   |  validates input
        |                                                   v
        +--------- JSON results ------  R statistical modules (backend/*.R)  <--  data/*.csv (FRED)
```

---

## 1. Features

| Section | What R computes |
|---|---|
| **Exchange Rate Dashboard** (with its **Simulation Parameters** panel) | GBM simulation from the panel's inputs: four KPI cards (latest USD/INR rate with its daily change, mean simulated rate, simulated daily volatility, expected total return), up to 500 simulated paths with the mean path, the distribution of **all** simulated daily returns with a normal curve, summary statistics, a ±2σ normality check and Jarque–Bera test, and a Monte Carlo panel (5–95% band, sample paths, lowest/highest/mean final rate) |
| **Historical Analysis** | Levels, daily log returns, 20-day rolling volatility, calendar-year table, largest moves, full data-validation report; **CSV upload** |
| **Descriptive Statistics** | n, mean, median, min/max, range, sample & population variance, SD, SE, quartiles/IQR, percentiles, skewness, kurtosis, volatility, histogram + fitted normal, Q-Q plot, tail check — for historical data or numbers you type |
| **Probability Distributions** | Normal, binomial, Poisson, uniform, exponential: PDF/PMF, CDF, quantiles, P(X ≤ a), P(X > a), P(a ≤ X ≤ b), moments, and a random sample compared with theory |
| **Monte Carlo Simulation** | A detailed analysis of the **current** simulation (inputs come from the Simulation Parameters panel on the Dashboard; this page has no copy of that form): percentile fan chart, terminal distribution vs exact log-normal theory, E[S_T] with 95% CI, convergence plot, per-path statistics. Its own options: how many paths to draw, and a threshold rate for which R re-runs the same simulation (same inputs and seed) to give P(S_T > K), P(S_T < K) and the probability of reaching K (Wilson CIs) |
| **Hypothesis Testing** | One-sample t-test (+ Newey-West robust check), confidence intervals (mean, variance, SD), normality (Shapiro-Wilk, Jarque-Bera), Welch two-sample t-test, F-test + Brown-Forsythe, Ljung-Box — with hypotheses, assumptions and warnings |
| **Correlation & Regression** | USD/INR returns vs Broad U.S. Dollar Index returns (a real second FRED series), a time-trend regression, or your own (x, y) data: Pearson/Spearman, OLS with CIs, HC1 robust SEs, R², F-test, residual diagnostics |
| **About Project** | Methods, formulas, data source, assumptions and limitations |

Three simulation models share the same GBM drift/volatility: normal shocks (classic GBM), Student-t
shocks (heavy tails), and shocks bootstrapped from standardised historical USD/INR returns.

**Interface.** The design follows `frontend/assets/fxlab-reference.png`: a blurred mountain-at-sunset scene,
frosted-glass panels, a floating circular navigation rail and a
**Simulation Parameters** panel on the Exchange Rate Dashboard. The rail has one icon per page (Dashboard,
Distribution Analysis, Monte Carlo, Hypothesis Testing, Correlation & Regression, Historical Data, About);
each icon simply opens its page. Simulation Parameters is a fixed part of the Dashboard page, not a pop-up:
it has no open/close button and does not appear on other pages. On desktop (1200 px and wider) it is the
Dashboard's left column, laid out on shared lines: its top is level with the first rail icon and the
"Explore · Analyze · Simulate" line, and its bottom with the Distribution / Monte Carlo panels. On tablets
and phones it appears in the page under the title. It is the one place
where simulation inputs are entered; the Dashboard and the Monte Carlo page both show its result. On phones
the rail becomes a bottom bar.
Charts can be downloaded as PNG and expanded to full screen. The latest rate is always labelled with its date
and is never presented as a live quote.

---

## 2. Technology stack

* **R 4.x** (developed with R 4.6.1) — all statistics; RStudio project `FXLab.Rproj`.
* **plumber** — HTTP API (also serves the web page). **jsonlite** — JSON.
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
│   ├── historical_analysis.R  CSV loading + validation, dataset state, historical summaries
│   ├── distributions.R        probability distributions
│   ├── simulation.R           GBM and Monte Carlo
│   ├── hypothesis_testing.R   tests and confidence intervals
│   └── regression_analysis.R  correlation and regression
├── frontend/
│   ├── index.html             the single-page interface: top bar, rail, dashboard (with its parameters panel), analysis pages
│   ├── style.css              glass theme; design tokens at the top (colours, glass, layout)
│   ├── js/config.js           API base URL (the one place to change it)
│   ├── js/api.js              API client (errors, timeouts, no duplicate requests)
│   ├── js/ui.js, charts.js, format.js   shared helpers (display only)
│   ├── js/sim-params.js       the Dashboard's Simulation Parameters panel (defaults, Run, Reset)
│   ├── js/app.js              navigation, rail, backend status, currency popover, clock
│   ├── js/sections/*.js       one file per page (dashboard.js = the main dashboard)
│   └── assets/                background.svg, icons.svg (Lucide sprite), fonts/ (Inter),
│                              vendor/chart.umd.min.js (Chart.js), fxlab-reference.png (design reference)
├── data/
│   ├── exchange_rates.csv     USD/INR, Date,USDINR (FRED DEXINUS, 1973-01-02 to 2026-10-09)
│   ├── dollar_index.csv       Date,DollarIndex (FRED DTWEXBGS, 2006-01-02 to 2026-10-09)
│   └── raw/                   untouched FRED downloads + download_log.csv (time, MD5)
├── scripts/
│   ├── install_packages.R     installs the R packages
│   ├── prepare_data.R         rebuilds data/*.csv from data/raw (optionally re-downloads FRED)
│   └── run_tests.R            runs all tests
└── tests/                     testthat tests (statistics, distributions, simulation,
                               historical data, hypothesis tests, regression, API, browser)
```

---

## 4. Installation (once)

1. Install **R** (4.2 or newer) from <https://cran.r-project.org/>. RStudio is optional but recommended.
2. Install the packages. In a terminal opened in the project folder:

   ```bat
   "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/install_packages.R
   ```

   or in the RStudio console: `source("scripts/install_packages.R")`
   (minimum to run the app: `install.packages("plumber")`).

> If `Rscript` is on your PATH you can type `Rscript` instead of the full path. Adjust `R-4.6.1` to your
> installed version (look in `C:\Program Files\R\`).

---

## 5. Running FXLab

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

## 6. Data

* **USD/INR:** FRED series [DEXINUS](https://fred.stlouisfed.org/series/DEXINUS) — Indian rupees per one
  U.S. dollar, noon buying rates in New York (Board of Governors of the Federal Reserve System, H.10).
  A rise means the rupee weakened.
* **Broad dollar strength:** FRED series [DTWEXBGS](https://fred.stlouisfed.org/series/DTWEXBGS) — Nominal
  Broad U.S. Dollar Index (Jan 2006 = 100), used as the second variable for correlation and regression.
* The raw files were copied unchanged from the original project
  (`C:\Projects\Pns Project\Currency_Exchange_Volatility_Project\data\raw`, retrieved 2026-10-14; checksums in
  `data/raw/download_log.csv`). `scripts/prepare_data.R` only renames the columns.
* **No live rates.** The app never fetches data from the internet; the latest value is always shown with its
  date and the note "not a live quote". To refresh, set `REFRESH_FROM_FRED <- TRUE` in
  `scripts/prepare_data.R`, run it, and restart the backend.
* Default analysis window: **2016-01-01 to the last observation**. Set the end date to 2025-12-31 to
  reproduce the original report's numbers exactly (the tests check this).

### CSV format (bundled file or your upload)

```
Date,USDINR
2026-10-08,96.78
2026-10-09,96.73
```

* `Date` in ISO format `YYYY-MM-DD`; `USDINR` a positive number (INR per USD). Header row required.
* A blank value, `NA` or `.` means no rate was published that day (e.g. a holiday). These rows are
  **counted and excluded — never filled in**. Returns are measured between consecutive valid observations.
* FRED downloads (`observation_date,DEXINUS`) are accepted unchanged. Extra columns are ignored (and reported).
* Validation (all reported in Historical Analysis → *Data validation report*):
  missing columns → rejected; invalid dates, non-numeric or non-positive rates → excluded with line numbers
  (the whole file is rejected if they exceed 5% of rows); exact duplicate dates → removed; conflicting
  duplicates → rejected; unsorted rows → sorted; fewer than 10 valid rows → rejected; one-day moves above
  10% → flagged (kept). Upload limit 5 MB / 100,000 rows.
* **Upload** in Historical Analysis → *Use your own data*. The uploaded file replaces the bundled data for
  every analysis until you click *Use bundled FRED data* (it is kept in memory only, never written to disk).
* If `data/exchange_rates.csv` is missing, the app still starts: the status bar says so, data-based sections
  explain how to add the file, and simulations/distributions keep working with clearly labelled example inputs.

---

## 7. Statistical formulas and assumptions

| Quantity | Definition used in R |
|---|---|
| Simple return | R_t = 100 · (S_t / S_(t−1) − 1) % |
| Log return | r_t = 100 · ln(S_t / S_(t−1)) % (used for volatility, tests and simulation estimates) |
| Cumulative return | 100 · (S_t / S_0 − 1) % |
| Sample variance / SD | s² = Σ(x − x̄)² / (n − 1) (`var`, `sd`); population variance (÷ n) also shown |
| Quantiles | `quantile(type = 7)` (R's default) |
| Skewness | g₁ = m₃ / m₂^(3/2), m_k = mean((x − x̄)^k) |
| Kurtosis | b₂ = m₄ / m₂² (normal = 3); excess kurtosis = b₂ − 3 |
| Volatility | daily SD of log returns; annualised = daily SD × √252 |
| Rolling volatility | SD of the last 20 daily log returns × √252 |
| GBM | S_(t+Δt) = S_t · exp[(μ − σ²/2)Δt + σ√Δt · Z_t], Δt = 1 trading day |
| GBM theory | ln(S_T/S_0) ~ N((μ − σ²/2)T, σ²T); E[S_T] = S_0·e^(μT); median = S_0·e^((μ−σ²/2)T) |
| Default μ, σ | σ̂ = SD of daily log returns; μ̂ = mean log return + σ̂²/2 (so GBM's expected log return equals the sample mean) |
| Monte Carlo SE | SD(S_T) / √N; probability CIs by the Wilson score method |
| t-interval | x̄ ± t(1−α/2, n−1) · s/√n |
| Variance interval | [(n−1)s² / χ²(1−α/2), (n−1)s² / χ²(α/2)] (exact only for normal data) |
| Jarque-Bera | JB = n/6 · (g₁² + (b₂ − 3)²/4) ~ χ²(2) under normality |
| Newey-West | HAC standard error, Bartlett weights, lag = ⌊4(n/100)^(2/9)⌋ |
| Regression | OLS `lm(y ~ x)`; HC1 robust SEs; Durbin-Watson; Breusch-Pagan (Koenker) |

**Units.** In the simulation forms μ and σ are **daily** percentages (e.g. μ = 0.0144 % per trading day,
σ = 0.311 % per trading day). R reports the annualised equivalents (μ×252, σ×√252) for context only.

**Assumptions and caveats** (also shown on the About page and next to each result):
* GBM assumes constant drift and volatility and independent shocks. Real USD/INR returns are heavy-tailed
  (excess kurtosis ≈ 4) and show volatility clustering; the Reserve Bank of India intervenes. Simulations
  illustrate the model — they are not forecasts.
* With Student-t shocks E[S_T] is mathematically infinite, so only the median/percentiles and the
  log-return moments are compared with theory.
* "Fail to reject H₀" is never presented as proof that H₀ is true.
* Regressing the rate *level* on time is descriptive only (non-stationary, autocorrelated residuals).

### Example simulation inputs

| Scenario | S₀ | μ (%/day) | σ (%/day) | Days | Paths |
|---|---|---|---|---|---|
| Historical estimate (form default) | last observation (96.73) | ≈ 0.0144 | ≈ 0.311 | 180 | 500 |
| Calm market | 96.73 | 0.01 | 0.15 | 126 | 5,000 |
| Stressed market | 96.73 | 0.02 | 0.60 | 252 | 5,000 |

---

## 8. API reference

All responses share one envelope:

```json
{ "success": true,  "data": { ... }, "message": "Simulation completed", "error": null }
{ "success": false, "data": null, "message": "'days' must be at least 1 and at most 1000 (received 0).",
  "error": { "code": "VALIDATION_ERROR", "field": "days", "status": 400 } }
```

| Status | Code(s) | Meaning |
|---|---|---|
| 200 | — | success |
| 400 | `VALIDATION_ERROR`, `MALFORMED_JSON` | a value is missing/invalid, or the body is not a JSON object |
| 404 | `DATA_UNAVAILABLE`, `NOT_FOUND` | data file missing, or unknown route |
| 413 | `PAYLOAD_TOO_LARGE` | upload above 5 MB |
| 422 | `INVALID_DATA`, `INSUFFICIENT_DATA`, `DEGENERATE_DATA` | the data cannot support the analysis (bad CSV, too few values, zero variance) |
| 500 | `INTERNAL_ERROR` | unexpected; details only in the R console, never in the response |

POST bodies are JSON (`Content-Type: application/json`). Numbers must be JSON numbers. Optional fields can
be omitted. Dates are `YYYY-MM-DD`. Custom samples (`values`, `x`, `y`) may be a JSON array or a text string
of numbers separated by commas/spaces/new lines (max 20,000).

### `GET /health`
`data`: `status` ("ok"), `api_version`, `r_version`, `plumber_version`, `server_time`,
`statistical_engine` ("R"), `datasets.usdinr` / `datasets.dollar_index` (`available`, `kind`, `observations`,
`first_date`, `last_date`, or `message` when unavailable), `uploaded_dataset_active`.

### `GET /defaults`
`data.simulation`: `start_rate`, `mu_daily_pct`, `sigma_daily_pct` (estimated from data, `source`
"historical", or labelled `"example"` without data), `description`, `as_of`, `threshold`, `days`,
`n_paths`, `n_display_paths`, `seed`, `model`, `t_df`. Also `simulation_models`, `limits`,
`distributions` (parameter catalogue), `hypothesis_tests`, `regression_models`, `alpha_choices`.

### `POST /simulate`
| Field | Type | Rule |
|---|---|---|
| `start_rate` | number | > 0 (INR per USD) — required |
| `mu_daily_pct` | number | −5 … 5 (% per trading day) — required |
| `sigma_daily_pct` | number | 0 … 20 (% per trading day) — required |
| `days` | integer | 1 … 1000 — required |
| `n_paths` | integer | 1 … 10,000, and days × n_paths ≤ 5,000,000 — required |
| `model` | string | `gbm_normal` (default), `gbm_student_t`, `gbm_bootstrap` |
| `t_df` | number | 3 … 100 (Student-t model; default 5) |
| `seed` | integer | optional, 1 … 2147483647; generated and returned if omitted |
| `threshold` | number | optional, > 0 |
| `n_display_paths` | integer | 1 … 500 (paths returned for charts; default 20). R returns at most 100,000 path values in total (paths × (days + 1)) and reports `paths.capped` |

Example: `{"start_rate": 96.73, "mu_daily_pct": 0.0144, "sigma_daily_pct": 0.311, "days": 252, "n_paths": 2000, "seed": 2026, "threshold": 101.57}`

`data`: `inputs` (+ annualised equivalents), `seed`, `seed_source`, `terminal` (`mean`, `median`, `sd`,
`min`, `max`, `mean_standard_error`, `mean_ci95`, `percentiles[]`, log-return mean/SD, `prob_above_start`),
`theory` (exact log-normal values for `gbm_normal`), `threshold` (`prob_above`, `prob_below`, `prob_touch` with
95% CIs, `theory_prob_above`), `terminal_histogram[]`, `bands` (`day`, `p05`, `p25`, `p50`, `p75`, `p95`,
`mean` and `cross_sectional_sd` per day across **all** paths), `paths` (`day`, `n_requested`, `n_returned`,
`capped`, `values[[...]]` for the displayed paths), `path_statistics[]`, `convergence` (running mean ± 95% band),
`all_positive`, and `daily_returns` — every simulated daily log return (all paths × all days) summarised
in R: `n`, `mean_pct`, `sd_pct`, `variance` (%²), `skewness`, `kurtosis`, `min_pct`, `max_pct`, `histogram[]`
(counts with the expected normal counts), `within_2sd` (share inside the model mean ± 2σ vs 95.45% for a
normal) and `jarque_bera` (statistic, p-value, decision). `terminal` also has `mean_return_pct`,
`median_return_pct`, `lowest_point` and `highest_point`.

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
simulate_exchange_rate(96.73, 0.0144, 0.311, days = 252, n_paths = 5000, seed = 1, threshold = 100)$terminal
run_hypothesis_test(list(test = "normality"))$results
run_regression(list(model = "returns_vs_dollar_index"))$coefficients
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
| `test_statistics.R` | returns, mean/median/variance/SD/quantiles against known values, moments, missing/non-finite values, zero variance, rolling volatility, histograms; reproduces the original report (n = 2,496, mean, SD, skewness, kurtosis) |
| `test_distributions.R` | probabilities/quantiles vs R's d/p/q functions, exact discrete bounds, moments, reproducible samples, invalid parameters |
| `test_simulation.R` | output dimensions, positivity, initial rate, seed reproducibility, summaries over **all** paths (re-simulated independently), agreement with log-normal theory, zero volatility, other shock models, every input limit |
| `test_historical.R` | CSV validation rules, bundled-data quality figures, missing file, windows, upload/reset, alignment with the dollar index |
| `test_hypothesis.R` | t-tests vs `t.test`, CIs, JB/Shapiro-Wilk, Welch/F/Brown-Forsythe, Ljung-Box — reproducing the original report — and invalid input |
| `test_regression.R` | OLS vs `lm`/`cor.test`, dollar-index and trend regressions vs the report, constant/mismatched/short input |
| `test_api.R` | starts the real server: health → defaults → simulate → invalid input; malformed JSON; limits; every endpoint; upload/reset; 404; CORS; the web page; and a server with **no data files** |
| `test_frontend.R` | headless Chrome: the dashboard's KPIs, Monte Carlo cards and statistics equal the API response for the same request; Simulation Parameters is part of the Dashboard only (no gear in the rail, no close button, Esc/clicks do not hide or move it); each rail icon (real mouse clicks) opens its own page; Run Simulation and Reset; the Monte Carlo page uses the current simulation (paths drawn, threshold probabilities from R); validation messages; every page renders without JS errors; desktop alignment (shared top and bottom lines, one-line subtitle); phone layout (panel in the page flow); offline message. Skipped if Chrome/chromote is unavailable or unresponsive |

---

## 11. Troubleshooting

| Problem | Fix |
|---|---|
| Status bar says **"R backend offline"** | Start the backend (section 5) and click *Retry*. Check the console for errors. |
| `Missing R package(s): plumber` | Run `scripts/install_packages.R` or `install.packages("plumber")`. |
| `Could not find the FXLab project folder` | Open `FXLab.Rproj` in RStudio, or run the command from the folder containing it. |
| Port 8000 already in use | Stop the other program, or set `FXLAB_PORT=8001` and open `http://127.0.0.1:8001/`. |
| Page opened from disk shows a red banner | Open `http://127.0.0.1:8000/` instead of double-clicking `index.html`. |
| "USD/INR data NOT AVAILABLE" | Restore `data/exchange_rates.csv` (run `scripts/prepare_data.R`) or upload a CSV. |
| Upload rejected | Read the message: it names the missing column, bad lines or conflicting dates. |
| Simulation rejected | Keep days ≤ 1000, paths ≤ 10,000 and days × paths ≤ 5,000,000. |
| `'Rscript' is not recognized` | Use the full path `"C:\Program Files\R\R-x.y.z\bin\Rscript.exe"` or `start_fxlab.bat`. |
| Browser test skipped/hangs | It needs Google Chrome (or Edge: `set CHROMOTE_CHROME=C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe`) and the `chromote` package. A fresh headless profile can take 15–90 s for its first page load on some Windows machines; if it hangs, close stray headless browsers (or restart Windows) and retry, or set `FXLAB_SKIP_BROWSER=1` to skip it. |

---

## 12. Known limitations

* Data are a stored FRED snapshot (to 2026-10-09); there is no live feed by design.
* GBM (constant μ and σ, independent shocks) ignores volatility clustering, regime changes and central-bank
  intervention; the Student-t and bootstrap models address tails only, not clustering.
* Annualisation by √252 assumes uncorrelated daily returns.
* An uploaded dataset lives in the R process memory: restarting the backend returns to the bundled data.
  The API is single-user (one R process); it is meant to run locally.
* Charts show at most 3,000 historical points / 1,500 scatter points (evenly thinned); all statistics use the
  full data.
* The dashboard draws up to 500 simulated paths (at most 100,000 path values), so its default response is
  about 0.8 MB; fine on the local machine, but it is the largest request the app makes.
* The background is a generated illustration, not a photograph. The glass effect needs a browser with
  `backdrop-filter` (current Chrome, Edge, Firefox, Safari); older browsers show plain translucent panels.
* The top-right round button shows the R backend status (it is not a user account), and the currency
  selector only offers USD/INR, the one pair the project analyses.
* Headless Chrome captures can show canvases blank when `backdrop-filter` is used; real browsers draw them
  normally (the browser tests check the chart data, not screenshots).

---

## 13. Relationship to the original project

The earlier report-oriented analysis lives in
`C:\Projects\Pns Project\Currency_Exchange_Volatility_Project` (scripts `R/00`–`08`, report and viva notes)
and was **not modified**. FXLab reuses its FRED data and ports its tested helpers (Newey-West test,
Brown-Forsythe test, HC1 standard errors, Breusch-Pagan, Durbin-Watson). The test suite checks that FXLab
reproduces that report's key numbers for 2016–2025.
