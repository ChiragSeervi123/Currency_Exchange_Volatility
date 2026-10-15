/* =============================================================================
   dashboard.js - the main dashboard (reference composition): KPI cards,
   simulated exchange-rate paths, distribution of simulated daily returns and
   the Monte Carlo panel. Data: POST /simulate (sent by the Simulation Parameters panel) and
   GET /historical/summary. Every number shown here was computed by R; this
   file only formats and draws it.
   ============================================================================= */
(function () {
  // Set at load time: results can arrive (e.g. from the Monte Carlo page) before this page is opened.
  var ui = FX.ui, fmt = FX.fmt;
  var state = { busy: false, sim: null, request: null, histVersion: -1 };

  function $(sel) { return document.querySelector(sel); }
  function rupee(x, d) { return typeof x === 'number' && isFinite(x) ? '₹' + fmt.num(x, d == null ? 2 : d) : '—'; }
  function signedPct(x, d) { return typeof x === 'number' && isFinite(x) ? (x >= 0 ? '+' : '') + fmt.num(x, d == null ? 2 : d) + '%' : '—'; }

  function setChartState(canvasId, text, kind) {
    var s = document.querySelector('[data-state-for="' + canvasId + '"]');
    if (!s) return;
    if (!text) { s.classList.add('is-hidden'); return; }
    s.textContent = text;
    s.classList.remove('is-hidden');
    s.classList.toggle('is-error', kind === 'error');
  }

  function setKpi(id, opts) {
    var card = document.getElementById(id);
    var value = card.querySelector('[data-kpi="value"]');
    value.textContent = opts.value;
    value.className = 'kpi-value' + (opts.valueClass ? ' ' + opts.valueClass : '');
    var sub = card.querySelector('[data-kpi="sub"]');
    sub.replaceChildren.apply(sub, [].concat(opts.sub || []).map(function (n) { return typeof n === 'string' ? document.createTextNode(n) : n; }));
    FX.charts.sparkline(card.querySelector('[data-kpi="spark"]'), opts.spark, opts.sparkColor);
    if (opts.title) card.setAttribute('title', opts.title);
  }

  // ---- KPI 1: latest historical rate (GET /historical/summary) ------------------------------
  function loadHistorical() {
    state.histVersion = FX.dataVersion;
    return FX.api.get('/historical/summary').then(function (s) {
      var l = s.latest, up = l.change >= 0;
      var date = new Date(l.date + 'T00:00:00');
      var when = date.toLocaleDateString('en-GB', { day: '2-digit', month: 'short', year: 'numeric' });
      setKpi('kpi-rate', {
        value: rupee(l.rate),
        sub: [ui.el('span', { class: up ? 'up' : 'down', text: (up ? '▲ ' : '▼ ') + fmt.num(Math.abs(l.change), 2) +
               ' (' + fmt.num(Math.abs(l.change_pct), 2) + '%)' })],
        spark: s.recent.rate, sparkColor: up ? '#4ade80' : '#ff7a85',
        title: 'Last observation in the dataset (' + l.date + ', ' + (s.dataset.kind === 'uploaded' ? 'uploaded file' : 'FRED DEXINUS') +
               '). Change versus the previous observation (' + l.previous_date + '). This is not a live quote.'
      });
      state.latestNote = 'Latest rate: ' + (s.dataset.kind === 'uploaded' ? 'uploaded file' : 'FRED DEXINUS') + ', ' + when +
        ' (stored data, not a live quote).';
      renderFootnote();
      var note = document.getElementById('pair-data-note');
      if (note) note.textContent = 'Data: ' + (s.dataset.kind === 'uploaded' ? 'your uploaded file' : 'FRED series DEXINUS') +
        ', last observation ' + l.date + ' (stored locally, not a live feed).';
    }).catch(function (err) {
      state.histVersion = -1;
      setKpi('kpi-rate', { value: '—', sub: 'Historical data unavailable: ' + err.message });
    });
  }

  // ---- Simulation results ------------------------------------------------------------------
  // Options for "how many of the returned paths to draw"; keeps the user's choice when still possible.
  function pathOptions(select, n, preferred, labelFn) {
    var options = [10, 20, 25, 50, 100, 200].filter(function (k) { return k < n; }).concat([n]);
    var current = Number(select.value);
    select.replaceChildren();
    options.forEach(function (k) { select.appendChild(ui.el('option', { value: k, text: labelFn(k, k === n) })); });
    var pick = options.indexOf(current) >= 0 ? current : (options.indexOf(preferred) >= 0 ? preferred : n);
    select.value = String(pick);
    select.disabled = false;
  }

  function renderPaths() {
    var d = state.sim, c = FX.charts.colors();
    var k = Number($('#paths-select').value) || d.paths.values.length;
    var sets = d.paths.values.slice(0, k).map(function (vals, i) {
      return { label: 'Path ' + (i + 1), data: FX.charts.xy(d.paths.day, vals), borderColor: c.path,
               borderWidth: 1, excludeFromTable: true, isPath: true };
    });
    sets.push({ label: 'Mean path (all ' + fmt.int(d.inputs.n_paths) + ' paths)', data: FX.charts.xy(d.bands.day, d.bands.mean),
                borderColor: c.s1, borderWidth: 2.6, order: -1 });
    FX.charts.render('chart-paths', {
      type: 'line', data: { datasets: sets },
      options: {
        parsing: false, normalized: true, spanGaps: true,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: { filter: function (item) { return !item.dataset.isPath; },
                     callbacks: { title: function (items) { return 'Day ' + items[0].parsed.x; },
                                  label: function (item) { return 'Mean path: ' + rupee(item.parsed.y, 4); } } }
        },
        scales: {
          x: FX.charts.axis('Days', { extra: { min: 0, max: d.inputs.days } }),
          y: FX.charts.axis('Exchange Rate (₹ per $)', { maxTicks: 6 })
        }
      }
    });
  }

  function renderDistribution() {
    var r = state.sim.daily_returns, c = FX.charts.colors();
    var h = FX.charts.columns(r.histogram);
    // Show the part of R's histogram grid that contains simulated returns.
    var first = h.count.findIndex(function (v) { return v > 0; });
    var last = h.count.length - 1 - h.count.slice().reverse().findIndex(function (v) { return v > 0; });
    if (first < 0) { first = 0; last = h.count.length - 1; }
    var mids = h.mid.slice(first, last + 1);
    var sets = [{
      type: 'bar', label: 'Simulated returns (count)', data: FX.charts.xy(mids, h.count.slice(first, last + 1)),
      backgroundColor: FX.charts.alpha(c.s1, 0.85), borderColor: 'rgba(170, 200, 255, 0.55)', borderWidth: 1,
      barPercentage: 1, categoryPercentage: 1, order: 2,
      binLower: h.lower.slice(first, last + 1), binUpper: h.upper.slice(first, last + 1)
    }];
    if (h.normal_expected_count && h.normal_expected_count[0] != null) {
      sets.push({ type: 'line', label: 'Normal distribution (expected count)',
                  data: FX.charts.xy(mids, h.normal_expected_count.slice(first, last + 1)),
                  borderColor: c.s2, borderWidth: 2, pointRadius: 0, tension: 0.35, order: 1 });
    }
    FX.charts.render('chart-dist', {
      type: 'bar', data: { datasets: sets },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: { callbacks: {
            title: function (items) {
              var ds = items[0].dataset.binLower ? items[0].dataset : sets[0], i = items[0].dataIndex;
              return fmt.num(ds.binLower[i], 3) + '% to ' + fmt.num(ds.binUpper[i], 3) + '%';
            },
            label: function (item) { return item.dataset.label + ': ' + fmt.int(item.parsed.y); }
          } }
        },
        scales: {
          x: FX.charts.axis('Daily Return (%)', { maxTicks: 9, extra: { offset: false } }),
          y: FX.charts.axis('Frequency', { maxTicks: 6, extra: { beginAtZero: true } })
        }
      }
    });

    var box = $('#dist-stats');
    box.querySelector('[data-stat="mean"]').textContent = fmt.num(r.mean_pct, 3) + '%';
    box.querySelector('[data-stat="sd"]').textContent = fmt.num(r.sd_pct, 3) + '%';
    box.querySelector('[data-stat="variance"]').textContent = fmt.sig(r.variance, 3);
    box.querySelector('[data-stat="skewness"]').textContent = fmt.num(r.skewness, 3);
    box.querySelector('[data-stat="kurtosis"]').textContent = fmt.num(r.kurtosis, 3);
    box.title = 'Computed in R from all ' + fmt.int(r.n) + ' simulated daily log returns (' + r.unit + '). Variance in %².';

    var w = r.within_2sd, jb = r.jarque_bera, norm = $('#dist-normality');
    var rejected = typeof jb.p_value === 'number' && jb.p_value < 0.05;
    norm.classList.toggle('stat-box-good', !rejected);
    norm.classList.toggle('stat-box-warn', rejected);
    norm.querySelector('[data-stat="range"]').textContent = 'P(' + fmt.num(w.lower_pct, 2) + '% < R < ' + fmt.num(w.upper_pct, 2) + '%)';
    norm.querySelector('[data-stat="share"]').textContent = fmt.prob(w.share, 1);
    var jbText = typeof jb.p_value !== 'number' ? 'n/a' : jb.p_value < 0.001 ? '< 0.001' : jb.p_value.toFixed(2);
    norm.querySelector('[data-stat="note"]').textContent = 'Normal ' + fmt.prob(w.normal_share, 1) + ' · JB p ' +
      (jbText.charAt(0) === '<' ? jbText : '= ' + jbText);
    norm.title = 'Share of simulated daily returns within the model mean ± 2σ, compared with a normal distribution. ' +
      jb.decision + '.';
  }

  function renderMonteCarlo() {
    var d = state.sim, c = FX.charts.colors();
    var k = Number($('#mc-paths-select').value) || Math.min(50, d.paths.values.length);
    var days = d.bands.day;
    var sets = [
      { label: '5th percentile', data: FX.charts.xy(days, d.bands.p05), borderWidth: 0, fill: false, excludeFromTable: false, isBand: true },
      { label: '5th–95th percentile band', data: FX.charts.xy(days, d.bands.p95), borderWidth: 0,
        backgroundColor: c.s1softer, fill: '-1', isBand: true }
    ];
    d.paths.values.slice(0, k).forEach(function (vals, i) {
      sets.push({ label: 'Path ' + (i + 1), data: FX.charts.xy(d.paths.day, vals), borderColor: c.path, borderWidth: 1,
                  excludeFromTable: true, isPath: true });
    });
    sets.push({ label: 'Mean path', data: FX.charts.xy(days, d.bands.mean), borderColor: c.s1, borderWidth: 2.4, order: -1 });
    FX.charts.render('chart-mc', {
      type: 'line', data: { datasets: sets },
      options: {
        parsing: false, normalized: true,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: { filter: function (item) { return !item.dataset.isPath; },
                     callbacks: { title: function (items) { return 'Day ' + items[0].parsed.x; },
                                  label: function (item) {
                                    var name = item.dataset.label === '5th–95th percentile band' ? '95th percentile' : item.dataset.label;
                                    return name + ': ' + rupee(item.parsed.y, 2);
                                  } } }
        },
        scales: {
          x: FX.charts.axis('Days', { extra: { min: 0, max: d.inputs.days } }),
          y: FX.charts.axis('Exchange Rate (₹ per $)', { maxTicks: 6 })
        }
      }
    });
    var t = d.terminal;
    $('[data-mc="low"]').textContent = rupee(t.min);
    $('[data-mc="high"]').textContent = rupee(t.max);
    $('[data-mc="mean"]').textContent = rupee(t.mean);
    $('[data-mc="low"]').parentNode.title = 'Lowest final rate of all ' + fmt.int(t.n_paths) + ' paths (lowest point reached on any day: ' + rupee(t.lowest_point) + ').';
    $('[data-mc="high"]').parentNode.title = 'Highest final rate of all ' + fmt.int(t.n_paths) + ' paths (highest point reached on any day: ' + rupee(t.highest_point) + ').';
    $('[data-mc="mean"]').parentNode.title = 'Mean final rate E[S_T] over all paths; 95% CI ' + rupee(t.mean_ci95[0]) + ' – ' + rupee(t.mean_ci95[1]) + '.';
  }

  function renderKpis() {
    var d = state.sim, t = d.terminal, r = d.daily_returns, inp = d.inputs;
    setKpi('kpi-mean', {
      value: rupee(t.mean),
      sub: fmt.int(inp.n_paths) + ' paths',
      spark: d.bands.mean, sparkColor: '#5b9bff',
      title: 'Mean simulated rate on the final day (E[S_T]); 95% CI ' + rupee(t.mean_ci95[0]) + ' – ' + rupee(t.mean_ci95[1]) +
             '. Trend line: the mean path.'
    });
    setKpi('kpi-vol', {
      value: fmt.num(r.sd_pct, 2) + '%',
      sub: 'Daily, simulated',
      spark: d.bands.cross_sectional_sd, sparkColor: '#a78bfa',
      title: 'Standard deviation of all simulated daily log returns. Trend line: spread (SD) of the simulated rates across paths, day by day.'
    });
    var card = document.getElementById('kpi-return');
    card.querySelector('[data-kpi="period"]').textContent = '(' + fmt.int(inp.days) + ' Days)';
    setKpi('kpi-return', {
      value: signedPct(t.mean_return_pct),
      valueClass: t.mean_return_pct >= 0 ? 'is-positive' : 'is-negative',
      sub: 'Median ' + signedPct(t.median_return_pct),
      spark: d.bands.p50, sparkColor: t.mean_return_pct >= 0 ? '#4ade80' : '#ff7a85',
      title: 'Expected total return E[S_T]/S_0 − 1 over ' + inp.days + ' trading days. Trend line: the median path.'
    });
  }

  function render() {
    var d = state.sim;
    var n = d.paths.values.length;
    pathOptions($('#paths-select'), n, n, function (k, all) {
      return all ? (d.paths.capped ? 'All returned (' + k + ')' : 'All Paths (' + k + ')') : k + ' Paths';
    });
    pathOptions($('#mc-paths-select'), n, Math.min(50, n), function (k) { return 'Show ' + k + ' Paths'; });
    renderKpis();
    renderPaths();
    renderDistribution();
    renderMonteCarlo();
    ['chart-paths', 'chart-dist', 'chart-mc'].forEach(function (id) {
      setChartState(id, null);
      document.querySelector('[data-download="' + id + '"]').disabled = false;
    });
    renderFootnote();
  }

  function renderFootnote() {
    var d = state.sim, parts = [];
    if (d) {
      var inp = d.inputs;
      parts.push('Simulation computed in R: ' + fmt.int(inp.n_paths) + ' paths × ' + fmt.int(inp.days) +
        ' trading days · ' + FX.simForm.modelLabel(FX.lastDefaults || {}, inp.model) + ' · S₀ = ₹' + fmt.num(inp.start_rate, 2) +
        ', μ = ' + fmt.num(inp.mu_daily_pct, 4) + '%/day, σ = ' + fmt.num(inp.sigma_daily_pct, 4) + '%/day · seed ' + d.seed +
        (d.paths.capped ? ' · ' + d.paths.n_returned + ' of ' + d.paths.n_requested + ' paths drawn (display limit)' : '') +
        '. Daily-return statistics use all ' + fmt.int(d.daily_returns.n) + ' simulated returns.');
    }
    if (state.latestNote) parts.push(state.latestNote);
    $('#dash-footnote').textContent = parts.join(' ') || 'All values are computed in R. Rates in INR per USD.';
  }

  // ---- The current simulation (shared by the Dashboard and the Monte Carlo page) --------------
  // There is ONE set of simulation inputs: the Simulation Parameters panel on this page. Its result is kept
  // here; pages that show simulation results subscribe instead of having their own input form.
  var listeners = [];
  var version = 0;

  // POST /simulate. Called by the Simulation Parameters panel; resolves with R's result or rejects with an FX.ApiError.
  function run(body) {
    if (state.busy) return Promise.reject(new FX.ApiError('A simulation is already running.', { code: 'BUSY' }));
    state.busy = true;
    var view = document.getElementById('view-dashboard');
    view.classList.add('is-busy');
    if (!state.sim) ['chart-paths', 'chart-dist', 'chart-mc'].forEach(function (id) { setChartState(id, 'R is simulating…'); });
    state.runPromise = FX.api.post('/simulate', body).then(function (data) {
      state.sim = data;
      state.request = body;
      version += 1;
      render();
      listeners.forEach(function (fn) { fn(data); });
      return data;
    }).catch(function (err) {
      if (!state.sim) {
        ['chart-paths', 'chart-dist', 'chart-mc'].forEach(function (id) { setChartState(id, err.message, 'error'); });
      }
      throw err;
    }).finally(function () {
      state.busy = false;
      view.classList.remove('is-busy');
    });
    return state.runPromise;
  }

  // First use: load the defaults into the Simulation Parameters panel and run them once, so pages show real R output.
  // Resolves when a simulation is available, or with null (and lastError set) if it is not.
  var pending = null;
  function ensureSimulation() {
    if (state.sim) return Promise.resolve(state.sim);
    if (pending) return pending;
    if (state.busy && state.runPromise) {
      return state.runPromise.then(function () { return state.sim; }, function () { return null; });
    }
    pending = FX.simParams.ready().then(function (defs) {
      FX.lastDefaults = defs;
      if (!state.sim && !state.busy) return FX.simParams.run({ auto: true });
    }).then(function () {
      if (!state.sim) throw new FX.ApiError(document.getElementById('params-message').textContent || 'The simulation could not be run.');
      state.lastError = null;
      return state.sim;
    }).catch(function (err) {
      state.lastError = err.message;
      ['chart-paths', 'chart-dist', 'chart-mc'].forEach(function (id) { setChartState(id, err.message, 'error'); });
      ['kpi-mean', 'kpi-vol', 'kpi-return'].forEach(function (id) { setKpi(id, { value: '—', sub: 'No simulation yet' }); });
      return null;
    }).finally(function () { pending = null; });
    return pending;
  }

  function ensureData() {
    if (state.histVersion !== FX.dataVersion) loadHistorical();
    ensureSimulation();
  }

  FX.simulation = {
    run: run,
    ensure: ensureSimulation,
    current: function () { return state.sim; },
    version: function () { return version; },
    isBusy: function () { return state.busy; },
    lastError: function () { return state.lastError || null; },
    onChange: function (fn) { listeners.push(fn); },
    // Re-run the CURRENT simulation (same inputs and the seed R used) with extra options such as a
    // threshold rate. Same seed + same inputs = the same paths, so the results stay consistent.
    rerunWith: function (extra) {
      if (!state.sim || !state.request) return Promise.reject(new FX.ApiError('Run a simulation first.', { code: 'NO_SIMULATION' }));
      return run(Object.assign({}, state.request, { seed: state.sim.seed }, extra));
    }
  };
  FX.dashboard = { run: run };

  FX.sections.register('dashboard', {
    init: function () {
      $('#paths-select').addEventListener('change', function () { if (state.sim) renderPaths(); });
      $('#mc-paths-select').addEventListener('change', function () { if (state.sim) renderMonteCarlo(); });
      document.querySelectorAll('#view-dashboard [data-download]').forEach(function (btn) {
        btn.addEventListener('click', function () { FX.charts.downloadPNG(btn.dataset.download, 'fxlab-' + btn.dataset.download + '.png'); });
      });
      document.querySelectorAll('#view-dashboard [data-expand]').forEach(function (btn) {
        btn.addEventListener('click', function () { FX.charts.toggleFullscreen(document.getElementById(btn.dataset.expand)); });
      });
    },
    show: ensureData
  });
})();
