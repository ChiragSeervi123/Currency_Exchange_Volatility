/* =============================================================================
   montecarlo.js - Monte Carlo analysis of the CURRENT simulation.
   The simulation inputs live only in the Simulation Parameters panel of the
   Exchange Rate Dashboard; this page shows R's detailed results for that
   simulation and offers
   its own two options: how many paths to draw (display only) and a threshold
   rate, for which R re-runs the same simulation (same inputs and seed) and
   returns the threshold probabilities.
   ============================================================================= */
(function () {
  var ui = FX.ui, fmt = FX.fmt;
  var state = { busy: false, version: -1 };
  var form, results, select, thresholdInput;

  function ci(arr, digits) {
    return arr && arr[0] != null ? '95% CI ' + fmt.num(arr[0], digits) + ' – ' + fmt.num(arr[1], digits) : '';
  }
  function ciProb(arr) {
    return arr && arr[0] != null ? '95% CI ' + fmt.prob(arr[0], 1) + ' – ' + fmt.prob(arr[1], 1) : '';
  }

  function render(d, root) {
    var inp = d.inputs, t = d.terminal, th = d.theory, c = FX.charts.colors(), thr = d.threshold;
    var isNormal = inp.model === 'gbm_normal';
    var drawn = drawnCount(d);
    root.appendChild(ui.cards([
      { label: 'Expected final rate E[S_T] (simulated)', value: fmt.num(t.mean, 4),
        sub: ci(t.mean_ci95, 3) + (th.mean != null ? ' · theory ' + fmt.num(th.mean, 4) : '') },
      { label: 'Median final rate', value: fmt.num(t.median, 4), sub: th.median != null ? 'theory ' + fmt.num(th.median, 4) : 'INR per USD' },
      { label: 'SD of final rate', value: fmt.num(t.sd, 4), sub: th.sd != null ? 'theory ' + fmt.num(th.sd, 4) : 'INR per USD' },
      { label: '5th – 95th percentile', value: fmt.num(t.percentiles[1].value, 2) + ' – ' + fmt.num(t.percentiles[7].value, 2),
        sub: '90% of simulated final rates' },
      thr ? { label: 'P(S_T > ' + fmt.num(thr.threshold, 2) + ')', value: fmt.prob(thr.prob_above),
        sub: ciProb(thr.prob_above_ci95) + (thr.theory_prob_above != null ? ' · theory ' + fmt.prob(thr.theory_prob_above) : '') } : null,
      thr ? { label: 'P(S_T < ' + fmt.num(thr.threshold, 2) + ')', value: fmt.prob(thr.prob_below), sub: ciProb(thr.prob_below_ci95) } : null,
      thr ? { label: 'P(reaching ' + fmt.num(thr.threshold, 2) + ' at least once)', value: fmt.prob(thr.prob_touch),
              sub: ciProb(thr.prob_touch_ci95) + ' · checked daily' } : null
    ]));

    // Fan chart: percentile bands across ALL paths + a few sample paths.
    var days = d.bands.day;
    var datasets = d.paths.values.slice(0, drawn).map(function (vals, i) {
      return { label: i === 0 ? 'Sample paths (' + drawn + ')' : 'Sample path ' + (i + 1),
               data: FX.charts.xy(d.paths.day, vals), borderColor: c.path, borderWidth: 1,
               legendHidden: i > 0, excludeFromTable: true, order: 3 };
    });
    var nPaths = drawn;
    datasets.push(
      { label: '5th percentile', data: FX.charts.xy(days, d.bands.p05), borderWidth: 0, fill: false, legendHidden: true, order: 2 },
      { label: '5th–95th percentile band', tooltipLabel: '95th percentile', data: FX.charts.xy(days, d.bands.p95), borderWidth: 0,
        backgroundColor: c.s1softer, fill: '-1', order: 2 },
      { label: '25th percentile', data: FX.charts.xy(days, d.bands.p25), borderWidth: 0, fill: false, legendHidden: true, order: 2 },
      { label: '25th–75th percentile band', tooltipLabel: '75th percentile', data: FX.charts.xy(days, d.bands.p75), borderWidth: 0,
        backgroundColor: c.s1soft, fill: '-1', order: 2 },
      { label: 'Median across all paths', data: FX.charts.xy(days, d.bands.p50), borderColor: c.s1, borderWidth: 2, order: 1 },
      { label: 'Mean across all paths', data: FX.charts.xy(days, d.bands.mean), borderColor: c.s2, borderWidth: 2, order: 1 }
    );
    root.appendChild(ui.chartCard({ id: 'mc-fan-chart', tall: true, title: 'Simulated USD/INR: percentile bands across all ' + fmt.int(inp.n_paths) + ' paths',
      note: 'Bands and lines are computed in R from every path at every day; grey lines are individual sample paths.' }));
    FX.charts.render('mc-fan-chart', {
      type: 'line', data: { datasets: datasets },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: { legend: { labels: { filter: FX.charts.legendFilter } },
                   tooltip: {
                     // Show the percentiles, median and mean for the hovered day (not every sample path).
                     filter: function (item) { return item.datasetIndex >= nPaths; },
                     callbacks: {
                       title: function (items) { return 'Day ' + items[0].parsed.x; },
                       label: function (item) {
                         return (item.dataset.tooltipLabel || item.dataset.label) + ': ' + fmt.num(item.parsed.y, 4);
                       }
                     }
                   } },
        scales: { x: FX.charts.axis('Trading day', { extra: { min: 0, max: inp.days } }), y: FX.charts.axis('INR per USD') }
      }
    });

    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);

    // Distribution of final rates.
    var h = FX.charts.columns(d.terminal_histogram);
    var histSets = [{ type: 'bar', label: 'Simulated final rates (density)', data: FX.charts.xy(h.mid, h.density),
                      backgroundColor: c.s1, barPercentage: 0.94, categoryPercentage: 1, order: 2 }];
    if (h.theoretical_density) {
      histSets.push({ type: 'line', label: 'Log-normal density (theory)', data: FX.charts.xy(h.mid, h.theoretical_density),
                      borderColor: c.s2, borderWidth: 2, order: 1 });
    }
    if (thr) {
      histSets.push({ type: 'line', label: 'Threshold ' + fmt.num(thr.threshold, 2),
        data: [{ x: thr.threshold, y: 0 }, { x: thr.threshold, y: Math.max.apply(null, h.density) }],
        borderColor: c.text2, borderWidth: 1.5, order: 0 });
    }
    grid.appendChild(ui.chartCard({ id: 'mc-terminal-chart', title: 'Distribution of the final rate S_T after ' + inp.days + ' days',
      note: isNormal ? 'Bars: all simulated final rates. Line: exact log-normal density of GBM.' : 'Bars: all simulated final rates.' }));
    FX.charts.render('mc-terminal-chart', {
      type: 'bar', data: { datasets: histSets },
      options: {
        plugins: { tooltip: { mode: 'nearest', intersect: false } },
        scales: { x: FX.charts.axis('Final rate (INR per USD)', { extra: { offset: false } }), y: FX.charts.axis('Density') }
      }
    });

    // Convergence of the Monte Carlo mean.
    if (d.convergence) {
      var cv = d.convergence;
      var convSets = [
        { label: '95% CI (lower)', data: FX.charts.xy(cv.paths, cv.lower95), borderWidth: 0, fill: false, legendHidden: true },
        { label: '95% confidence band', data: FX.charts.xy(cv.paths, cv.upper95), borderWidth: 0, backgroundColor: c.s1soft, fill: '-1' },
        { label: 'Running Monte Carlo mean', data: FX.charts.xy(cv.paths, cv.running_mean), borderColor: c.s1, borderWidth: 2 }
      ];
      if (cv.theory_mean != null) {
        convSets.push({ label: 'Theoretical E[S_T]', data: [{ x: cv.paths[0], y: cv.theory_mean }, { x: cv.paths[cv.paths.length - 1], y: cv.theory_mean }],
                        borderColor: c.s2, borderWidth: 2 });
      }
      grid.appendChild(ui.chartCard({ id: 'mc-conv-chart', title: 'Convergence of the estimate of E[S_T]',
        note: 'Law of Large Numbers: the estimate settles as paths are added; the band narrows like 1/√N.' }));
      FX.charts.render('mc-conv-chart', {
        type: 'line', data: { datasets: convSets },
        options: {
          plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
          scales: { x: FX.charts.axis('Number of paths (log scale)', { type: 'logarithmic' }), y: FX.charts.axis('INR per USD') }
        }
      });
    }

    var rows = t.percentiles.map(function (p, i) {
      return { pct: p.percentile, mc: p.value, theory: th.percentiles ? th.percentiles[i].value : null };
    });
    root.appendChild(ui.table({
      caption: 'Percentiles of the final rate S_T' + (th.percentiles ? ': simulation vs exact GBM theory' : ''),
      columns: [
        { key: 'pct', label: 'Percentile', num: true, format: function (v) { return fmt.num(v, 0) + 'th'; } },
        { key: 'mc', label: 'Simulated', num: true, format: function (v) { return fmt.num(v, 4); } },
        th.percentiles ? { key: 'theory', label: 'Theory (log-normal)', num: true, format: function (v) { return fmt.num(v, 4); } } : null
      ].filter(Boolean),
      rows: rows
    }));
    root.appendChild(ui.table({
      caption: 'Statistics of each path drawn on the chart (computed in R)',
      columns: [
        { key: 'path', label: 'Path', num: true, format: fmt.int },
        { key: 'final_rate', label: 'Final rate', num: true, format: function (v) { return fmt.num(v, 4); } },
        { key: 'total_return_pct', label: 'Total return', num: true, format: function (v) { return fmt.pct(v, 2); } },
        { key: 'max_rate', label: 'Maximum', num: true, format: function (v) { return fmt.num(v, 4); } },
        { key: 'min_rate', label: 'Minimum', num: true, format: function (v) { return fmt.num(v, 4); } },
        { key: 'realised_daily_vol_pct', label: 'Realised daily vol.', num: true, format: function (v) { return fmt.pct(v, 4); } }
      ],
      rows: d.path_statistics.slice(0, drawn), compact: drawn > 10
    }));
    root.appendChild(ui.kvTable('Log return over the horizon, ln(S_T / S_0)', [
      ['Simulated mean', fmt.pct(t.log_return_mean_pct, 4)],
      ['Theoretical mean (μ − σ²/2)·T', fmt.pct(th.log_return_mean_pct, 4)],
      ['Simulated SD', fmt.pct(t.log_return_sd_pct, 4)],
      ['Theoretical SD σ·√T', fmt.pct(th.log_return_sd_pct, 4)],
      ['Monte Carlo standard error of E[S_T]', fmt.num(t.mean_standard_error, 5)]
    ]));
    root.appendChild(ui.warnings([th.note, d.all_positive ? 'All simulated rates are positive.' : null], 'Model notes'));
  }

  // ---- Page logic ----------------------------------------------------------------------------
  function drawnCount(d) {
    var v = Number(select.value);
    return v > 0 ? Math.min(v, d.paths.values.length) : Math.min(20, d.paths.values.length);
  }

  function describe(d) {
    var inp = d.inputs;
    return 'Current simulation: ' + FX.simForm.modelLabel(FX.lastDefaults || {}, inp.model) +
      (inp.t_df ? ' (df = ' + inp.t_df + ')' : '') + ' · ' + fmt.int(inp.n_paths) + ' paths × ' + fmt.int(inp.days) +
      ' trading days · S₀ = ₹' + fmt.num(inp.start_rate, 2) + ', μ = ' + fmt.num(inp.mu_daily_pct, 4) + '%/day, σ = ' +
      fmt.num(inp.sigma_daily_pct, 4) + '%/day · seed ' + d.seed +
      (d.bootstrap_pool ? ' · shocks resampled from ' + fmt.int(d.bootstrap_pool.size) + ' standardised historical returns' : '') + '.';
  }

  // "Paths drawn" options: subsets of the paths R returned (display only; statistics use all paths).
  function fillPathOptions(d) {
    var n = d.paths.values.length;
    var options = [5, 10, 20, 50, 100, 200].filter(function (k) { return k < n; }).concat([n]);
    var current = Number(select.value);
    select.replaceChildren();
    options.forEach(function (k) { select.appendChild(ui.el('option', { value: k, text: k === n ? 'All returned (' + k + ')' : String(k) })); });
    select.value = String(options.indexOf(current) >= 0 ? current : (options.indexOf(20) >= 0 ? 20 : n));
    select.disabled = false;
  }

  function prepare(d) {
    state.version = FX.simulation.version();
    document.getElementById('mc-params').textContent = describe(d);
    fillPathOptions(d);
    if (d.threshold) thresholdInput.value = d.threshold.threshold;
  }

  function showCurrent() {
    var d = FX.simulation.current();
    if (!d) return;
    prepare(d);
    FX.charts.destroyWithin(results);
    results.replaceChildren();
    render(d, results);
  }

  FX.sections.register('monte-carlo', {
    init: function () {
      form = document.getElementById('mc-form');
      results = document.getElementById('mc-results');
      select = document.getElementById('mc-paths-drawn');
      thresholdInput = document.getElementById('mc-threshold');
      select.addEventListener('change', function () { if (FX.simulation.current() && !state.busy) showCurrent(); });
      form.addEventListener('submit', function (e) {
        e.preventDefault();
        ui.runAnalysis({
          state: state, form: form, results: results, loadingText: 'R is calculating the probabilities for the current simulation…',
          request: function () { return FX.simulation.rerunWith({ threshold: Number(thresholdInput.value) }); },
          render: function (d, root) { prepare(d); render(d, root); }
        });
      });
      // A new simulation from the Simulation Parameters panel updates this page (unless a request from this page is running).
      FX.simulation.onChange(function () {
        if (!state.busy && !document.getElementById('view-monte-carlo').hidden) showCurrent();
      });
      FX.defaults().then(function (defs) {
        if (defs.simulation.threshold) thresholdInput.placeholder = 'e.g. ' + defs.simulation.threshold;
      }).catch(function () {});
    },
    show: function () {
      // First visit: run the default simulation if none exists yet.
      FX.simulation.ensure().then(function (d) {
        if (d) { if (state.version !== FX.simulation.version()) showCurrent(); return; }
        results.replaceChildren(ui.banner('error', 'No simulation is available yet.', [FX.simulation.lastError() || '']));
      });
    }
  });
})();
