/* hypothesis.js - Hypothesis Testing via POST /hypothesis-test. */
(function () {
  var ui, fmt;
  var state = { busy: false, version: -1 };

  function nullDistributionChart(id, ch) {
    var c = FX.charts.colors();
    var crit = ch.critical_values;
    function rejected(x) {
      if (ch.distribution === 'F') return x <= crit[0] || x >= crit[1];
      if (ch.alternative === 'two.sided') return x <= crit[0] || x >= crit[1];
      if (ch.alternative === 'less') return x <= crit[0];
      return x >= crit[0];
    }
    var maxD = Math.max.apply(null, ch.density.filter(function (v) { return v != null; }));
    var datasets = [
      { label: 'Rejection region (α)', data: ch.x.map(function (x, i) { return { x: x, y: rejected(x) ? ch.density[i] : null }; }),
        borderWidth: 0, backgroundColor: FX.charts.alpha(c.s2, 0.4), fill: 'origin', spanGaps: false, excludeFromTable: true },
      { label: 'Distribution of the statistic under H0', data: FX.charts.xy(ch.x, ch.density), borderColor: c.text2, borderWidth: 1.5 }
    ];
    if (!ch.observed_off_scale) {
      datasets.push({ label: 'Observed statistic = ' + fmt.sig(ch.observed, 4), data: [{ x: ch.observed, y: 0 }, { x: ch.observed, y: maxD }],
                      borderColor: c.s1, borderWidth: 2.5 });
    }
    FX.charts.render(id, {
      type: 'line', data: { datasets: datasets },
      options: { scales: { x: FX.charts.axis('Test statistic (' + ch.distribution + (ch.df1 != null ? ', df = ' + fmt.sig(ch.df1, 5) + (ch.df2 != null ? ', ' + fmt.sig(ch.df2, 5) : '') : '') + ')'),
                           y: FX.charts.axis('Density', { extra: { beginAtZero: true } }) } }
    });
  }

  function acfChart(id, ch) {
    var c = FX.charts.colors();
    FX.charts.render(id, {
      type: 'bar',
      data: { labels: ch.lags, datasets: [
        { label: 'Autocorrelation of values', data: ch.acf, backgroundColor: c.s1, barPercentage: 0.9, categoryPercentage: 0.8 },
        { label: 'Autocorrelation of squared deviations', data: ch.acf_squared, backgroundColor: c.s2, barPercentage: 0.9, categoryPercentage: 0.8 },
        { type: 'line', label: '+95% bound (±' + fmt.num(ch.bound, 4) + ')', data: ch.lags.map(function () { return ch.bound; }), borderColor: c.text2, borderWidth: 1, pointRadius: 0 },
        { type: 'line', label: '−95% bound', data: ch.lags.map(function () { return -ch.bound; }), borderColor: c.text2, borderWidth: 1, pointRadius: 0, legendHidden: true }
      ] },
      options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
        scales: { x: FX.charts.axis('Lag (days)', { type: 'category' }), y: FX.charts.axis('Autocorrelation') } }
    });
  }

  function render(d, root) {
    root.appendChild(ui.el('p', { class: 'meta', text: d.test_name + ' · ' + d.data_description + ' · α = ' + d.alpha }));
    if (d.hypotheses) {
      root.appendChild(ui.el('div', { class: 'panel hypotheses' }, [
        ui.el('p', { text: fmt.symbols(d.hypotheses.null) }),
        ui.el('p', { text: fmt.symbols(d.hypotheses.alternative) }),
        d.decision ? ui.el('p', {}, ['Decision at α = ' + d.alpha + ': ',
          ui.el('span', { class: 'badge ' + (d.reject_null ? 'badge-reject' : 'badge-keep'), text: d.decision })]) : null
      ]));
    }
    var ci = d.confidence_interval;
    root.appendChild(ui.cards([
      d.statistic != null ? { label: 'Test statistic ' + d.statistic_name, value: fmt.sig(d.statistic, 5), sub: d.df != null ? 'df = ' + (typeof d.df === 'number' ? fmt.sig(d.df, 6) : d.df) : '' } : null,
      d.p_value != null ? { label: 'p-value', value: fmt.pval(d.p_value), sub: d.p_value < d.alpha ? 'below α = ' + d.alpha : 'not below α = ' + d.alpha } : null,
      { label: 'Sample size', value: fmt.int(d.n) },
      ci ? { label: fmt.num(100 * ci.level, 0) + '% CI for the ' + ci.parameter, value: fmt.sig(ci.lower, 4) + ' to ' + fmt.sig(ci.upper, 4),
             sub: ci.note || '' } : null
    ]));
    root.appendChild(ui.el('p', { class: 'interpretation', text: d.interpretation }));

    if (d.results) {
      root.appendChild(ui.table({ caption: 'Test results (R)',
        columns: [
          { key: 'test', label: 'Test' }, { key: 'statistic_name', label: 'Statistic' },
          { key: 'statistic', label: 'Value', num: true, format: function (x) { return fmt.sig(x, 5); } },
          { key: 'df', label: 'df', num: true }, { key: 'p_value', label: 'p-value', num: true, format: fmt.pval },
          { key: 'decision', label: 'Decision' }, { key: 'note', label: 'Note' }
        ], rows: d.results }));
    }
    if (d.groups) {
      root.appendChild(ui.table({ caption: 'Groups',
        columns: [{ key: 'group', label: 'Group' }, { key: 'label', label: 'Data' }, { key: 'n', label: 'n', num: true, format: fmt.int },
                  { key: 'mean', label: 'Mean', num: true, format: function (x) { return fmt.sig(x, 5); } },
                  { key: 'sd', label: 'SD', num: true, format: function (x) { return fmt.sig(x, 5); } },
                  { key: 'variance', label: 'Variance', num: true, format: function (x) { return fmt.sig(x, 5); } }],
        rows: d.groups }));
    }
    if (d.intervals) {
      root.appendChild(ui.table({ caption: fmt.num(100 * d.confidence_interval.level, 0) + '% confidence intervals (R)',
        columns: [{ key: 'parameter', label: 'Parameter' },
                  { key: 'estimate', label: 'Estimate', num: true, format: function (x) { return fmt.sig(x, 5); } },
                  { key: 'lower', label: 'Lower', num: true, format: function (x) { return fmt.sig(x, 5); } },
                  { key: 'upper', label: 'Upper', num: true, format: function (x) { return fmt.sig(x, 5); } },
                  { key: 'method', label: 'Method' }],
        rows: d.intervals }));
    }

    var ch = d.chart || {};
    if (ch.distribution) {
      root.appendChild(ui.chartCard({ id: 'hyp-null-chart', title: 'Where the observed statistic falls under H0',
        note: 'Shaded tails: rejection region of total probability α. ' + (ch.observed_off_scale ? 'The observed statistic (' + fmt.sig(ch.observed, 4) + ') lies far beyond the plotted range.' : '') }));
      nullDistributionChart('hyp-null-chart', ch);
    } else if (ch.type === 'qq' && ch.theoretical) {
      root.appendChild(ui.chartCard({ id: 'hyp-qq-chart', title: 'Normal Q-Q plot', note: 'Points on the line are consistent with a normal distribution.' }));
      FX.charts.qq('hyp-qq-chart', ch);
    } else if (ch.type === 'acf') {
      root.appendChild(ui.chartCard({ id: 'hyp-acf-chart', title: 'Autocorrelation function, lags 1–' + ch.lags.length,
        note: 'Bars outside the bounds are individually significant at α. Squared deviations measure volatility clustering.' }));
      acfChart('hyp-acf-chart', ch);
    }
    if (d.assumptions && d.assumptions.length) {
      root.appendChild(ui.section('Assumptions', [ui.el('ul', { class: 'list-plain' }, d.assumptions.map(function (a) { return ui.el('li', { text: a }); }))]));
    }
    root.appendChild(ui.warnings(d.warnings, 'Warnings about assumptions'));
  }

  function refresh(form) {
    var test = form.querySelector('[name="test"]').value;
    var source = form.querySelector('[name="source"]:checked').value;
    ui.toggleBy(form, 'tests', test);
    ui.toggleBy(form, 'source', source);
    document.getElementById('hyp-values-label-suffix').textContent =
      (test === 'two_sample_t' || test === 'variance_equality') ? '1 values' : 'values';
  }

  function run(form) {
    state.version = FX.dataVersion;
    return ui.runAnalysis({
      state: state, form: form, results: document.getElementById('hyp-results'), loadingText: 'R is running the test…',
      request: function () { return FX.api.post('/hypothesis-test', ui.readForm(form)); },
      render: render
    }).then(function (ok) { if (ok === false) state.version = -1; });
  }

  FX.sections.register('hypothesis', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt;
      var form = document.getElementById('hyp-form');
      form.addEventListener('change', function (e) {
        if (e.target.name === 'test' || e.target.name === 'source') { refresh(form); ui.clearFieldErrors(form); }
      });
      refresh(form);
      form.addEventListener('submit', function (e) { e.preventDefault(); run(form); });
    },
    show: function () { if (state.version !== FX.dataVersion) run(document.getElementById('hyp-form')); }
  });
})();
