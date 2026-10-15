/* descriptive.js - Descriptive Statistics via POST /statistics. */
(function () {
  var ui, fmt;
  var state = { busy: false, version: -1 };

  function statsTable(s, kind) {
    var d = kind === 'level' ? 4 : 5;
    var n = function (x) { return fmt.sig(x, 6); };
    return ui.kvTable('Summary statistics (R)', [
      ['Valid observations (n)', fmt.int(s.n)],
      ['Mean', n(s.mean)], ['Median', n(s.median)], ['Minimum', n(s.min)], ['Maximum', n(s.max)], ['Range', n(s.range)],
      ['Sample variance s² (divisor n − 1)', n(s.variance)],
      ['Population variance (divisor n)', n(s.variance_population)],
      ['Sample standard deviation s', n(s.sd)],
      ['Standard error of the mean s / √n', n(s.standard_error)],
      ['First quartile Q1', n(s.q1)], ['Third quartile Q3', n(s.q3)], ['Interquartile range', n(s.iqr)],
      ['Skewness', fmt.num(s.skewness, 4)], ['Kurtosis (normal = 3)', fmt.num(s.kurtosis, 4)],
      ['Excess kurtosis', fmt.num(s.excess_kurtosis, 4)],
      s.cv_pct != null ? ['Coefficient of variation', fmt.pct(s.cv_pct, d > 4 ? 2 : 3)] : null
    ]);
  }

  function render(d, root) {
    var s = d.statistics;
    root.appendChild(ui.el('p', { class: 'meta', text: d.data_description }));
    root.appendChild(ui.cards([
      { label: 'Observations', value: fmt.int(s.n), sub: s.n_excluded ? s.n_excluded + ' excluded' : 'all valid' },
      { label: 'Mean', value: fmt.sig(s.mean, 5), sub: d.label },
      { label: 'Median', value: fmt.sig(s.median, 5) },
      { label: 'Standard deviation', value: fmt.sig(s.sd, 5), sub: 'sample, n − 1' },
      { label: 'Skewness', value: fmt.num(s.skewness, 3), sub: 'normal = 0' },
      { label: 'Excess kurtosis', value: fmt.num(s.excess_kurtosis, 3), sub: 'normal = 0' }
    ]));
    root.appendChild(ui.el('p', { class: 'interpretation', text: d.shape_interpretation }));
    if (d.volatility) {
      var v = d.volatility;
      root.appendChild(ui.cards([
        { label: 'Daily volatility', value: fmt.pct(v.daily_sd_pct, 4), sub: 'SD of daily log returns' },
        { label: 'Annualised volatility', value: fmt.pct(v.annualised_vol_pct, 2), sub: 'daily SD × √' + v.trading_days_per_year },
        { label: 'Mean absolute daily return', value: fmt.pct(v.mean_abs_return_pct, 4) },
        d.cumulative_return_pct != null ? { label: 'Cumulative return', value: fmt.pct(d.cumulative_return_pct, 2), sub: 'first to last price' } : null,
        v.rolling_latest_pct != null ? { label: 'Latest ' + v.rolling_window + '-day volatility', value: fmt.pct(v.rolling_latest_pct, 2), sub: 'annualised, ' + v.rolling_latest_date } : null
      ]));
    }
    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'desc-hist-chart', title: 'Histogram with fitted normal curve',
      note: 'Bars: R\'s histogram (Freedman–Diaconis bins). Curve: normal density with the sample mean and SD.' }));
    FX.charts.histogram('desc-hist-chart', d.histogram, {
      xTitle: d.label, curve: d.normal_fit ? { x: d.normal_fit.x, y: d.normal_fit.density } : null,
      curveLabel: 'Normal density (same mean and SD)', barLabel: 'Observed density'
    });
    if (d.qq) {
      grid.appendChild(ui.chartCard({ id: 'desc-qq-chart', title: 'Normal Q-Q plot',
        note: 'Points on the line = normal. Points bending away at the ends = heavier tails' + (d.qq.n_shown < d.qq.n ? ' (' + d.qq.n_shown + ' of ' + d.qq.n + ' quantiles shown).' : '.') }));
      FX.charts.qq('desc-qq-chart', d.qq);
    }
    var two = ui.el('div', { class: 'grid-2' }, [
      statsTable(s, d.kind),
      ui.table({ caption: 'Percentiles (R quantile type 7)',
        columns: [{ key: 'percentile', label: 'Percentile', num: true, format: function (x) { return fmt.num(x, 0) + 'th'; } },
                  { key: 'value', label: 'Value', num: true, format: function (x) { return fmt.sig(x, 6); } }],
        rows: s.quantiles })
    ]);
    root.appendChild(two);
    if (d.tails) {
      root.appendChild(ui.table({
        caption: 'Tail check: share of observations more than k standard deviations from the mean',
        columns: [
          { key: 'k_sd', label: 'k', num: true, format: function (x) { return '|z| > ' + x; } },
          { key: 'empirical_count', label: 'Count', num: true, format: fmt.int },
          { key: 'empirical_share', label: 'Observed share', num: true, format: function (x) { return fmt.prob(x, 3); } },
          { key: 'normal_share', label: 'Normal distribution', num: true, format: function (x) { return fmt.prob(x, 3); } },
          { key: 'ratio', label: 'Ratio', num: true, format: function (x) { return fmt.num(x, 2) + '×'; } }
        ],
        rows: d.tails
      }));
    }
    if (d.level_statistics) {
      var l = d.level_statistics;
      root.appendChild(ui.kvTable('The entered prices themselves', [
        ['n', fmt.int(l.n)], ['Mean', fmt.sig(l.mean, 6)], ['SD', fmt.sig(l.sd, 6)], ['Min', fmt.sig(l.min, 6)], ['Max', fmt.sig(l.max, 6)]
      ]));
    }
    root.appendChild(ui.warnings((s.warnings || []).concat(d.note ? [d.note] : [])));
  }

  function run(form) {
    var results = document.getElementById('desc-results');
    state.version = FX.dataVersion;
    return ui.runAnalysis({
      state: state, form: form, results: results, loadingText: 'R is calculating the statistics…',
      request: function () { return FX.api.post('/statistics', ui.readForm(form)); },
      render: render
    }).then(function (ok) { if (ok === false) state.version = -1; });
  }

  FX.sections.register('descriptive', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt;
      var form = document.getElementById('desc-form');
      form.addEventListener('change', function (e) {
        if (e.target.name === 'source') ui.toggleBy(form, 'source', e.target.value);
      });
      ui.toggleBy(form, 'source', 'historical');
      form.addEventListener('submit', function (e) { e.preventDefault(); run(form); });
    },
    show: function () { if (state.version !== FX.dataVersion) run(document.getElementById('desc-form')); }
  });
})();
