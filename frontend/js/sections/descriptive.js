/* descriptive.js - Descriptive Statistics of the uploaded chart (computed by R; this file only draws them).
   Shown: summary statistics (with moments), histogram with the fitted normal curve, and a bar graph of
   observed against normal frequencies. */
(function () {
  var ui, fmt;
  var state = { version: -1 };

  function summaryTable(r, p) {
    var n = function (x) { return fmt.sig(x, 6); };
    var mo = r.moments;
    return ui.kvTable('Summary statistics of the returns per candle (%) — computed in R', [
      ['Number of returns (n)', fmt.int(r.n)],
      ['Mean x̄ = Σx / n', n(r.mean)],
      ['Median', n(r.median)],
      ['Mode (grouped: L + (f₁ − f₀) / (2f₁ − f₀ − f₂) × h)', n(r.mode.value)],
      ['Minimum', n(r.min)], ['Maximum', n(r.max)], ['Range', n(r.range)],
      ['First quartile Q₁', n(r.q1)], ['Third quartile Q₃', n(r.q3)], ['Interquartile range Q₃ − Q₁', n(r.iqr)],
      ['Sample variance s² = Σ(x − x̄)² / (n − 1)', n(r.variance)],
      ['Population variance σ² = Σ(x − x̄)² / n', n(r.variance_population)],
      ['Standard deviation s', n(r.sd)],
      ['Standard error s / √n', n(r.standard_error)],
      ['Raw moments μ′₁, μ′₂, μ′₃, μ′₄ (about 0)', mo.raw.map(function (x) { return fmt.sig(x, 4); }).join(' · ')],
      ['Central moments μ₁, μ₂, μ₃, μ₄ (about the mean)', mo.central.map(function (x) { return fmt.sig(x, 4); }).join(' · ')],
      ['β₁ = μ₃² / μ₂³', fmt.num(mo.beta1, 4)], ['β₂ = μ₄ / μ₂²', fmt.num(mo.beta2, 4)],
      ['Skewness γ₁ = μ₃ / μ₂^(3/2)  (normal = 0)', fmt.num(r.skewness, 4)],
      ['Kurtosis β₂  (normal = 3)', fmt.num(r.kurtosis, 4)],
      ['Excess kurtosis γ₂ = β₂ − 3', fmt.num(r.excess_kurtosis, 4)],
      ['Prices: mean · median · mode', fmt.price(p.mean) + ' · ' + fmt.price(p.median) + ' · ' + fmt.price(p.mode.value)],
      ['Prices: standard deviation · coefficient of variation', fmt.price(p.sd) + ' · ' + fmt.pct(p.cv_pct, 2)]
    ]);
  }

  function bandChart(id, bands, n) {
    var c = FX.charts.colors();
    FX.charts.render(id, {
      type: 'bar',
      data: { labels: bands.map(function (b) { return b.band; }), datasets: [
        { label: 'Observed (chart)', data: bands.map(function (b) { return 100 * b.observed_share; }), backgroundColor: c.s1, barPercentage: 0.9, categoryPercentage: 0.75 },
        { label: 'Normal distribution', data: bands.map(function (b) { return 100 * b.normal_share; }), backgroundColor: c.s2, barPercentage: 0.9, categoryPercentage: 0.75 }
      ] },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: { tooltip: { callbacks: { label: function (item) {
          var b = bands[item.dataIndex];
          return item.dataset.label + ': ' + fmt.num(item.parsed.y, 1) + '% (' + (item.datasetIndex ? fmt.num(b.normal_count, 1) : fmt.int(b.observed)) + ' of ' + n + ')';
        } } } },
        scales: { x: FX.charts.axis('Distance from the mean', { type: 'category' }),
                  y: FX.charts.axis('Share of returns (%)', { extra: { beginAtZero: true } }) }
      }
    });
  }

  function render(a, root) {
    var d = a.descriptive, r = d.returns, p = d.prices, c = a.chart;
    root.appendChild(ui.el('p', { class: 'meta', text: c.name + ' · ' + fmt.int(r.n) + ' returns per ' + (c.interval ? c.interval + ' candle' : 'candle') +
      ' from ' + fmt.int(c.n_points) + ' prices read from the chart · r = 100 · ln(Pₜ / Pₜ₋₁)' }));
    root.appendChild(ui.cards([
      { label: 'Mean return', value: fmt.num(r.mean, 4) + '%', sub: 'per candle' },
      { label: 'Median', value: fmt.num(r.median, 4) + '%' },
      { label: 'Mode (grouped)', value: fmt.num(r.mode.value, 4) + '%', sub: 'modal class ' + fmt.num(r.mode.modal_class[0], 3) + ' to ' + fmt.num(r.mode.modal_class[1], 3) },
      { label: 'Standard deviation', value: fmt.num(r.sd, 4) + '%', sub: 'volatility per candle' },
      { label: 'Skewness', value: fmt.num(r.skewness, 3), sub: 'normal = 0' },
      { label: 'Kurtosis', value: fmt.num(r.kurtosis, 3), sub: 'normal = 3' }
    ]));
    root.appendChild(ui.el('p', { class: 'interpretation', text: d.shape_interpretation }));

    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'desc-hist-chart', title: 'Histogram with fitted normal curve',
      note: 'Bars: R\'s histogram of the returns (Freedman–Diaconis bins). Curve: normal density with the same mean and SD.' }));
    FX.charts.histogram('desc-hist-chart', d.returns_histogram, {
      xTitle: 'Return per candle (%)', curve: d.normal_fit ? { x: d.normal_fit.x, y: d.normal_fit.density } : null,
      curveLabel: 'Fitted normal curve', barLabel: 'Observed density'
    });
    grid.appendChild(ui.chartCard({ id: 'desc-band-chart', title: 'Observed vs normal frequencies (bar graph)',
      note: 'Share of returns in each band of standard deviations from the mean. Taller outer bars than the normal = heavier tails.' }));
    bandChart('desc-band-chart', d.sigma_bands, r.n);

    root.appendChild(summaryTable(r, p));
    root.appendChild(ui.warnings((r.warnings || []).concat(c.notes || [])));
  }

  FX.sections.register('descriptive', {
    init: function () { ui = FX.ui; fmt = FX.fmt; },
    show: function () { return FX.chart.renderInto('desc-results', state, render); }
  });
})();
