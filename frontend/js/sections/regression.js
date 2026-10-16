/* regression.js - Correlation and Regression for the uploaded chart (fitted in R; this file only draws it).
   Price on candle number (least squares), Spearman rank correlation, the multiple regression on t and t²,
   and the regression of each return on the previous one. */
(function () {
  var ui, fmt;
  var state = { version: -1 };

  function formula(text) { return ui.el('p', { class: 'formula', text: text }); }

  function coefTable(caption, rows) {
    var sig = function (x) { return fmt.sig(x, 5); };
    return ui.table({ caption: caption,
      columns: [
        { key: 'term', label: 'Term' }, { key: 'estimate', label: 'Estimate', num: true, format: sig },
        { key: 'std_error', label: 'SE', num: true, format: sig }, { key: 't_value', label: 't', num: true, format: function (x) { return fmt.num(x, 3); } },
        { key: 'p_value', label: 'p-value', num: true, format: fmt.pval },
        { key: 'ci_lower', label: 'CI lower', num: true, format: sig }, { key: 'ci_upper', label: 'CI upper', num: true, format: sig },
        { key: 'robust_se', label: 'Robust SE', num: true, format: sig }, { key: 'robust_p', label: 'Robust p', num: true, format: fmt.pval }
      ].filter(function (c) { return rows[0][c.key] !== undefined; }), rows: rows });
  }

  function render(a, root) {
    var g = a.regression, tr = g.trend, r = tr.correlation, f = tr.fit, co = tr.coefficients, c = FX.charts.colors();
    var sp = g.spearman, mu = g.multiple, lag = g.lag;
    root.appendChild(ui.el('p', { class: 'meta', text: a.chart.name + ' · ' + fmt.int(tr.n) + ' prices read from the chart · x = candle number t = 1, 2, …, n' }));
    root.appendChild(ui.cards([
      { label: 'Pearson correlation r (price, t)', value: fmt.num(r.pearson_r, 4), sub: '95% CI ' + fmt.num(r.pearson_ci_lower, 3) + ' to ' + fmt.num(r.pearson_ci_upper, 3) + ' · ' + r.strength },
      { label: 'Spearman rank correlation ρ', value: fmt.num(sp.rho, 4), sub: '1 − 6Σd² / n(n² − 1), Σd² = ' + fmt.int(sp.sum_d2) },
      { label: 'R-squared', value: fmt.num(f.r_squared, 4), sub: 'adjusted ' + fmt.num(f.adj_r_squared, 4) },
      { label: 'Slope b (change per candle)', value: fmt.sig(co[1].estimate, 5), sub: fmt.pText(co[1].p_value) + ' (robust ' + fmt.pText(co[1].robust_p) + ')' },
      { label: 'Multiple regression R²', value: fmt.num(mu.r_squared, 4), sub: 'price on t and t²' }
    ]));
    root.appendChild(ui.el('p', { class: 'interpretation' }, [ui.el('strong', { text: 'Least-squares line: ' + tr.equation }), ui.el('br'), tr.interpretation]));

    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'reg-fit-chart', title: 'Scatter diagram: price against candle number',
      note: 'Points: prices read from the chart. Lines: least-squares line and the quadratic (multiple regression) fit.' }));
    var ch = tr.chart;
    FX.charts.render('reg-fit-chart', {
      type: 'scatter',
      data: { datasets: [
        { label: 'Price', data: FX.charts.xy(ch.x, ch.y), backgroundColor: FX.charts.alpha(c.s1, 0.5), borderColor: c.surface, borderWidth: 1, pointRadius: 2.5 },
        { type: 'line', label: 'Least-squares line', data: FX.charts.xy(ch.line.x, ch.line.y), borderColor: c.s2, borderWidth: 2, pointRadius: 0 },
        { type: 'line', label: 'Quadratic fit (t, t²)', data: FX.charts.xy(a.series.index, mu.fitted), borderColor: c.s3, borderWidth: 2, pointRadius: 0, borderDash: [6, 4] }
      ] },
      options: { interaction: { mode: 'nearest', intersect: false },
        scales: { x: FX.charts.axis('Candle number t'), y: FX.charts.axis('Price', { tickFormat: function (v) { return fmt.price(v); } }) } }
    });
    grid.appendChild(ui.chartCard({ id: 'reg-lag-chart', title: 'Scatter diagram: return against the previous return',
      note: 'A rising line means moves tend to continue (momentum); a falling line means they tend to reverse.' }));
    var lc = lag.chart;
    FX.charts.render('reg-lag-chart', {
      type: 'scatter',
      data: { datasets: [
        { label: 'Pairs (rₜ₋₁, rₜ)', data: FX.charts.xy(lc.x, lc.y), backgroundColor: FX.charts.alpha(c.s1, 0.5), borderColor: c.surface, borderWidth: 1, pointRadius: 2.5 },
        { type: 'line', label: 'Least-squares line', data: FX.charts.xy(lc.line.x, lc.line.y), borderColor: c.s2, borderWidth: 2, pointRadius: 0 }
      ] },
      options: { interaction: { mode: 'nearest', intersect: false },
        scales: { x: FX.charts.axis('Previous return (%)'), y: FX.charts.axis('Current return (%)') } }
    });

    root.appendChild(coefTable('Least-squares line: price = b₀ + b₁ t (95% confidence intervals; robust = HC1 heteroskedasticity-consistent)', co));

    root.appendChild(ui.section('Multiple regression: price = b₀ + b₁ t + b₂ t²', [
      formula(mu.equation),
      coefTable('Coefficients (least squares)', mu.coefficients),
      ui.el('p', { class: 'interpretation', text: mu.interpretation + ' F = ' + fmt.sig(mu.f_statistic, 5) + ' on ' + mu.f_df1 + ' and ' + mu.f_df2 + ' df, ' + fmt.pText(mu.f_p_value) +
        '; adjusted R² = ' + fmt.num(mu.adj_r_squared, 4) + '.' })
    ]));

    root.appendChild(ui.section('Regression of each return on the previous return', [
      ui.cards([
        { label: 'Correlation r(rₜ₋₁, rₜ)', value: fmt.num(lag.correlation.pearson_r, 4), sub: fmt.pText(lag.correlation.pearson_p) + ' · ' + lag.correlation.strength },
        { label: 'Spearman ρ', value: fmt.num(lag.correlation.spearman_rho, 4), sub: fmt.pText(lag.correlation.spearman_p) },
        { label: 'Slope', value: fmt.sig(lag.coefficients[1].estimate, 4), sub: fmt.pText(lag.coefficients[1].p_value) }
      ]),
      ui.el('p', { class: 'interpretation', text: lag.equation + '. ' + lag.interpretation })
    ]));

    var two = ui.el('div', { class: 'grid-2' }, [
      ui.kvTable('Model fit (price on t)', [
        ['R-squared', fmt.num(f.r_squared, 4)], ['Adjusted R-squared', fmt.num(f.adj_r_squared, 4)],
        ['Residual standard error', fmt.sig(f.residual_se, 5)],
        ['F statistic', fmt.sig(f.f_statistic, 5) + ' on ' + f.f_df1 + ' and ' + f.f_df2 + ' df'], ['F-test p-value', fmt.pval(f.f_p_value)],
        ['Pearson t statistic', fmt.sig(r.pearson_t, 5) + ' (df ' + r.pearson_df + ')'], ['Pearson p-value', fmt.pval(r.pearson_p)],
        g.range_vs_return ? ['Correlation of candle range (H − L) with |return|', 'Pearson ' + fmt.num(g.range_vs_return.pearson, 3) + ' · Spearman ' + fmt.num(g.range_vs_return.spearman, 3)] : null
      ]),
      ui.table({ caption: 'Residual diagnostics (price on t)',
        columns: [{ key: 'diagnostic', label: 'Diagnostic' }, { key: 'value', label: 'Value', num: true, format: function (x) { return fmt.sig(x, 4); } },
                  { key: 'p_value', label: 'p-value', num: true, format: fmt.pval }, { key: 'reading', label: 'Reading' }],
        rows: tr.diagnostics })
    ]);
    root.appendChild(two);
    root.appendChild(ui.warnings([g.warnings.join(' ')]));
  }

  FX.sections.register('regression', {
    init: function () { ui = FX.ui; fmt = FX.fmt; },
    show: function () { return FX.chart.renderInto('reg-results', state, render); }
  });
})();
