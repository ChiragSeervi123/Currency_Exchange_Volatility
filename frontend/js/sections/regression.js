/* regression.js - Correlation and Regression via POST /regression. */
(function () {
  var ui, fmt;
  var state = { busy: false, version: -1 };

  function render(d, root) {
    var co = d.coefficients, r = d.correlation, f = d.fit, c = FX.charts.colors(), ch = d.chart;
    root.appendChild(ui.el('p', { class: 'meta', text: d.data_description }));
    root.appendChild(ui.cards([
      { label: 'Pearson correlation r', value: fmt.num(r.pearson_r, 4), sub: fmt.num(100 * (1 - d.alpha), 0) + '% CI ' + fmt.num(r.pearson_ci_lower, 3) + ' to ' + fmt.num(r.pearson_ci_upper, 3) + ' · ' + r.strength },
      { label: 'Spearman rank correlation ρ', value: fmt.num(r.spearman_rho, 4), sub: fmt.pText(r.spearman_p) },
      { label: 'R-squared', value: fmt.num(f.r_squared, 4), sub: 'adjusted ' + fmt.num(f.adj_r_squared, 4) },
      { label: 'Slope b₁', value: fmt.sig(co[1].estimate, 5), sub: fmt.pText(co[1].p_value) + ' (robust ' + fmt.pText(co[1].robust_p) + ')' },
      { label: 'Observations', value: fmt.int(d.n), sub: d.n_dropped ? d.n_dropped + ' incomplete pairs excluded' : 'complete pairs' }
    ]));
    root.appendChild(ui.el('p', { class: 'interpretation' }, [ui.el('strong', { text: 'Fitted line: ' + d.equation }), ui.el('br'), d.interpretation]));

    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    if (d.model === 'time_trend') {
      grid.appendChild(ui.chartCard({ id: 'reg-fit-chart', title: 'USD/INR and fitted linear trend' }));
      FX.charts.render('reg-fit-chart', {
        type: 'line',
        data: { labels: ch.dates, datasets: [
          { label: 'Observed rate', data: ch.y, borderColor: c.s1, borderWidth: 1.2 },
          { label: 'Fitted trend', data: ch.fitted, borderColor: c.s2, borderWidth: 2 }
        ] },
        options: { scales: { x: FX.charts.axis('Date', { type: 'category' }), y: FX.charts.axis(d.y_label) } }
      });
      grid.appendChild(ui.chartCard({ id: 'reg-resid-chart', title: 'Residuals over time',
        note: 'Long swings instead of random scatter: the residuals are autocorrelated.' }));
      FX.charts.render('reg-resid-chart', {
        type: 'line',
        data: { labels: ch.dates, datasets: [{ label: 'Residual', data: ch.residual, borderColor: c.s1, borderWidth: 1.2 }] },
        options: { plugins: { legend: { display: false } }, scales: { x: FX.charts.axis('Date', { type: 'category' }), y: FX.charts.axis('Residual (INR)') } }
      });
    } else {
      grid.appendChild(ui.chartCard({ id: 'reg-fit-chart', title: 'Scatter plot with fitted regression line',
        note: ch.thinned ? ch.n_shown + ' of ' + d.n + ' points shown (evenly spaced); the fit uses all points.' : 'Each point is one (x, y) pair.' }));
      FX.charts.render('reg-fit-chart', {
        type: 'scatter',
        data: { datasets: [
          { label: 'Observations', data: FX.charts.xy(ch.x, ch.y), backgroundColor: FX.charts.alpha(c.s1, 0.45), borderColor: c.surface, borderWidth: 1, pointRadius: 3 },
          { type: 'line', label: 'Fitted line', data: FX.charts.xy(ch.line.x, ch.line.y), borderColor: c.s2, borderWidth: 2, pointRadius: 0 }
        ] },
        options: { interaction: { mode: 'nearest', intersect: false }, scales: { x: FX.charts.axis(d.x_label), y: FX.charts.axis(d.y_label) } }
      });
      grid.appendChild(ui.chartCard({ id: 'reg-resid-chart', title: 'Residuals vs fitted values',
        note: 'A patternless band around zero supports linearity and constant variance.' }));
      FX.charts.render('reg-resid-chart', {
        type: 'scatter',
        data: { datasets: [
          { label: 'Residual', data: FX.charts.xy(ch.fitted, ch.residual), backgroundColor: FX.charts.alpha(c.s1, 0.45), borderColor: c.surface, borderWidth: 1, pointRadius: 3 },
          { type: 'line', label: 'Zero', data: [{ x: Math.min.apply(null, ch.fitted), y: 0 }, { x: Math.max.apply(null, ch.fitted), y: 0 }],
            borderColor: c.text2, borderWidth: 1, pointRadius: 0, excludeFromTable: true }
        ] },
        options: { interaction: { mode: 'nearest', intersect: false }, plugins: { legend: { display: false } },
          scales: { x: FX.charts.axis('Fitted value'), y: FX.charts.axis('Residual') } }
      });
    }

    var sig = function (x) { return fmt.sig(x, 5); };
    root.appendChild(ui.table({ caption: 'Coefficients (OLS) with ' + fmt.num(100 * (1 - d.alpha), 0) + '% confidence intervals; robust = HC1 heteroskedasticity-consistent',
      columns: [
        { key: 'term', label: 'Term' }, { key: 'estimate', label: 'Estimate', num: true, format: sig },
        { key: 'std_error', label: 'SE', num: true, format: sig }, { key: 't_value', label: 't', num: true, format: function (x) { return fmt.num(x, 3); } },
        { key: 'p_value', label: 'p-value', num: true, format: fmt.pval },
        { key: 'ci_lower', label: 'CI lower', num: true, format: sig }, { key: 'ci_upper', label: 'CI upper', num: true, format: sig },
        { key: 'robust_se', label: 'Robust SE', num: true, format: sig }, { key: 'robust_p', label: 'Robust p', num: true, format: fmt.pval }
      ], rows: co }));
    var two = ui.el('div', { class: 'grid-2' }, [
      ui.kvTable('Model fit', [
        ['R-squared', fmt.num(f.r_squared, 4)], ['Adjusted R-squared', fmt.num(f.adj_r_squared, 4)],
        ['Residual standard error', fmt.sig(f.residual_se, 5)],
        ['F statistic', fmt.sig(f.f_statistic, 5) + ' on ' + f.f_df1 + ' and ' + f.f_df2 + ' df'], ['F-test p-value', fmt.pval(f.f_p_value)],
        ['Pearson t statistic', fmt.sig(r.pearson_t, 5) + ' (df ' + r.pearson_df + ')'], ['Pearson p-value', fmt.pval(r.pearson_p)]
      ]),
      ui.table({ caption: 'Residual diagnostics',
        columns: [{ key: 'diagnostic', label: 'Diagnostic' }, { key: 'value', label: 'Value', num: true, format: function (x) { return fmt.sig(x, 4); } },
                  { key: 'p_value', label: 'p-value', num: true, format: fmt.pval }, { key: 'reading', label: 'Reading' }],
        rows: d.diagnostics })
    ]);
    root.appendChild(two);
    root.appendChild(ui.warnings(d.warnings));
  }

  function run(form) {
    state.version = FX.dataVersion;
    return ui.runAnalysis({
      state: state, form: form, results: document.getElementById('reg-results'), loadingText: 'R is fitting the model…',
      request: function () { return FX.api.post('/regression', ui.readForm(form)); },
      render: render
    }).then(function (ok) { if (ok === false) state.version = -1; });
  }

  FX.sections.register('regression', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt;
      var form = document.getElementById('reg-form');
      var model = form.querySelector('[name="model"]');
      model.addEventListener('change', function () { ui.toggleBy(form, 'models', model.value); ui.clearFieldErrors(form); });
      ui.toggleBy(form, 'models', model.value);
      form.addEventListener('submit', function (e) { e.preventDefault(); run(form); });
    },
    show: function () { if (state.version !== FX.dataVersion) run(document.getElementById('reg-form')); }
  });
})();
