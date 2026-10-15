/* distributions.js - Probability Distribution Analysis via POST /distribution. */
(function () {
  var ui, fmt;
  var state = { busy: false, ran: false };
  var catalogue = [];

  // Starting values for the probability inputs a and b (form presets, not results).
  var QUERY_PRESETS = {
    normal: { a: -1.96, b: 1.96 }, uniform: { a: 0.25, b: 0.75 }, exponential: { a: 1, b: 2 },
    binomial: { a: 8, b: 12 }, poisson: { a: 5, b: 7 }
  };

  function buildParams(form, distId) {
    var spec = catalogue.filter(function (d) { return d.id === distId; })[0];
    var holder = document.getElementById('dist-params');
    holder.replaceChildren();
    if (!spec) return;
    spec.params.forEach(function (p) {
      var id = 'dist-param-' + p.name;
      var range = [p.min != null ? 'min ' + p.min : null, p.max != null ? 'max ' + p.max : null].filter(Boolean).join(', ');
      var input = ui.el('input', { id: id, name: 'params.' + p.name, type: 'number', step: p.integer ? '1' : 'any',
        min: p.min, max: p.max, value: p.default, required: true });
      holder.appendChild(ui.el('div', { class: 'field' }, [
        ui.el('label', { for: id }, [p.label + ' ', range ? ui.el('span', { class: 'unit', text: range }) : null]),
        input
      ]));
    });
    var preset = QUERY_PRESETS[distId] || { a: 1, b: 2 };
    form.querySelector('[name="query.a"]').value = preset.a;
    form.querySelector('[name="query.b"]').value = preset.b;
  }

  function toggleB(form) {
    document.getElementById('dist-b-field').hidden = form.querySelector('[name="query_type"]').value !== 'between';
  }

  function inRegion(x, q) {
    if (!q) return false;
    if (q.type === 'less_equal') return x <= q.a;
    if (q.type === 'greater') return x > q.a;
    return x >= q.a && x <= q.b;
  }

  function render(d, root) {
    var t = d.theoretical, c = FX.charts.colors(), q = d.probability;
    var params = Object.keys(d.parameters).map(function (k) { return k + ' = ' + d.parameters[k]; }).join(', ');
    root.appendChild(ui.el('p', { class: 'meta', text: d.label + ' distribution (' + params + '). ' + d.formula }));
    root.appendChild(ui.cards([
      q ? { label: q.expression, value: fmt.sig(q.probability, 6), sub: fmt.prob(q.probability, 4) +
            (q.point_probability != null ? ' · P(X = ' + q.a + ') = ' + fmt.sig(q.point_probability, 5) : '') } : null,
      d.quantile_query ? { label: 'Quantile x for p = ' + d.quantile_query.p, value: fmt.sig(d.quantile_query.value, 6), sub: d.quantile_query.expression } : null,
      { label: 'Mean E[X]', value: fmt.sig(t.mean, 6) },
      { label: 'Variance Var(X)', value: fmt.sig(t.variance, 6), sub: 'SD ' + fmt.sig(t.sd, 6) },
      { label: 'Median', value: fmt.sig(t.median, 6), sub: t.mode != null ? 'mode ' + fmt.sig(t.mode, 6) : 'mode: every value in [a, b]' },
      { label: 'Skewness', value: fmt.sig(t.skewness, 4), sub: 'excess kurtosis ' + fmt.sig(t.excess_kurtosis, 4) }
    ]));

    var cur = d.curve;
    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'dist-pdf-chart', title: d.discrete ? 'Probability mass function P(X = k)' : 'Probability density function f(x)',
      note: (q ? 'Shaded: the region of ' + q.expression + '. ' : '') + (cur.thinned ? 'Support thinned for display.' : '') }));
    if (d.discrete) {
      FX.charts.render('dist-pdf-chart', {
        type: 'bar',
        data: { datasets: [{ label: 'P(X = k)', data: FX.charts.xy(cur.x, cur.density),
          backgroundColor: cur.x.map(function (x) { return inRegion(x, q) ? c.s2 : c.s1; }),
          barPercentage: 0.9, categoryPercentage: 1 }] },
        options: { plugins: { legend: { display: false } },
          scales: { x: FX.charts.axis('k', { extra: { offset: true } }), y: FX.charts.axis('Probability', { extra: { beginAtZero: true } }) } }
      });
    } else {
      var shaded = cur.x.map(function (x, i) { return inRegion(x, q) ? cur.density[i] : null; });
      FX.charts.render('dist-pdf-chart', {
        type: 'line',
        data: { datasets: [
          { label: 'Region of ' + (q ? q.expression : ''), data: cur.x.map(function (x, i) { return { x: x, y: shaded[i] }; }),
            borderWidth: 0, backgroundColor: FX.charts.alpha(c.s2, 0.35), fill: 'origin', spanGaps: false, excludeFromTable: true, legendHidden: !q },
          { label: 'f(x)', data: FX.charts.xy(cur.x, cur.density), borderColor: c.s1 }
        ] },
        options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
          scales: { x: FX.charts.axis('x'), y: FX.charts.axis('Density', { extra: { beginAtZero: true } }) } }
      });
    }
    grid.appendChild(ui.chartCard({ id: 'dist-cdf-chart', title: 'Cumulative distribution function F(x) = P(X ≤ x)' }));
    FX.charts.render('dist-cdf-chart', {
      type: 'line',
      data: { datasets: [{ label: 'F(x)', data: FX.charts.xy(cur.x, cur.cdf), borderColor: c.s1, stepped: d.discrete ? 'after' : false }] },
      options: { plugins: { legend: { display: false } },
        scales: { x: FX.charts.axis(d.discrete ? 'k' : 'x'), y: FX.charts.axis('Cumulative probability', { extra: { min: 0, max: 1 } }) } }
    });

    root.appendChild(ui.table({ caption: 'Quantiles: smallest x with P(X ≤ x) ≥ p',
      columns: [{ key: 'probability', label: 'p', num: true, format: function (x) { return fmt.num(x, 2); } },
                { key: 'value', label: 'Quantile', num: true, format: function (x) { return fmt.sig(x, 6); } }],
      rows: d.quantiles }));

    if (d.sample) {
      var s = d.sample.summary;
      root.appendChild(ui.section('Random sample drawn in R (n = ' + fmt.int(s.n) + ', seed ' + s.seed + ')', [
        ui.table({ caption: 'Sample statistics vs theory',
          columns: [{ key: 'm', label: 'Measure' }, { key: 's', label: 'Sample', num: true }, { key: 't', label: 'Theory', num: true }],
          rows: [
            { m: 'Mean', s: fmt.sig(s.mean, 6), t: fmt.sig(t.mean, 6) },
            { m: 'Variance', s: fmt.sig(s.variance, 6), t: fmt.sig(t.variance, 6) },
            { m: 'Standard deviation', s: fmt.sig(s.sd, 6), t: fmt.sig(t.sd, 6) },
            { m: 'Skewness', s: fmt.sig(s.skewness, 4), t: fmt.sig(t.skewness, 4) },
            { m: 'Standard error of the sample mean', s: fmt.sig(s.standard_error_of_mean, 4), t: '' }
          ] })
      ]));
      root.appendChild(ui.chartCard({ id: 'dist-sample-chart', title: 'Sample vs theoretical distribution',
        note: 'As n grows, the sample histogram approaches the theoretical curve (Law of Large Numbers).' }));
      if (d.sample.discrete_frequencies) {
        var f = d.sample.discrete_frequencies;
        FX.charts.render('dist-sample-chart', {
          type: 'bar',
          data: { datasets: [
            { type: 'bar', label: 'Sample share', data: FX.charts.xy(f.map(function (r) { return r.value; }), f.map(function (r) { return r.observed_share; })),
              backgroundColor: c.s1, barPercentage: 0.9, categoryPercentage: 1, order: 2 },
            { type: 'line', label: 'Theoretical P(X = k)', data: FX.charts.xy(f.map(function (r) { return r.value; }), f.map(function (r) { return r.expected_share; })),
              borderColor: c.s2, backgroundColor: c.s2, pointRadius: 3, borderWidth: 2, order: 1 }
          ] },
          options: { scales: { x: FX.charts.axis('k', { extra: { offset: true } }), y: FX.charts.axis('Proportion', { extra: { beginAtZero: true } }) } }
        });
      } else {
        var h = d.sample.histogram;
        FX.charts.histogram('dist-sample-chart', h, {
          xTitle: 'x', barLabel: 'Sample density',
          curve: d.discrete ? null : { x: cur.x, y: cur.density }, curveLabel: 'Theoretical density f(x)'
        });
      }
    }
  }

  function run(form) {
    var body = ui.readForm(form);
    body.query = { type: body.query_type, a: body.query ? body.query.a : undefined, b: body.query ? body.query.b : undefined };
    delete body.query_type;
    return ui.runAnalysis({
      state: state, form: form, results: document.getElementById('dist-results'), loadingText: 'R is calculating…',
      request: function () { return FX.api.post('/distribution', body); },
      render: render
    });
  }

  FX.sections.register('distributions', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt;
      var form = document.getElementById('dist-form');
      var select = form.querySelector('[name="distribution"]');
      select.addEventListener('change', function () { buildParams(form, select.value); ui.clearFieldErrors(form); });
      form.querySelector('[name="query_type"]').addEventListener('change', function () { toggleB(form); });
      form.addEventListener('submit', function (e) { e.preventDefault(); run(form); });
    },
    // Load the distribution catalogue from R (retried on every visit until it succeeds).
    show: function () {
      if (catalogue.length) return;
      var form = document.getElementById('dist-form');
      var select = form.querySelector('[name="distribution"]');
      FX.defaults().then(function (defs) {
        catalogue = defs.distributions;
        select.replaceChildren();
        catalogue.forEach(function (d) { select.appendChild(ui.el('option', { value: d.id, text: d.label })); });
        buildParams(form, select.value);
        toggleB(form);
        if (!state.ran) { state.ran = true; run(form); }
      }).catch(function (err) {
        document.getElementById('dist-results').replaceChildren(ui.banner('error', null, [err.message]));
      });
    }
  });
})();
