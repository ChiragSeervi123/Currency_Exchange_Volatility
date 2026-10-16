/* hypothesis.js - Hypothesis Testing for the uploaded chart (computed by R; this file only draws it):
   the general procedure and formulas, point and interval estimates, every test with its decision,
   and "Our hypothesis" - the outlook (up, down or close to the mean line) built from the results. */
(function () {
  var ui, fmt;
  var state = { version: -1 };

  function formula(text) { return ui.el('p', { class: 'formula', text: text }); }

  function nullChart(id, ch) {
    var c = FX.charts.colors();
    var crit = ch.critical;
    function rejected(x) {
      if (ch.alternative === 'two.sided') return x <= crit[0] || x >= crit[1];
      if (ch.alternative === 'less') return x <= crit[0];
      return x >= crit[0];
    }
    var maxD = Math.max.apply(null, ch.density.filter(function (v) { return v != null; }));
    var sets = [
      { label: 'Critical (rejection) region, α = 5%', data: ch.x.map(function (x, i) { return { x: x, y: rejected(x) ? ch.density[i] : null }; }),
        borderWidth: 0, backgroundColor: FX.charts.alpha(c.s2, 0.45), fill: 'origin', spanGaps: false, excludeFromTable: true },
      { label: 'Distribution under H0', data: FX.charts.xy(ch.x, ch.density), borderColor: c.text2, borderWidth: 1.5 }
    ];
    if (!ch.observed_off_scale) {
      sets.push({ label: 'Observed = ' + fmt.sig(ch.observed, 4), data: [{ x: ch.observed, y: 0 }, { x: ch.observed, y: maxD }], borderColor: c.s1, borderWidth: 2.5 });
    }
    var name = { z: 'Z (standard normal)', t: 't', F: 'F', chisq: 'χ²' }[ch.distribution];
    FX.charts.render(id, {
      type: 'line', data: { datasets: sets },
      options: { plugins: { legend: { labels: { boxWidth: 12 } } },
        scales: { x: FX.charts.axis(name + (ch.df1 != null ? ', df = ' + fmt.sig(ch.df1, 4) + (ch.df2 != null ? ', ' + fmt.sig(ch.df2, 4) : '') : '')),
                  y: FX.charts.axis('Density', { extra: { beginAtZero: true } }) } }
    });
  }

  function testCard(t, i) {
    var crit = t.critical.map(function (x) { return fmt.num(x, 3); }).join(' and ');
    var card = ui.el('article', { class: 'test-card' + (t.reject ? ' is-reject' : ' is-keep') }, [
      ui.el('header', { class: 'test-head' }, [
        ui.el('h3', { text: (i + 1) + '. ' + t.title }),
        ui.el('span', { class: 'badge ' + (t.reject ? 'badge-reject' : 'badge-keep'), text: t.decision })
      ]),
      formula(t.formula),
      ui.el('div', { class: 'hypotheses' }, [ui.el('p', { text: t.h0 }), ui.el('p', { text: t.h1 + '   (' + t.tails + ')' })]),
      ui.el('dl', { class: 'test-numbers' }, [
        ui.el('div', {}, [ui.el('dt', { text: 'Calculated ' + t.statistic_name }), ui.el('dd', { text: fmt.num(t.statistic, 4) })]),
        ui.el('div', {}, [ui.el('dt', { text: 'Critical value' + (t.critical.length > 1 ? 's' : '') }), ui.el('dd', { text: crit })]),
        ui.el('div', {}, [ui.el('dt', { text: 'p-value' }), ui.el('dd', { text: fmt.pval(t.p_value) })]),
        t.df != null ? ui.el('div', {}, [ui.el('dt', { text: 'Degrees of freedom' }), ui.el('dd', { text: typeof t.df === 'number' ? fmt.sig(t.df, 4) : t.df })]) : null
      ]),
      ui.el('p', { class: 'test-conclusion', text: (t.reject ? 'In the critical region (p < α): ' : 'Not in the critical region (p ≥ α): ') + t.conclusion })
    ]);
    if (t.note) card.appendChild(ui.el('p', { class: 'chart-note', text: t.note }));
    var extra = detailsFor(t);
    if (extra) card.appendChild(extra);
    if (t.chart) {
      var id = 'hyp-null-' + t.id;
      card.appendChild(ui.el('div', { class: 'chart-box chart-box-sm' }, [ui.el('canvas', { id: id, role: 'img', 'aria-label': 'Distribution of the test statistic under H0 for ' + t.title })]));
    }
    return card;
  }

  function detailsFor(t) {
    var d = t.details || {};
    var s4 = function (x) { return fmt.sig(x, 4); };
    if (d.classes) {
      return ui.el('details', { class: 'table-twin' }, [ui.el('summary', { text: 'Observed and expected frequencies' }),
        ui.table({ compact: true, columns: [{ key: 'z_class', label: 'Class (z = (r − x̄) / s)' }, { key: 'observed', label: 'O', num: true, format: fmt.int },
          { key: 'expected', label: 'E', num: true, format: function (x) { return fmt.num(x, 2); } },
          { key: 'contribution', label: '(O − E)² / E', num: true, format: function (x) { return fmt.num(x, 3); } }], rows: d.classes })]);
    }
    if (d.groups) {
      return ui.el('details', { class: 'table-twin' }, [ui.el('summary', { text: 'Periods and ANOVA table' }),
        ui.table({ compact: true, columns: [{ key: 'period', label: 'Period' }, { key: 'n', label: 'n', num: true, format: fmt.int },
          { key: 'mean', label: 'Mean (%)', num: true, format: s4 }, { key: 'sd', label: 'SD (%)', num: true, format: s4 }], rows: d.groups }),
        ui.el('p', { class: 'chart-note', text: 'SSB = ' + s4(d.ss_between) + ', SSW = ' + s4(d.ss_within) + ', MSB = ' + s4(d.ms_between) + ', MSW = ' + s4(d.ms_within) + '.' })]);
    }
    if (d.observed && d.expected) {
      var row = function (lab, i) { return lab + ': O = ' + d.observed[i].join(', ') + ' · E = ' + d.expected[i].map(function (x) { return fmt.num(x, 1); }).join(', '); };
      return ui.el('p', { class: 'chart-note', text: 'Rows previous Up / Down, columns current Up / Down. ' + row('Up', 0) + '; ' + row('Down', 1) + '.' });
    }
    if (d.n1 != null) {
      return ui.el('p', { class: 'chart-note', text: 'First half: n₁ = ' + d.n1 + ', x̄₁ = ' + s4(d.mean1) + '%, s₁ = ' + s4(d.sd1) +
        '%. Second half: n₂ = ' + d.n2 + ', x̄₂ = ' + s4(d.mean2) + '%, s₂ = ' + s4(d.sd2) + '%.' });
    }
    if (d.p_hat != null) {
      return ui.el('p', { class: 'chart-note', text: 'n = ' + d.n + ' moves, p̂ = ' + fmt.num(d.p_hat, 4) + ', q̂ = ' + fmt.num(d.q_hat, 4) +
        ', standard error √(P₀Q₀/n) = ' + fmt.num(d.standard_error, 4) + '.' });
    }
    if (d.mean != null) {
      return ui.el('p', { class: 'chart-note', text: 'n = ' + d.n + ', x̄ = ' + s4(d.mean) + '%, s = ' + s4(d.sd) + '%' +
        (d.standard_error != null ? ', s/√n = ' + s4(d.standard_error) + '%' : '') + '.' });
    }
    return null;
  }

  function projectionChart(id, a) {
    var o = a.hypothesis.outlook, s = a.series, c = FX.charts.colors();
    var n = s.close.length, k = Math.min(n, 60);
    var hist = s.close.slice(n - k);
    var labels = [];
    for (var i = n - k + 1; i <= n + o.horizon; i++) labels.push(String(i));
    var pad = function (arr, before) { return new Array(before).fill(null).concat(arr); };
    FX.charts.render(id, {
      type: 'line',
      data: { labels: labels, datasets: [
        { label: 'Prices read from the chart', data: hist, borderColor: c.s1, borderWidth: 2 },
        { label: '95% range (lower)', data: pad(o.projection.low, k - 1), borderColor: 'transparent', pointRadius: 0, legendHidden: true },
        { label: '95% range', data: pad(o.projection.high, k - 1), borderColor: 'transparent', backgroundColor: FX.charts.alpha(c.s2, 0.22), fill: '-1', pointRadius: 0 },
        { label: 'Projected median', data: pad(o.projection.median, k - 1), borderColor: c.s2, borderWidth: 2, borderDash: [6, 4] },
        { label: 'Mean line', data: labels.map(function () { return o.mean_price; }), borderColor: c.text2, borderWidth: 1, borderDash: [2, 4], pointRadius: 0 }
      ] },
      options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
        scales: { x: FX.charts.axis('Candle', { type: 'category', maxTicks: 10 }),
                  y: FX.charts.axis('Price', { tickFormat: function (v) { return fmt.price(v); } }) } }
    });
  }

  function render(a, root) {
    var h = a.hypothesis, e = h.estimation, o = h.outlook;
    root.appendChild(ui.el('p', { class: 'meta', text: a.chart.name + ' · ' + fmt.int(e.n) + ' returns per candle · significance level α = ' + h.alpha }));

    root.appendChild(ui.section('How every test works', [
      ui.el('ol', { class: 'steps' }, [
        ui.el('li', { text: 'State the null hypothesis H₀ (no effect) and the alternative H₁ (one-tailed or two-tailed).' }),
        ui.el('li', { text: 'Choose the level of significance α = 0.05: the probability of a Type I error (rejecting a true H₀). A Type II error is keeping a false H₀.' }),
        ui.el('li', { text: 'Compute the test statistic from the chart: Z = (estimate − hypothesised value) / standard error (large samples, n ≥ 30), or t, F, χ².' }),
        ui.el('li', { text: 'Compare it with the critical value(s): if it falls in the critical region (equivalently p-value < α), reject H₀; otherwise fail to reject H₀.' })
      ])
    ]));

    root.appendChild(ui.section('Point and interval estimation', [
      ui.cards([
        { label: 'Point estimate of μ: x̄', value: fmt.num(e.mean, 4) + '%', sub: 'mean return per candle' },
        { label: 'Point estimate of σ: s', value: fmt.num(e.sd, 4) + '%', sub: 'standard error s/√n = ' + fmt.num(e.standard_error, 4) },
        { label: '95% confidence interval for μ', value: fmt.num(e.ci_mean[0], 4) + ' to ' + fmt.num(e.ci_mean[1], 4), sub: e.ci_method },
        { label: 'Point estimate of P(up): p̂', value: fmt.num(e.p_hat, 4), sub: 'q̂ = 1 − p̂ = ' + fmt.num(e.q_hat, 4) },
        { label: '95% confidence interval for P', value: fmt.num(e.ci_p[0], 4) + ' to ' + fmt.num(e.ci_p[1], 4), sub: 'p̂ ± 1.96 √(p̂q̂/n), n = ' + e.n_moves }
      ])
    ]));

    var grid = ui.el('div', { class: 'test-grid' });
    root.appendChild(ui.section('Tests on the chart (α = 0.05)', [grid]));
    h.tests.forEach(function (t, i) { grid.appendChild(testCard(t, i)); });
    h.tests.forEach(function (t) { if (t.chart) nullChart('hyp-null-' + t.id, t.chart); });

    // Our hypothesis
    var label = { up: 'UP', down: 'DOWN', sideways: 'CLOSE TO THE MEAN LINE' }[o.verdict];
    var icon = { up: 'i-trending-up', down: 'i-trending-down', sideways: 'i-move-horizontal' }[o.verdict];
    var verdict = ui.el('div', { class: 'verdict verdict-lg verdict-' + o.verdict }, [
      ui.el('span', { class: 'verdict-icon', 'aria-hidden': 'true' }),
      ui.el('div', {}, [
        ui.el('p', { class: 'verdict-label', text: 'Prediction from the graph: ' + label + ' (' + o.strength + ' evidence)' }),
        ui.el('p', { class: 'verdict-text', text: o.headline + '.' })
      ])
    ]);
    verdict.querySelector('.verdict-icon').innerHTML = '<svg class="icon"><use href="assets/icons.svg?v=3#' + icon + '"/></svg>';
    var projGrid = ui.el('div', { class: 'grid-2' });
    root.appendChild(ui.el('section', { class: 'panel glass outlook-panel', id: 'our-hypothesis' }, [
      ui.el('h2', { class: 'panel-title', text: 'Our hypothesis' }), verdict,
      ui.cards([
        { label: 'Last price', value: fmt.price(o.last_price) },
        { label: 'Expected after ' + o.horizon + ' candles', value: fmt.price(o.expected_price), sub: 'median of the projection' },
        { label: '95% range after ' + o.horizon + ' candles', value: fmt.price(o.range_low) + ' – ' + fmt.price(o.range_high) },
        { label: 'P(price higher after ' + o.horizon + ')', value: fmt.prob(o.p_higher, 1), sub: 'Φ(x̄√h / s)' },
        { label: 'Mean line', value: fmt.price(o.mean_price), sub: 'last price ' + fmt.num(o.z_mean, 2) + ' SD ' + (o.z_mean >= 0 ? 'above' : 'below') }
      ]),
      projGrid,
      ui.el('p', { class: 'chart-note', text: o.method }),
      ui.el('p', { class: 'meta', text: o.caveat })
    ]));
    projGrid.appendChild(ui.chartCard({ id: 'hyp-projection-chart', title: 'Last candles and the projected range', note: 'Dashed: projected median path; band: 95% range under the random-walk model with the estimated drift and volatility.' }));
    projGrid.appendChild(ui.el('div', { class: 'insight-lists' }, [
      ui.el('h3', { class: 'subhead', text: 'Why (from the calculations)' }),
      ui.el('ul', { class: 'list-plain' }, o.reasons.map(function (t) { return ui.el('li', { text: t }); })),
      ui.el('h3', { class: 'subhead', text: 'Insights from the graph' }),
      ui.el('ul', { class: 'list-plain' }, (o.insights || []).map(function (t) { return ui.el('li', { text: t }); }))
    ]));
    projectionChart('hyp-projection-chart', a);
  }

  FX.sections.register('hypothesis', {
    init: function () { ui = FX.ui; fmt = FX.fmt; },
    show: function () { return FX.chart.renderInto('hyp-results', state, render); }
  });
})();
