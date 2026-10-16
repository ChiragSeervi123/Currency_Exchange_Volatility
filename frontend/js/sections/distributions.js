/* distributions.js - Probability for the uploaded chart (computed by R; this file only draws it):
   basic and conditional probability, Bayes' theorem, the joint distribution of consecutive candles,
   and the binomial, Poisson and normal models. */
(function () {
  var ui, fmt;
  var state = { version: -1 };

  function pct(x) { return fmt.prob(x, 1); }

  function formula(text) { return ui.el('p', { class: 'formula', text: text }); }

  function jointTable(j) {
    var names = ['Up', 'Down'];
    var head = ui.el('tr', {}, [ui.el('th', { scope: 'col', text: 'Previous candle ↓ / Current candle →' })]
      .concat(names.map(function (n) { return ui.el('th', { class: 'num', scope: 'col', text: n }); }))
      .concat([ui.el('th', { class: 'num', scope: 'col', text: 'Marginal P(previous)' })]));
    var rows = names.map(function (n, i) {
      return ui.el('tr', {}, [ui.el('th', { scope: 'row', text: n })]
        .concat(names.map(function (m, k) {
          return ui.el('td', { class: 'num', text: fmt.num(j.probabilities[i][k], 4) + ' (' + j.counts[i][k] + ')' });
        }))
        .concat([ui.el('td', { class: 'num', text: fmt.num(j.marginal_previous[i], 4) })]));
    });
    rows.push(ui.el('tr', {}, [ui.el('th', { scope: 'row', text: 'Marginal P(current)' })]
      .concat(j.marginal_current.map(function (x) { return ui.el('td', { class: 'num', text: fmt.num(x, 4) }); }))
      .concat([ui.el('td', { class: 'num', text: '1.0000' })])));
    return ui.el('div', { class: 'table-wrap' }, [ui.el('table', {}, [
      ui.el('caption', { text: 'Joint probability distribution of (previous candle, current candle) — ' + j.n_pairs + ' pairs (counts in brackets)' }),
      ui.el('thead', {}, [head]), ui.el('tbody', {}, rows)])]);
  }

  function compareBars(id, labels, observed, theory, xTitle, theoryLabel) {
    var c = FX.charts.colors();
    var sets = [];
    if (observed) sets.push({ label: 'Observed on the chart', data: observed.map(function (x) { return 100 * x; }), backgroundColor: c.s1, barPercentage: 0.9, categoryPercentage: 0.75 });
    sets.push({ label: theoryLabel, data: theory.map(function (x) { return 100 * x; }), backgroundColor: c.s2, barPercentage: 0.9, categoryPercentage: 0.75 });
    FX.charts.render(id, {
      type: 'bar', data: { labels: labels, datasets: sets },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: { tooltip: { callbacks: { label: function (item) { return item.dataset.label + ': ' + fmt.num(item.parsed.y, 2) + '%'; } } } },
        scales: { x: FX.charts.axis(xTitle, { type: 'category' }), y: FX.charts.axis('Probability (%)', { extra: { beginAtZero: true } }) }
      }
    });
  }

  function render(a, root) {
    var p = a.probability, c = p.conditional, b = p.bayes, ind = p.independence;
    root.appendChild(ui.el('p', { class: 'meta', text: a.chart.name + ' · ' + fmt.int(p.n) + ' moves between consecutive candles: ' +
      p.n_up + ' up, ' + p.n_down + ' down' + (p.n_flat ? ', ' + p.n_flat + ' flat' : '') }));

    // Unit 1: basic probability
    root.appendChild(ui.section('1. Probability, conditional probability and Bayes\' theorem', [
      formula('P(A) = favourable cases / total cases      P(A | B) = P(A ∩ B) / P(B)      P(B | A) = P(A | B) · P(B) / P(A)'),
      ui.cards([
        { label: 'P(Up) = p', value: pct(p.p_up), sub: p.n_up + ' of ' + (p.n_up + p.n_down) + ' moves' },
        { label: 'P(Down) = q = 1 − p', value: pct(p.p_down), sub: p.n_down + ' of ' + (p.n_up + p.n_down) + ' moves' },
        { label: 'P(Up | previous Up)', value: pct(c.p_up_given_up), sub: 'conditional probability' },
        { label: 'P(Up | previous Down)', value: pct(c.p_up_given_down), sub: 'conditional probability' },
        { label: 'Bayes: P(previous Up | Up)', value: pct(b.result),
          sub: fmt.num(b.p_up_given_up, 3) + ' × ' + fmt.num(b.p_prev_up, 3) + ' / ' + fmt.num(b.p_up_now, 3) },
        { label: 'P(move > 1 SD)', value: pct(p.big_moves.p_big),
          sub: p.big_moves.p_big_after_big != null ? 'after a big move: ' + pct(p.big_moves.p_big_after_big) : '' }
      ]),
      ui.el('p', { class: 'interpretation', text: 'Independence check: P(Up before ∩ Up now) = ' + fmt.num(ind.p_joint_up_up, 4) +
        ' and P(Up before) × P(Up now) = ' + fmt.num(ind.product, 4) + '. ' +
        (Math.abs(ind.difference) < 0.02 ? 'They are almost equal, so consecutive candles behave close to independent events.'
          : 'They differ by ' + fmt.num(Math.abs(ind.difference), 4) + ', so consecutive candles are not exactly independent (the chi-square test on the Hypothesis page checks whether this is significant).') }),
      p.flat_note ? ui.el('p', { class: 'chart-note', text: p.flat_note }) : null
    ]));

    // Unit 3: bivariate distribution
    var j = p.joint;
    root.appendChild(ui.section('2. Bivariate (joint) distribution of consecutive candles', [
      formula('Joint P(X = x, Y = y)      Marginal P(X = x) = Σᵧ P(x, y)      Conditional P(Y = y | X = x) = P(x, y) / P(X = x)'),
      jointTable(j),
      ui.el('p', { class: 'interpretation', text: 'Conditional distribution of the current candle: after an up candle, P(Up) = ' + pct(c.p_up_given_up) +
        ' and P(Down) = ' + pct(c.p_down_given_up) + '; after a down candle, P(Up) = ' + pct(c.p_up_given_down) + ' and P(Down) = ' + pct(c.p_down_given_down) + '.' })
    ]));

    // Unit 2: binomial
    var bi = p.binomial;
    var binGrid = ui.el('div', { class: 'grid-2' });
    root.appendChild(ui.section('3. Binomial distribution: up candles among the next ' + bi.m, [
      formula('X ~ B(n = ' + bi.m + ', p = ' + fmt.num(bi.p, 4) + ')      P(X = k) = C(n, k) pᵏ qⁿ⁻ᵏ      E[X] = np      Var(X) = npq'),
      ui.cards([
        { label: 'Mean np', value: fmt.num(bi.mean, 3), sub: 'expected up candles' },
        { label: 'Variance npq', value: fmt.num(bi.variance, 3), sub: 'SD ' + fmt.num(bi.sd, 3) },
        { label: 'Skewness (q − p) / √npq', value: fmt.num(bi.skewness, 4) },
        { label: 'Kurtosis 3 + (1 − 6pq) / npq', value: fmt.num(bi.kurtosis, 4) },
        { label: 'P(X ≥ 6)', value: pct(bi.p_at_least_6), sub: 'a majority of up candles' },
        { label: 'Most likely k', value: String(bi.mode_k) }
      ]),
      binGrid
    ]));
    binGrid.appendChild(ui.chartCard({ id: 'prob-binom-chart', title: 'Binomial probabilities P(X = k)',
      note: bi.observed_share ? 'Observed: the share of the chart\'s ' + bi.n_blocks + ' consecutive blocks of ' + bi.m + ' candles with k up candles.' : 'Too few blocks of ' + bi.m + ' candles on the chart to show observed shares.' }));
    compareBars('prob-binom-chart', bi.k.map(String), bi.observed_share, bi.pmf, 'k = number of up candles in ' + bi.m, 'Binomial model');

    // Unit 2: Poisson
    var po = p.poisson;
    if (po) {
      var poisGrid = ui.el('div', { class: 'grid-2' });
      root.appendChild(ui.section('4. Poisson distribution: rare extreme moves', [
        formula('X ~ P(λ = ' + fmt.num(po.lambda, 4) + ')      P(X = k) = e^(−λ) λᵏ / k!      E[X] = Var(X) = λ      skewness = 1/√λ      kurtosis = 3 + 1/λ'),
        ui.cards([
          { label: 'λ (mean per block)', value: fmt.num(po.lambda, 4) },
          { label: 'Sample variance of counts', value: fmt.num(po.sample_variance, 4), sub: 'Poisson: equal to λ' },
          { label: 'P(no extreme move)', value: pct(po.p_zero), sub: 'P(X = 0)' },
          { label: 'P(at least one)', value: pct(po.p_at_least_1) },
          { label: 'Skewness 1/√λ', value: fmt.num(po.skewness, 4) },
          { label: 'Kurtosis 3 + 1/λ', value: fmt.num(po.kurtosis, 4) }
        ]),
        poisGrid
      ]));
      poisGrid.appendChild(ui.chartCard({ id: 'prob-pois-chart', title: 'Poisson: extreme moves per ' + po.block + ' candles',
        note: 'An extreme move is a return more than 2 SD from the mean. λ = ' + fmt.num(po.lambda, 3) + ' per block, from ' + po.n_blocks + ' blocks.' }));
      compareBars('prob-pois-chart', po.k.map(String), po.observed_share, po.pmf, 'k = extreme moves in a block', 'Poisson model');
    }

    // Unit 2: normal
    var no = p.normal;
    var normGrid = ui.el('div', { class: 'grid-2' });
    root.appendChild(ui.section((po ? '5' : '4') + '. Normal distribution of the returns', [
      formula('R ~ N(μ = ' + fmt.num(no.mean, 4) + '%, σ = ' + fmt.num(no.sd, 4) + '%)      Z = (R − μ) / σ      M(t) = exp(μt + σ²t²/2)      skewness = 0, kurtosis = 3'),
      ui.cards([
        { label: 'Mean μ', value: fmt.num(no.mean, 4) + '%' },
        { label: 'Variance σ²', value: fmt.sig(no.variance, 4) },
        { label: 'P(R > 0) under the normal', value: pct(no.comparisons[0].normal), sub: 'observed ' + pct(no.comparisons[0].observed) },
        { label: 'Moment generating function', value: 'M(t)', sub: no.mgf }
      ]),
      normGrid
    ]));
    normGrid.appendChild(ui.chartCard({ id: 'prob-norm-chart', title: 'Normal model vs the chart', note: 'Probabilities of events for a return, under N(μ, σ²) and as observed.' }));
    compareBars('prob-norm-chart', no.comparisons.map(function (x) { return x.event; }), no.comparisons.map(function (x) { return x.observed; }),
                no.comparisons.map(function (x) { return x.normal; }), 'Event', 'Normal model');
  }

  FX.sections.register('distributions', {
    init: function () { ui = FX.ui; fmt = FX.fmt; },
    show: function () { return FX.chart.renderInto('prob-results', state, render); }
  });
})();
