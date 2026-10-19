/* report.js - the Report page: the whole analysis of the uploaded chart written as one document
   (cover, chart reading, descriptive statistics, probability, hypothesis tests, our hypothesis,
   regression, limitations). It can be downloaded as a standalone HTML file (charts and the
   screenshot embedded) or printed / saved as PDF. Everything shown was computed by R; this file only
   formats it. */
(function () {
  var ui, fmt, el;
  var state = { version: -1 };

  function p(text, cls) { return el('p', { class: cls || null, text: text }); }
  function section(title) { return el('h2', { text: title }); }
  function formula(text) { return el('div', { class: 'rp-formula', text: text }); }
  function stats(items) {
    return el('div', { class: 'rp-grid' }, items.filter(Boolean).map(function (it) {
      return el('div', { class: 'rp-stat' }, [el('span', { text: it[0] }), el('b', { text: it[1] }), it[2] ? el('span', { text: it[2] }) : null]);
    }));
  }
  // head: [label | {label, num}], rows: arrays of strings
  function table(caption, head, rows) {
    return el('table', {}, [
      caption ? el('caption', { text: caption }) : null,
      el('thead', {}, [el('tr', {}, head.map(function (h) { return el('th', { class: h.num ? 'num' : null, scope: 'col', text: h.label || h }); }))]),
      el('tbody', {}, rows.map(function (r) {
        return el('tr', {}, r.map(function (c, i) { return el('td', { class: head[i] && head[i].num ? 'num' : null, text: c }); }));
      }))
    ]);
  }
  function kv(caption, pairs) {
    return table(caption, ['Measure', { label: 'Value', num: true }], pairs.filter(Boolean));
  }
  function chartBox(id, label) { return el('div', { class: 'rp-chart' }, [el('canvas', { id: id, role: 'img', 'aria-label': label })]); }
  function sig(x) { return fmt.sig(x, 5); }
  function crit(t) { return t.critical.map(function (x) { return fmt.num(x, 3); }).join(' / '); }

  // ---- Charts (drawing only) ----------------------------------------------------------------
  function priceChart(a) {
    var s = a.series, c = FX.charts.colors(), candles = a.chart.type === 'candles';
    var sets = [];
    if (candles) {
      sets.push({ label: 'Low', data: s.low, borderColor: 'transparent', pointRadius: 0, legendHidden: true });
      sets.push({ label: 'High-low range', data: s.high, borderColor: 'transparent', backgroundColor: FX.charts.alpha(c.s1, 0.2), fill: '-1', pointRadius: 0 });
    }
    sets.push({ label: 'Close', data: s.close, borderColor: c.s1, borderWidth: 1.8, pointRadius: 0 });
    sets.push({ label: 'Mean line', data: s.close.map(function () { return a.overview.mean_price; }), borderColor: c.s2, borderWidth: 1.3, borderDash: [6, 4], pointRadius: 0 });
    FX.charts.render('rp-price', { type: 'line', data: { labels: s.index.map(String), datasets: sets },
      options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
        scales: { x: FX.charts.axis('Candle', { type: 'category', maxTicks: 10 }), y: FX.charts.axis('Price', { tickFormat: function (v) { return fmt.price(v); } }) } } });
  }
  function bandChart(a) {
    var bands = a.descriptive.sigma_bands, c = FX.charts.colors();
    FX.charts.render('rp-bands', { type: 'bar',
      data: { labels: bands.map(function (b) { return b.band; }), datasets: [
        { label: 'Observed (chart)', data: bands.map(function (b) { return 100 * b.observed_share; }), backgroundColor: c.s1 },
        { label: 'Normal distribution', data: bands.map(function (b) { return 100 * b.normal_share; }), backgroundColor: c.s2 }] },
      options: { scales: { x: FX.charts.axis('Distance from the mean', { type: 'category' }), y: FX.charts.axis('Share of returns (%)', { extra: { beginAtZero: true } }) } } });
  }
  function projectionChart(a) {
    var o = a.hypothesis.outlook, s = a.series, c = FX.charts.colors();
    var n = s.close.length, k = Math.min(n, 60), labels = [];
    for (var i = n - k + 1; i <= n + o.horizon; i++) labels.push(String(i));
    var pad = function (arr) { return new Array(k - 1).fill(null).concat(arr); };
    FX.charts.render('rp-projection', { type: 'line',
      data: { labels: labels, datasets: [
        { label: 'Prices read from the chart', data: s.close.slice(n - k), borderColor: c.s1, borderWidth: 2, pointRadius: 0 },
        { label: '95% range (lower)', data: pad(o.projection.low), borderColor: 'transparent', pointRadius: 0, legendHidden: true },
        { label: '95% range', data: pad(o.projection.high), borderColor: 'transparent', backgroundColor: FX.charts.alpha(c.s2, 0.25), fill: '-1', pointRadius: 0 },
        { label: 'Projected median', data: pad(o.projection.median), borderColor: c.s2, borderWidth: 2, borderDash: [6, 4], pointRadius: 0 },
        { label: 'Mean line', data: labels.map(function () { return o.mean_price; }), borderColor: c.text2, borderWidth: 1, borderDash: [2, 4], pointRadius: 0 }] },
      options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
        scales: { x: FX.charts.axis('Candle', { type: 'category', maxTicks: 10 }), y: FX.charts.axis('Price', { tickFormat: function (v) { return fmt.price(v); } }) } } });
  }

  // ---- The document ------------------------------------------------------------------------------------
  function render(a, root) {
    var c = a.chart, o = a.overview, d = a.descriptive, r = d.returns, pr = a.probability, h = a.hypothesis, out = h.outlook, g = a.regression;
    var unit = c.type === 'candles' ? 'candles' : 'points';
    var per = c.interval ? 'per ' + c.interval + ' candle' : 'per candle';
    var title = c.name + (c.interval && c.name.indexOf(c.interval) < 0 ? ' · ' + c.interval : '');

    root.appendChild(el('header', { class: 'rp-cover' }, [
      p('Chart Reader · Probability & Statistics report', 'rp-kicker'),
      el('h1', { text: 'Statistical analysis of ' + title }),
      p('Generated ' + c.analysed_at + ' from ' + (c.file_name || 'an uploaded chart screenshot') + '. All calculations were performed in R on prices read from the image.', 'rp-muted')
    ]));

    // 1. Summary
    root.appendChild(section('1. Summary'));
    root.appendChild(el('div', { class: 'rp-verdict is-' + out.verdict }, [
      el('strong', { text: out.headline }),
      el('span', { text: 'Strength of evidence: ' + out.strength + '. Expected price after ' + out.horizon + ' candles: ' + fmt.price(out.expected_price) +
        ' (95% range ' + fmt.price(out.range_low) + ' to ' + fmt.price(out.range_high) + '); probability that the price is higher: ' + fmt.prob(out.p_higher, 1) + '.' })
    ]));
    root.appendChild(stats([
      ['Last price', fmt.price(o.last)], ['Mean price', fmt.price(o.mean_price)], ['Total change', fmt.signedPct(o.change_pct)],
      ['Volatility (SD of returns)', fmt.num(o.sd_return, 3) + '%', per], ['Highest / lowest', fmt.price(o.high) + ' / ' + fmt.price(o.low)],
      ['Up / down candles', o.n_up + ' / ' + o.n_down]
    ]));

    // 2. The chart and how it was read
    root.appendChild(section('2. The chart and how it was read'));
    root.appendChild(el('img', { class: 'rp-shot', alt: 'The uploaded chart screenshot', src: FX.config.apiBase + '/chart/image?v=' + FX.chart.version() }));
    root.appendChild(el('ul', {}, [
      el('li', { text: fmt.int(c.n_points) + ' ' + unit + ' were read from a ' + c.image.width + ' × ' + c.image.height + ' px ' + c.image.format + ' image (' + c.image.theme + ' theme).' }),
      el('li', { text: c.scale.note }),
      el('li', { text: 'One pixel row of the chart ≈ ' + fmt.sig(c.resolution, 3) + ' in price, so every price is approximate to about that amount.' })
    ].concat((c.notes || []).slice(1).map(function (t) { return el('li', { text: t }); }))));
    root.appendChild(chartBox('rp-price', 'Prices read from the chart'));

    // 3. Descriptive statistics
    root.appendChild(section('3. Descriptive statistics'));
    root.appendChild(p('Returns per candle are r = 100 · ln(Pₜ / Pₜ₋₁) %. ' + d.shape_interpretation));
    root.appendChild(kv('Summary of the returns (%)', [
      ['Number of returns (n)', fmt.int(r.n)], ['Mean', sig(r.mean)], ['Median', sig(r.median)], ['Mode (grouped)', sig(r.mode.value)],
      ['Minimum / maximum', sig(r.min) + ' / ' + sig(r.max)], ['Range', sig(r.range)],
      ['Quartiles Q₁ / Q₃ (IQR)', sig(r.q1) + ' / ' + sig(r.q3) + ' (' + sig(r.iqr) + ')'],
      ['Sample variance s²', sig(r.variance)], ['Population variance σ²', sig(r.variance_population)],
      ['Standard deviation s', sig(r.sd)], ['Standard error s/√n', sig(r.standard_error)],
      ['Raw moments μ′₁ … μ′₄', r.moments.raw.map(function (x) { return fmt.sig(x, 4); }).join(' · ')],
      ['Central moments μ₁ … μ₄', r.moments.central.map(function (x) { return fmt.sig(x, 4); }).join(' · ')],
      ['Skewness γ₁ (normal = 0)', fmt.num(r.skewness, 4)], ['Kurtosis β₂ (normal = 3)', fmt.num(r.kurtosis, 4)],
      ['Prices: mean / median / mode', fmt.price(d.prices.mean) + ' / ' + fmt.price(d.prices.median) + ' / ' + fmt.price(d.prices.mode.value)],
      ['Prices: SD / coefficient of variation', fmt.price(d.prices.sd) + ' / ' + fmt.pct(d.prices.cv_pct, 2)]
    ]));
    root.appendChild(chartBox('rp-hist', 'Histogram of returns with the fitted normal curve'));
    root.appendChild(chartBox('rp-bands', 'Observed against normal frequencies'));

    // 4. Probability
    var bi = pr.binomial, po = pr.poisson, no = pr.normal, j = pr.joint, cd = pr.conditional;
    root.appendChild(section('4. Probability and distributions'));
    root.appendChild(stats([
      ['P(Up) = p', fmt.prob(pr.p_up, 1)], ['P(Down) = q', fmt.prob(pr.p_down, 1)],
      ['P(Up | previous Up)', fmt.prob(cd.p_up_given_up, 1)], ['P(Up | previous Down)', fmt.prob(cd.p_up_given_down, 1)],
      ['Bayes: P(previous Up | Up)', fmt.prob(pr.bayes.result, 1)]
    ]));
    root.appendChild(table('Joint distribution of (previous candle, current candle): ' + j.n_pairs + ' pairs',
      ['Previous \\ current', { label: 'Up', num: true }, { label: 'Down', num: true }, { label: 'Marginal', num: true }],
      [['Up', fmt.num(j.probabilities[0][0], 4), fmt.num(j.probabilities[0][1], 4), fmt.num(j.marginal_previous[0], 4)],
       ['Down', fmt.num(j.probabilities[1][0], 4), fmt.num(j.probabilities[1][1], 4), fmt.num(j.marginal_previous[1], 4)],
       ['Marginal', fmt.num(j.marginal_current[0], 4), fmt.num(j.marginal_current[1], 4), '1.0000']]));
    root.appendChild(kv('Fitted models', [
      ['Binomial B(10, ' + fmt.num(bi.p, 4) + '): mean np / variance npq', fmt.num(bi.mean, 3) + ' / ' + fmt.num(bi.variance, 3)],
      ['Binomial skewness / kurtosis', fmt.num(bi.skewness, 4) + ' / ' + fmt.num(bi.kurtosis, 4)],
      ['Binomial P(X ≥ 6)', fmt.prob(bi.p_at_least_6, 1)],
      po ? ['Poisson P(λ = ' + fmt.num(po.lambda, 4) + ') for extreme moves per ' + po.block + ' candles: P(0) / P(≥ 1)', fmt.prob(po.p_zero, 1) + ' / ' + fmt.prob(po.p_at_least_1, 1)] : null,
      ['Normal N(' + fmt.num(no.mean, 4) + '%, ' + fmt.sig(no.variance, 4) + '): P(R > 0) model / observed', fmt.prob(no.comparisons[0].normal, 1) + ' / ' + fmt.prob(no.comparisons[0].observed, 1)]
    ]));

    // 5. Hypothesis tests
    var e = h.estimation;
    root.appendChild(section('5. Testing of hypotheses (α = 0.05)'));
    root.appendChild(p('Point estimate of the mean return: ' + fmt.num(e.mean, 4) + '% per candle; 95% confidence interval ' + fmt.num(e.ci_mean[0], 4) + ' to ' +
      fmt.num(e.ci_mean[1], 4) + '. Proportion of up candles: ' + fmt.num(e.p_hat, 4) + ', 95% confidence interval ' + fmt.num(e.ci_p[0], 4) + ' to ' + fmt.num(e.ci_p[1], 4) + '.'));
    h.tests.forEach(function (t, i) {
      root.appendChild(el('div', { class: 'rp-test' }, [
        el('h3', { text: (i + 1) + '. ' + t.title }), formula(t.formula),
        p(t.h0 + '   |   ' + t.h1 + '   (' + t.tails + ')'),
        p('Calculated ' + t.statistic_name + ' = ' + fmt.num(t.statistic, 4) + '; critical value(s) ' + crit(t) + (t.df != null ? '; df = ' + t.df : '') + '; p-value ' + fmt.pval(t.p_value) + '.'),
        el('p', {}, [el('span', { class: t.reject ? 'reject' : 'keep', text: t.decision + '. ' }), t.conclusion])
      ]));
    });

    // 6. Our hypothesis
    root.appendChild(section('6. Our hypothesis'));
    root.appendChild(el('div', { class: 'rp-verdict is-' + out.verdict }, [el('strong', { text: out.headline }), el('span', { text: 'Strength of evidence: ' + out.strength + '.' })]));
    root.appendChild(el('h3', { text: 'Why (from the calculations)' }));
    root.appendChild(el('ul', {}, out.reasons.map(function (t) { return el('li', { text: t }); })));
    if (out.insights && out.insights.length) {
      root.appendChild(el('h3', { text: 'Insights from the graph' }));
      root.appendChild(el('ul', {}, out.insights.map(function (t) { return el('li', { text: t }); })));
    }
    root.appendChild(chartBox('rp-projection', 'Last candles and the projected range'));
    root.appendChild(p(out.method, 'rp-muted'));

    // 7. Correlation and regression
    var tr = g.trend, mu = g.multiple, lag = g.lag;
    root.appendChild(section('7. Correlation and regression'));
    root.appendChild(p('Least-squares line: ' + tr.equation + '. ' + tr.interpretation));
    root.appendChild(kv('Correlation and fit', [
      ['Pearson r (price, candle number)', fmt.num(tr.correlation.pearson_r, 4) + ' (' + tr.correlation.strength + ')'],
      ['Spearman rank correlation ρ', fmt.num(g.spearman.rho, 4)], ['R² (adjusted)', fmt.num(tr.fit.r_squared, 4) + ' (' + fmt.num(tr.fit.adj_r_squared, 4) + ')'],
      ['Slope b₁ (p-value)', sig(tr.coefficients[1].estimate) + ' (' + fmt.pval(tr.coefficients[1].p_value) + ')'],
      ['Multiple regression ' + mu.equation, 'R² = ' + fmt.num(mu.r_squared, 4)],
      ['Return on previous return: correlation (p-value)', fmt.num(lag.correlation.pearson_r, 4) + ' (' + fmt.pval(lag.correlation.pearson_p) + ')']
    ]));
    root.appendChild(p(mu.interpretation));
    root.appendChild(p(g.warnings.join(' '), 'rp-muted'));

    // 8. Limitations
    root.appendChild(section('8. Limitations'));
    root.appendChild(el('ul', {}, [
      'Prices are read from an image, so each is accurate only to about one pixel row; line charts are sampled every 6 pixels.',
      'The tests treat returns as independent; real prices show clustering and trends, so p-values are indicative.',
      '"Fail to reject H₀" does not prove that H₀ is true.',
      out.caveat
    ].map(function (t) { return el('li', { text: t }); })));
    root.appendChild(p('Report produced by Chart Reader (R, Plumber, Tesseract OCR). Nothing in this report is a live quote or investment advice.', 'rp-muted rp-foot'));

    // charts that need the elements to be in the page first
    priceChart(a);
    FX.charts.histogram('rp-hist', d.returns_histogram, { xTitle: 'Return per candle (%)', curve: d.normal_fit ? { x: d.normal_fit.x, y: d.normal_fit.density } : null,
      curveLabel: 'Fitted normal curve', barLabel: 'Observed density' });
    bandChart(a);
    projectionChart(a);
  }

  // ---- Download / print ----------------------------------------------------------------------------------
  function blobToDataUrl(blob) {
    return new Promise(function (res, rej) { var fr = new FileReader(); fr.onload = function () { res(fr.result); }; fr.onerror = rej; fr.readAsDataURL(blob); });
  }

  function standaloneHtml(a) {
    var root = document.getElementById('report-results').querySelector('.report');
    var clone = root.cloneNode(true);
    var src = root.querySelectorAll('canvas'), dst = clone.querySelectorAll('canvas');
    src.forEach(function (cv, i) {
      var tmp = document.createElement('canvas');
      tmp.width = cv.width; tmp.height = cv.height;
      var ctx = tmp.getContext('2d');
      ctx.fillStyle = '#16223f'; ctx.fillRect(0, 0, tmp.width, tmp.height); ctx.drawImage(cv, 0, 0);
      var img = document.createElement('img');
      img.className = 'rp-img'; img.alt = cv.getAttribute('aria-label') || 'Chart'; img.src = tmp.toDataURL('image/png');
      dst[i].parentNode.replaceWith(img);
    });
    var shot = clone.querySelector('.rp-shot');
    return Promise.all([
      fetch('report.css').then(function (r) { return r.text(); }),
      shot ? fetch(FX.config.apiBase + '/chart/image?v=' + FX.chart.version()).then(function (r) { return r.blob(); }).then(blobToDataUrl) : null
    ]).then(function (parts) {
      if (shot && parts[1]) shot.src = parts[1];
      var title = 'Chart Reader report - ' + a.chart.name;
      return '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">' +
        '<title>' + title.replace(/</g, '&lt;') + '</title><style>body{margin:0;padding:16px;background:#0b1224}@media print{body{padding:0;background:#fff}}\n' +
        parts[0] + '</style></head><body>' + clone.outerHTML + '</body></html>';
    });
  }

  FX.reportHtml = function () { return standaloneHtml(FX.chart.current()); };   // also used by the browser tests

  function download() {
    var a = FX.chart.current();
    if (!a) return;
    var btn = document.getElementById('report-download');
    btn.disabled = true;
    standaloneHtml(a).then(function (html) {
      var slug = String(a.chart.name).toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '').slice(0, 40) || 'chart';
      var url = URL.createObjectURL(new Blob([html], { type: 'text/html' }));
      var link = document.createElement('a');
      link.href = url; link.download = 'chart-reader-report-' + slug + '-' + new Date().toISOString().slice(0, 10) + '.html';
      document.body.appendChild(link); link.click(); link.remove();
      setTimeout(function () { URL.revokeObjectURL(url); }, 2000);
    }).catch(function (err) {
      console.error(err);
      document.getElementById('report-message').replaceChildren(ui.banner('error', null, ['The report could not be created: ' + err.message]));
    }).finally(function () { btn.disabled = !FX.chart.current(); });
  }

  function updateButtons() {
    var on = !!FX.chart.current();
    document.getElementById('report-download').disabled = !on;
    document.getElementById('report-print').disabled = !on;
  }

  FX.sections.register('report', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt; el = ui.el;
      document.getElementById('report-download').addEventListener('click', download);
      document.getElementById('report-print').addEventListener('click', function () { window.print(); });
      FX.chart.onChange(updateButtons);
    },
    show: function () {
      return FX.chart.renderInto('report-results', state, function (a, root) {
        var doc = el('article', { class: 'report' });
        root.appendChild(doc);
        render(a, doc);
      }).then(updateButtons);
    }
  });
})();
