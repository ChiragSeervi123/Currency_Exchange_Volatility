/* =============================================================================
   dashboard.js - the main dashboard (reference composition): KPI cards, the
   price series read from the uploaded chart (or an overlay of it on the
   screenshot), the distribution of returns and "Our Hypothesis".
   Data: the chart analysis from R (FX.chart, filled by the Chart Upload panel).
   Every number shown here was computed by R; this file only formats and draws it.
   ============================================================================= */
(function () {
  var ui = FX.ui, fmt = FX.fmt;
  var CHARTS = ['chart-series', 'chart-dist'];

  function $(sel) { return document.querySelector(sel); }

  function setChartState(canvasId, text, kind) {
    var s = document.querySelector('[data-state-for="' + canvasId + '"]');
    if (!s) return;
    if (!text) { s.classList.add('is-hidden'); return; }
    s.textContent = text;
    s.classList.remove('is-hidden');
    s.classList.toggle('is-error', kind === 'error');
  }

  function setKpi(id, opts) {
    var card = document.getElementById(id);
    var value = card.querySelector('[data-kpi="value"]');
    value.textContent = opts.value;
    value.className = 'kpi-value' + (opts.valueClass ? ' ' + opts.valueClass : '');
    var sub = card.querySelector('[data-kpi="sub"]');
    sub.replaceChildren.apply(sub, [].concat(opts.sub || []).map(function (n) { return typeof n === 'string' ? document.createTextNode(n) : n; }));
    FX.charts.sparkline(card.querySelector('[data-kpi="spark"]'), opts.spark, opts.sparkColor);
    if (opts.title) card.setAttribute('title', opts.title); else card.removeAttribute('title');
  }

  function capitalise(t) { return t.charAt(0).toUpperCase() + t.slice(1); }
  function perCandle(a) { return a.chart.interval ? 'per ' + a.chart.interval + ' candle' : 'per candle'; }

  // ---- KPI cards ------------------------------------------------------------------------------
  function renderKpis(a) {
    var o = a.overview, s = a.series, n = s.close.length;
    var prev = s.close[n - 2], last = s.close[n - 1], ch = last - prev, up = ch >= 0;
    document.querySelector('#kpi-last [data-kpi="name"]').textContent = '';
    setKpi('kpi-last', {
      value: fmt.price(last),
      sub: [ui.el('span', { class: up ? 'up' : 'down', text: (up ? '▲ ' : '▼ ') + fmt.price(Math.abs(ch)) +
             ' (' + fmt.num(Math.abs(100 * ch / prev), 2) + '%)' })],
      spark: s.close, sparkColor: up ? '#4ade80' : '#ff7a85',
      title: 'Closing price of the last ' + (a.chart.type === 'candles' ? 'candle' : 'point') + ' read from the chart, and its change from the one before.'
    });
    setKpi('kpi-mean', {
      value: fmt.price(o.mean_price),
      sub: 'Median ' + fmt.price(a.descriptive.prices.median),
      spark: s.close, sparkColor: '#5b9bff',
      title: 'Mean of all closing prices on the chart (the mean line).'
    });
    setKpi('kpi-vol', {
      value: fmt.num(o.sd_return, 2) + '%',
      sub: perCandle(a),
      spark: a.rolling.sd, sparkColor: '#a78bfa',
      title: 'Standard deviation of the log returns per candle. Trend line: ' + a.rolling.window + '-candle rolling SD.'
    });
    document.querySelector('#kpi-return [data-kpi="period"]').textContent = '(' + fmt.int(n) + (a.chart.type === 'candles' ? ' Candles)' : ' Points)');
    setKpi('kpi-return', {
      value: fmt.signedPct(o.change_pct),
      valueClass: o.change_pct >= 0 ? 'is-positive' : 'is-negative',
      sub: 'H ' + fmt.price(o.high) + ' · L ' + fmt.price(o.low),
      spark: s.close, sparkColor: o.change_pct >= 0 ? '#4ade80' : '#ff7a85',
      title: 'Change from the first to the last closing price on the chart.'
    });
  }

  function emptyKpis() {
    ['kpi-last', 'kpi-mean', 'kpi-vol', 'kpi-return'].forEach(function (id) { setKpi(id, { value: '—', sub: 'Upload a chart' }); });
    document.querySelector('#kpi-return [data-kpi="period"]').textContent = '';
  }

  // ---- Price series (candles, or the line) -------------------------------------------------------
  function renderSeries(a) {
    var s = a.series, c = FX.charts.colors();
    var labels = s.index.map(String);
    var up = '#22ab94', down = '#f7525f';
    var sets = [];
    var candles = a.chart.type === 'candles';
    $('#series-legend').hidden = false;
    $('#series-legend').querySelectorAll('li').forEach(function (li, i) { li.hidden = !candles && i < 2; });
    if (candles) {
      var colours = s.colour.map(function (k) { return k === 'up' ? up : down; });
      sets.push({ type: 'bar', label: 'High–low (wick)', grouped: false, barPercentage: 0.18, categoryPercentage: 1,
                  data: s.low.map(function (lo, i) { return [lo, s.high[i]]; }), backgroundColor: colours, order: 3, excludeFromTable: true });
      sets.push({ type: 'bar', label: 'Open–close (body)', grouped: false, barPercentage: 0.72, categoryPercentage: 1,
                  data: s.open.map(function (op, i) { var cl = s.close[i]; return op === cl ? [op - 1e-9 * op, cl] : [Math.min(op, cl), Math.max(op, cl)]; }),
                  backgroundColor: colours, order: 2, excludeFromTable: true });
    } else {
      sets.push({ type: 'line', label: 'Price', data: s.close, borderColor: c.s1, borderWidth: 1.8, order: 2 });
    }
    sets.push({ type: 'line', label: 'Close', data: s.close, borderColor: 'transparent', borderWidth: 0, order: 4, legendHidden: true });
    sets.push({ type: 'line', label: 'Mean line (' + fmt.price(a.overview.mean_price) + ')', data: s.close.map(function () { return a.overview.mean_price; }),
                borderColor: c.s1, borderWidth: 1.4, borderDash: [6, 4], order: 1, excludeFromTable: true });
    FX.charts.render('chart-series', {
      type: 'bar', data: { labels: labels, datasets: sets },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: { filter: function (item) { return item.dataset.label === 'Close' || item.dataset.label === 'Price'; },
                     callbacks: {
                       title: function (items) { return (candles ? 'Candle ' : 'Point ') + items[0].label; },
                       label: function (item) {
                         var i = item.dataIndex;
                         return candles ? 'O ' + fmt.price(s.open[i]) + '  H ' + fmt.price(s.high[i]) + '  L ' + fmt.price(s.low[i]) + '  C ' + fmt.price(s.close[i])
                                        : 'Price ' + fmt.price(s.close[i]);
                       } } }
        },
        scales: {
          x: FX.charts.axis(candles ? 'Candle' : 'Point', { type: 'category', maxTicks: 10 }),
          y: FX.charts.axis('Price', { maxTicks: 6, tickFormat: function (v) { return fmt.price(v); },
                                       extra: { beginAtZero: false, grace: '4%' } })
        }
      }
    });
  }

  // The uploaded image with the points R read from it drawn on top (drawing only).
  function renderOverlay(a) {
    var img = $('#overlay-img'), svg = $('#overlay-svg'), box = $('#series-overlay');
    var w = a.chart.image.analysed_width, h = a.chart.image.analysed_height;
    box.style.aspectRatio = w + ' / ' + h;
    img.src = FX.config.apiBase + '/chart/image?v=' + FX.chart.version();
    svg.setAttribute('viewBox', '0 0 ' + w + ' ' + h);
    svg.replaceChildren();
    var ns = 'http://www.w3.org/2000/svg', s = a.series;
    var pts = s.x.map(function (x, i) { return x + ',' + s.y_close[i]; }).join(' ');
    var line = document.createElementNS(ns, 'polyline');
    line.setAttribute('points', pts);
    line.setAttribute('class', 'overlay-line');
    svg.appendChild(line);
    s.x.forEach(function (x, i) {
      var dot = document.createElementNS(ns, 'circle');
      dot.setAttribute('cx', x); dot.setAttribute('cy', s.y_close[i]); dot.setAttribute('r', Math.max(1.6, w / 700));
      dot.setAttribute('class', 'overlay-dot');
      svg.appendChild(dot);
    });
  }

  function applySeriesView() {
    var overlay = $('#series-view').value === 'overlay';
    $('#series-overlay').hidden = !overlay;
    $('#series-legend').style.visibility = overlay ? 'hidden' : '';
    $('#chart-series').style.visibility = overlay ? 'hidden' : '';
    $('[data-download="chart-series"]').disabled = overlay || !FX.chart.current();
  }

  // ---- Distribution of returns ---------------------------------------------------------------------
  function renderDistribution(a) {
    var d = a.descriptive, h = FX.charts.columns(d.returns_histogram), c = FX.charts.colors();
    var sets = [{
      type: 'bar', label: 'Returns (count)', data: FX.charts.xy(h.mid, h.count),
      backgroundColor: FX.charts.alpha(c.s1, 0.85), borderColor: 'rgba(170, 200, 255, 0.55)', borderWidth: 1,
      barPercentage: 1, categoryPercentage: 1, order: 2, binLower: h.lower, binUpper: h.upper
    }, {
      type: 'line', label: 'Normal distribution (expected count)', data: FX.charts.xy(h.mid, h.normal_expected_count),
      borderColor: c.s2, borderWidth: 2, pointRadius: 0, tension: 0.35, order: 1
    }];
    FX.charts.render('chart-dist', {
      type: 'bar', data: { datasets: sets },
      options: {
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: { callbacks: {
            title: function (items) { var i = items[0].dataIndex; return fmt.num(h.lower[i], 3) + '% to ' + fmt.num(h.upper[i], 3) + '%'; },
            label: function (item) { return item.dataset.label + ': ' + fmt.num(item.parsed.y, item.datasetIndex ? 1 : 0); }
          } }
        },
        scales: {
          x: FX.charts.axis('Return per candle (%)', { maxTicks: 9, extra: { offset: false } }),
          y: FX.charts.axis('Frequency', { maxTicks: 6, extra: { beginAtZero: true } })
        }
      }
    });
    var r = d.returns, box = $('#dist-stats');
    box.querySelector('[data-stat="mean"]').textContent = fmt.num(r.mean, 3) + '%';
    box.querySelector('[data-stat="sd"]').textContent = fmt.num(r.sd, 3) + '%';
    box.querySelector('[data-stat="variance"]').textContent = fmt.sig(r.variance, 3);
    box.querySelector('[data-stat="skewness"]').textContent = fmt.num(r.skewness, 3);
    box.querySelector('[data-stat="kurtosis"]').textContent = fmt.num(r.kurtosis, 3);
    box.title = 'Computed in R from all ' + fmt.int(r.n) + ' returns per candle (%). Variance in %².';

    var bands = d.sigma_bands, within = bands[1].observed_share + bands[2].observed_share + bands[3].observed_share + bands[4].observed_share;
    var jb = a.overview.jarque_bera, rejected = jb.p_value < 0.05, norm = $('#dist-normality');
    norm.classList.toggle('stat-box-good', !rejected);
    norm.classList.toggle('stat-box-warn', rejected);
    norm.querySelector('[data-stat="range"]').textContent = 'P(' + fmt.num(r.mean - 2 * r.sd, 2) + '% < R < ' + fmt.num(r.mean + 2 * r.sd, 2) + '%)';
    norm.querySelector('[data-stat="share"]').textContent = fmt.prob(within, 1);
    norm.querySelector('[data-stat="note"]').textContent = 'Normal 95.4% · JB ' + fmt.pText(jb.p_value);
    norm.title = 'Share of returns within the mean ± 2 SD, compared with 95.4% for a normal distribution. Jarque-Bera: ' +
      (rejected ? 'normality rejected at 5%.' : 'normality not rejected at 5%.');
  }

  // ---- Our hypothesis ----------------------------------------------------------------------------------
  function renderOutlook(a) {
    var o = a.hypothesis.outlook;
    var icon = { up: 'i-trending-up', down: 'i-trending-down', sideways: 'i-move-horizontal' }[o.verdict];
    var label = { up: 'Up', down: 'Down', sideways: 'Close to the mean line' }[o.verdict];
    var body = $('#outlook-body');
    body.replaceChildren(
      ui.el('div', { class: 'verdict verdict-' + o.verdict }, [
        ui.el('span', { class: 'verdict-icon', 'aria-hidden': 'true' }),
        ui.el('div', {}, [
          ui.el('p', { class: 'verdict-label', text: label + ' · ' + o.strength + ' evidence' }),
          ui.el('p', { class: 'verdict-text', text: capitalise(o.headline.replace('Our hypothesis: ', '')) + '.' })
        ])
      ]),
      ui.el('p', { class: 'verdict-note', text: o.reasons[0] })
    );
    body.querySelector('.verdict-icon').innerHTML = '<svg class="icon"><use href="assets/icons.svg?v=3#' + icon + '"/></svg>';
    $('[data-out="expected"]').textContent = fmt.price(o.expected_price);
    $('[data-out="range"]').textContent = fmt.price(o.range_low) + ' – ' + fmt.price(o.range_high);
    $('[data-out="p"]').textContent = fmt.prob(o.p_higher, 1);
  }

  function emptyOutlook() {
    $('#outlook-body').replaceChildren(ui.el('div', { class: 'chart-state' }, ['Upload a chart screenshot to begin.']));
    ['expected', 'range', 'p'].forEach(function (k) { $('[data-out="' + k + '"]').textContent = '—'; });
  }

  function renderFootnote(a) {
    var c = a.chart;
    $('#dash-footnote').textContent = 'Read in R from ' + (c.file_name || 'the uploaded image') + ': ' + fmt.int(c.n_points) + (c.type === 'candles' ? ' candles' : ' points of a line chart') +
      '. ' + c.scale.note + ' Resolution ≈ ' + fmt.sig(c.resolution, 3) + ' per pixel row. Prices are read from the image (approximate); nothing here is a live quote.';
  }

  // ---- Whole page --------------------------------------------------------------------------------------
  var shown = -1;
  function render() {
    var a = FX.chart.current();
    if (shown === FX.chart.version()) return;
    shown = FX.chart.version();
    if (!a) {
      CHARTS.forEach(function (id) { FX.charts.destroy(id); setChartState(id, 'Upload a chart screenshot to begin.'); });
      document.querySelectorAll('#view-dashboard [data-download]').forEach(function (b) { b.disabled = true; });
      $('#series-view').disabled = true; $('#series-view').value = 'series'; applySeriesView();
      emptyKpis(); emptyOutlook();
      $('#dash-footnote').textContent = 'All values are computed in R from the uploaded image.';
      return;
    }
    renderKpis(a);
    renderSeries(a);
    renderOverlay(a);
    renderDistribution(a);
    renderOutlook(a);
    renderFootnote(a);
    CHARTS.forEach(function (id) { setChartState(id, null); document.querySelector('[data-download="' + id + '"]').disabled = false; });
    $('#series-view').disabled = false;
    applySeriesView();
  }

  FX.chart.onChange(function () { render(); });

  FX.sections.register('dashboard', {
    init: function () {
      $('#series-view').addEventListener('change', applySeriesView);
      document.querySelectorAll('#view-dashboard [data-download]').forEach(function (btn) {
        btn.addEventListener('click', function () { FX.charts.downloadPNG(btn.dataset.download, 'fxlab-' + btn.dataset.download + '.png'); });
      });
      document.querySelectorAll('#view-dashboard [data-expand]').forEach(function (btn) {
        btn.addEventListener('click', function () { FX.charts.toggleFullscreen(document.getElementById(btn.dataset.expand)); });
      });
    },
    show: function () { FX.chart.ready().then(render); }
  });
})();
