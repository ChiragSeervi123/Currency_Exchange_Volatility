/* chart-data.js - Chart Data: the prices R read from the uploaded screenshot, how the price axis
   was read, an overlay of the extracted points on the image, returns and rolling volatility.
   Everything shown was computed by R; this file only draws it. */
(function () {
  var ui, fmt;
  var state = { version: -1 };

  function overlay(a) {
    var w = a.chart.image.analysed_width, h = a.chart.image.analysed_height, s = a.series;
    var box = ui.el('div', { class: 'overlay-view overlay-static' });
    box.style.aspectRatio = w + ' / ' + h;
    var img = ui.el('img', { alt: 'The uploaded chart with the price points R read from it', src: FX.config.apiBase + '/chart/image?v=' + FX.chart.version() });
    var ns = 'http://www.w3.org/2000/svg';
    var svg = document.createElementNS(ns, 'svg');
    svg.setAttribute('viewBox', '0 0 ' + w + ' ' + h);
    svg.setAttribute('preserveAspectRatio', 'none');
    var pb = a.chart.plot_box;
    var rect = document.createElementNS(ns, 'rect');
    rect.setAttribute('x', pb.x1); rect.setAttribute('y', pb.y1); rect.setAttribute('width', pb.x2 - pb.x1); rect.setAttribute('height', pb.y2 - pb.y1);
    rect.setAttribute('class', 'overlay-box');
    svg.appendChild(rect);
    if (a.chart.type === 'candles') {
      s.x.forEach(function (x, i) {
        var l = document.createElementNS(ns, 'line');
        l.setAttribute('x1', x); l.setAttribute('x2', x); l.setAttribute('y1', s.y_high[i]); l.setAttribute('y2', s.y_low[i]);
        l.setAttribute('class', 'overlay-wick');
        svg.appendChild(l);
      });
    }
    var line = document.createElementNS(ns, 'polyline');
    line.setAttribute('points', s.x.map(function (x, i) { return x + ',' + s.y_close[i]; }).join(' '));
    line.setAttribute('class', 'overlay-line');
    svg.appendChild(line);
    s.x.forEach(function (x, i) {
      var dot = document.createElementNS(ns, 'circle');
      dot.setAttribute('cx', x); dot.setAttribute('cy', s.y_close[i]); dot.setAttribute('r', Math.max(1.6, w / 700));
      dot.setAttribute('class', 'overlay-dot');
      svg.appendChild(dot);
    });
    box.appendChild(img); box.appendChild(svg);
    return box;
  }

  function render(a, root) {
    var c = a.chart, o = a.overview, s = a.series, r = a.returns, col = FX.charts.colors();
    var roll = a.rolling, finite = roll.sd.map(function (v, i) { return v == null ? null : { v: v, i: i + 2 }; }).filter(Boolean);
    var hi = finite.reduce(function (m, x) { return !m || x.v > m.v ? x : m; }, null);
    var lo = finite.reduce(function (m, x) { return !m || x.v < m.v ? x : m; }, null);
    var unit = c.type === 'candles' ? 'candles' : 'points';
    root.appendChild(ui.el('p', { class: 'meta', text: c.name + (c.interval && c.name.indexOf(c.interval) < 0 ? ' · ' + c.interval : '') + ' · ' + (c.file_name || 'uploaded image') +
      ' · ' + c.image.width + ' × ' + c.image.height + ' px ' + c.image.format + ', ' + c.image.theme + ' theme · analysed ' + c.analysed_at }));
    root.appendChild(ui.cards([
      { label: 'Prices read', value: fmt.int(c.n_points), sub: c.type === 'candles' ? 'candles (open, high, low, close)' : 'points along the line' },
      { label: 'Returns', value: fmt.int(r.length), sub: 'between consecutive ' + unit },
      { label: 'First → last', value: fmt.price(o.first) + ' → ' + fmt.price(o.last), sub: fmt.signedPct(o.change_pct) + ' over the chart' },
      { label: 'Volatility', value: fmt.num(o.sd_return, 4) + '%', sub: 'SD of returns per ' + (c.interval ? c.interval + ' ' : '') + 'candle' },
      hi ? { label: 'Highest ' + roll.window + '-candle volatility', value: fmt.num(hi.v, 3) + '%', sub: 'ending at candle ' + hi.i } : null,
      lo ? { label: 'Lowest ' + roll.window + '-candle volatility', value: fmt.num(lo.v, 3) + '%', sub: 'ending at candle ' + lo.i } : null
    ]));

    root.appendChild(ui.section('How the chart was read', [
      ui.el('ul', { class: 'list-plain' }, [
        ui.el('li', { text: c.scale.note }),
        c.scale.labels ? ui.el('li', { text: 'Axis labels used: ' + c.scale.labels.map(function (l) { return l.text; }).join(', ') + '.' }) : null,
        ui.el('li', { text: 'Resolution: one pixel row of the chart ≈ ' + fmt.sig(c.resolution, 3) + ' in price, so every value is approximate to about that amount.' }),
        c.detected_title ? ui.el('li', { text: 'Title read from the chart legend: "' + c.detected_title + '"' + (c.detected_interval ? ', interval ' + c.detected_interval : '') + '.' }) : null,
        c.time_labels && c.time_labels.length ? ui.el('li', { text: 'Time axis labels: ' + c.time_labels.join(' · ') + '.' }) : null
      ]),
      ui.el('figure', { class: 'chart-card' }, [
        ui.el('figcaption', { text: 'Extracted points drawn on your screenshot' }),
        ui.el('p', { class: 'chart-note', text: 'Yellow: the closing prices R read' + (c.type === 'candles' ? '; thin lines: each candle\'s high–low range' : '') +
          '. Dashed box: the plot area that was searched. The points should sit on the chart\'s candles or line.' }),
        overlay(a)
      ])
    ]));

    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'data-close-chart', title: 'Closing prices read from the chart', note: c.type === 'candles' ? 'Band: each candle\'s low to high.' : null }));
    var sets = [{ label: 'Close', data: s.close, borderColor: col.s1, borderWidth: 1.6 }];
    if (c.type === 'candles') {
      sets.push({ label: 'Low', data: s.low, borderColor: 'transparent', pointRadius: 0, legendHidden: true });
      sets.push({ label: 'High', data: s.high, borderColor: 'transparent', backgroundColor: FX.charts.alpha(col.s1, 0.18), fill: '-1', pointRadius: 0 });
    }
    FX.charts.render('data-close-chart', {
      type: 'line', data: { labels: s.index.map(String), datasets: sets },
      options: { plugins: { legend: { labels: { filter: FX.charts.legendFilter } } },
        scales: { x: FX.charts.axis('Candle', { type: 'category' }), y: FX.charts.axis('Price', { tickFormat: function (v) { return fmt.price(v); } }) } }
    });
    grid.appendChild(ui.chartCard({ id: 'data-ret-chart', title: 'Returns per candle and rolling volatility',
      note: 'Bars: r = 100 · ln(Pₜ / Pₜ₋₁). Line: SD of the last ' + roll.window + ' returns.' }));
    FX.charts.render('data-ret-chart', {
      type: 'bar',
      data: { labels: r.map(function (v, i) { return String(i + 2); }), datasets: [
        { type: 'bar', label: 'Return (%)', data: r, backgroundColor: r.map(function (v) { return v >= 0 ? '#22ab94' : '#f7525f'; }), barPercentage: 1, categoryPercentage: 0.9 },
        { type: 'line', label: roll.window + '-candle volatility (%)', data: roll.sd, borderColor: col.s2, borderWidth: 1.8, pointRadius: 0 }
      ] },
      options: { scales: { x: FX.charts.axis('Candle', { type: 'category' }), y: FX.charts.axis('Percent') } }
    });

    var rows = s.index.map(function (k, i) {
      return { index: k, open: s.open[i], high: s.high[i], low: s.low[i], close: s.close[i], ret: i ? r[i - 1] : null, colour: s.colour[i] };
    });
    var cols = [{ key: 'index', label: c.type === 'candles' ? 'Candle' : 'Point', num: true, format: fmt.int }];
    if (c.type === 'candles') {
      ['open', 'high', 'low'].forEach(function (k) { cols.push({ key: k, label: k.charAt(0).toUpperCase() + k.slice(1), num: true, format: fmt.price }); });
    }
    cols.push({ key: 'close', label: 'Close', num: true, format: fmt.price });
    cols.push({ key: 'ret', label: 'Return (%)', num: true, format: function (x) { return x == null ? '—' : fmt.num(x, 4); } });
    if (c.type === 'candles') cols.push({ key: 'colour', label: 'Candle' });
    root.appendChild(ui.el('details', { class: 'table-twin' }, [
      ui.el('summary', { text: 'Show every extracted ' + (c.type === 'candles' ? 'candle' : 'point') + ' (' + rows.length + ')' }),
      ui.table({ compact: true, columns: cols, rows: rows })
    ]));
    root.appendChild(ui.warnings(c.notes, 'About these values'));
  }

  FX.sections.register('data', {
    init: function () { ui = FX.ui; fmt = FX.fmt; },
    show: function () { return FX.chart.renderInto('data-results', state, render); }
  });
})();
