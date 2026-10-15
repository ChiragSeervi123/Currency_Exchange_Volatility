/* =============================================================================
   charts.js - thin wrapper around Chart.js.
   Charts only DRAW arrays returned by R; nothing is calculated here.
   - One chart instance per canvas: re-rendering destroys the old instance.
   - Colours come from CSS tokens (style.css), so light/dark mode and a later
     redesign need no changes here.
   - Every chart gets a data-table twin (accessibility; values never depend on hover).
   ============================================================================= */
(function () {
  var registry = new Map();

  function token(name) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  }

  function colors() {
    return {
      s1: token('--series-1'), s1soft: token('--series-1-soft'), s1softer: token('--series-1-softer'),
      s2: token('--series-2'), s2soft: token('--series-2-soft'), s3: token('--series-3'),
      path: token('--path-line'), grid: token('--grid'), axis: token('--axis'),
      text: token('--text'), text2: token('--text-2'), muted: token('--muted'), surface: token('--surface')
    };
  }

  function applyDefaults() {
    if (!window.Chart) return;
    var c = colors();
    Chart.defaults.font.family = '"Inter", system-ui, -apple-system, "Segoe UI", Roboto, sans-serif';
    Chart.defaults.font.size = 11.5;
    Chart.defaults.color = c.text2;
    Chart.defaults.borderColor = c.grid;
    Chart.defaults.animation = false;
    Chart.defaults.maintainAspectRatio = false;
    Chart.defaults.responsive = true;
    Chart.defaults.plugins.legend.labels.boxWidth = 14;
    Chart.defaults.plugins.legend.labels.boxHeight = 2;
    Chart.defaults.plugins.tooltip.backgroundColor = 'rgba(6, 10, 22, 0.92)';
    Chart.defaults.plugins.tooltip.borderColor = 'rgba(255, 255, 255, 0.16)';
    Chart.defaults.plugins.tooltip.borderWidth = 1;
    Chart.defaults.plugins.tooltip.titleColor = '#ffffff';
    Chart.defaults.plugins.tooltip.bodyColor = '#dfe7f5';
    Chart.defaults.plugins.tooltip.padding = 9;
    Chart.defaults.elements.line.borderWidth = 2;
    Chart.defaults.elements.point.radius = 0;
    Chart.defaults.elements.point.hoverRadius = 4;
  }

  // Axis options. type: 'linear' (default) or 'category'.
  function axis(title, opts) {
    opts = opts || {};
    var c = colors();
    var ticks = { color: c.muted, maxTicksLimit: opts.maxTicks || 8, autoSkip: true, autoSkipPadding: 16,
                  padding: 6, maxRotation: 0 };
    if (opts.tickFormat) ticks.callback = opts.tickFormat;   // otherwise keep Chart.js's own labels
    // Category axes of ISO dates: label ticks as YYYY-MM so they stay readable on narrow charts
    // (tooltips and the data table still show the full date).
    if (opts.type === 'category' && !opts.tickFormat) {
      ticks.callback = function (value) {
        var label = this.getLabelForValue(value);
        return typeof label === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(label) ? label.slice(0, 7) : label;
      };
    }
    return Object.assign({
      type: opts.type || 'linear',
      title: { display: !!title, text: title, color: c.text2 },
      grid: { color: c.grid, drawTicks: false },
      border: { color: c.axis },
      ticks: ticks
    }, opts.extra || {});
  }

  // Translucent version of a #rrggbb token (other formats are returned unchanged).
  function alpha(color, a) {
    if (/^#[0-9a-f]{6}$/i.test(color)) {
      return color + Math.round(a * 255).toString(16).padStart(2, '0');
    }
    return color;
  }

  function xy(xs, ys) {
    var out = [];
    for (var i = 0; i < xs.length; i++) {
      if (ys[i] !== null && ys[i] !== undefined) out.push({ x: xs[i], y: ys[i] });
    }
    return out;
  }

  function buildTwin(canvasId, chart) {
    var holder = document.querySelector('[data-twin-for="' + canvasId + '"]');
    if (!holder) return;
    holder.replaceChildren();
    var labels = chart.data.labels || [];
    chart.data.datasets.forEach(function (ds) {
      if (ds.excludeFromTable) return;
      var rows = ds.data.slice(0, 400).map(function (d, i) {
        if (d && typeof d === 'object') return { x: d.x, y: d.y };
        return { x: labels[i], y: d };
      });
      var xTitle = chart.options.scales && chart.options.scales.x && chart.options.scales.x.title ? chart.options.scales.x.title.text : 'x';
      var yTitle = chart.options.scales && chart.options.scales.y && chart.options.scales.y.title ? chart.options.scales.y.title.text : 'y';
      holder.appendChild(FX.ui.el('h4', { text: ds.label + (ds.data.length > 400 ? ' (first 400 of ' + ds.data.length + ' points)' : '') }));
      holder.appendChild(FX.ui.table({
        compact: true,
        columns: [
          { key: 'x', label: xTitle || 'x', num: typeof rows[0] === 'object' && typeof (rows[0] || {}).x === 'number' },
          { key: 'y', label: yTitle || 'y', num: true }
        ],
        rows: rows
      }));
    });
  }

  function render(canvasId, config) {
    var canvas = document.getElementById(canvasId);
    if (!canvas || !window.Chart) return null;
    destroy(canvasId);
    config.options = config.options || {};
    config.options.interaction = config.options.interaction || { mode: 'nearest', intersect: false, axis: 'x' };
    var chart = new Chart(canvas.getContext('2d'), config);
    registry.set(canvasId, chart);
    buildTwin(canvasId, chart);
    return chart;
  }

  function destroy(canvasId) {
    var existing = registry.get(canvasId);
    if (existing) { existing.destroy(); registry.delete(canvasId); }
  }

  function destroyWithin(container) {
    container.querySelectorAll('canvas[id]').forEach(function (c) { destroy(c.id); });
  }

  // Shaded band between two series, drawn as two lines with fill between them.
  function band(label, xs, lower, upper, fillColor) {
    return [
      { label: label + ' (lower)', data: xy(xs, lower), borderWidth: 0, pointRadius: 0, fill: false,
        excludeFromTable: false, legendHidden: true },
      { label: label, data: xy(xs, upper), borderWidth: 0, pointRadius: 0, backgroundColor: fillColor, fill: '-1' }
    ];
  }

  // R sends data frames row by row ([{a: 1, b: 2}, ...]); charts want columns ({a: [...], b: [...]}).
  function columns(rows) {
    if (!Array.isArray(rows)) return rows;
    var out = {};
    rows.forEach(function (row) {
      Object.keys(row).forEach(function (k) { (out[k] = out[k] || []).push(row[k]); });
    });
    return out;
  }

  // Histogram (bars from R's bins) with an optional density curve, on one linear x axis.
  function histogram(canvasId, bins, opts) {
    opts = opts || {};
    bins = columns(bins);
    var c = colors();
    var sets = [{
      type: 'bar', label: opts.barLabel || 'Observed (density)', data: xy(bins.mid, bins.density),
      backgroundColor: c.s1, barPercentage: 0.94, categoryPercentage: 1, order: 2,
      binLower: bins.lower, binUpper: bins.upper, binCount: bins.count
    }];
    if (opts.curve) {
      sets.push({ type: 'line', label: opts.curveLabel || 'Normal density', data: xy(opts.curve.x, opts.curve.y),
                  borderColor: c.s2, borderWidth: 2, order: 1 });
    }
    return render(canvasId, {
      type: 'bar', data: { datasets: sets },
      options: {
        interaction: { mode: 'nearest', intersect: false, axis: 'x' },
        plugins: {
          legend: { display: !!opts.curve },
          tooltip: { callbacks: { label: function (item) {
            var ds = item.dataset;
            if (ds.binLower) {
              var i = item.dataIndex;
              return 'Bin ' + FX.fmt.sig(ds.binLower[i], 5) + ' to ' + FX.fmt.sig(ds.binUpper[i], 5) + ': ' +
                FX.fmt.int(ds.binCount[i]) + ' values, density ' + FX.fmt.sig(item.parsed.y, 4);
            }
            return ds.label + ': ' + FX.fmt.sig(item.parsed.y, 4);
          } } }
        },
        scales: { x: axis(opts.xTitle || 'Value', { extra: { offset: false } }), y: axis(opts.yTitle || 'Density', { extra: { beginAtZero: true } }) }
      }
    });
  }

  // Normal Q-Q plot: standardised sample quantiles against N(0,1) quantiles, with the y = x line.
  function qq(canvasId, q) {
    var c = colors();
    var lo = Math.min(q.theoretical[0], q.sample[0]);
    var hi = Math.max(q.theoretical[q.theoretical.length - 1], q.sample[q.sample.length - 1]);
    return render(canvasId, {
      type: 'scatter',
      data: { datasets: [
        // Lower 'order' is drawn on top: points above the reference line.
        { label: 'Sample quantiles', data: xy(q.theoretical, q.sample), backgroundColor: alpha(c.s1, 0.7),
          borderColor: c.surface, borderWidth: 1, pointRadius: 3.5, pointHoverRadius: 5, order: 1 },
        { type: 'line', label: 'Normal reference line (y = x)', data: [{ x: lo, y: lo }, { x: hi, y: hi }],
          borderColor: c.s2, borderWidth: 1.5, pointRadius: 0, order: 2 }
      ] },
      options: {
        interaction: { mode: 'nearest', intersect: false },
        scales: { x: axis('Theoretical N(0,1) quantile'), y: axis('Standardised sample quantile') }
      }
    });
  }

  // Save a chart as PNG. The chart canvas is transparent (it sits on glass), so it is
  // copied onto a dark background to keep the light text readable in the file.
  function downloadPNG(canvasId, fileName) {
    var chart = registry.get(canvasId);
    if (!chart) return false;
    var src = chart.canvas;
    var out = document.createElement('canvas');
    out.width = src.width; out.height = src.height;
    var ctx = out.getContext('2d');
    ctx.fillStyle = '#101a32';
    ctx.fillRect(0, 0, out.width, out.height);
    ctx.drawImage(src, 0, 0);
    var a = document.createElement('a');
    a.href = out.toDataURL('image/png');
    a.download = fileName || canvasId + '.png';
    document.body.appendChild(a);
    a.click();
    a.remove();
    return true;
  }

  // Show a panel full screen (Fullscreen API); charts inside resize automatically.
  function toggleFullscreen(element) {
    if (!element) return;
    if (document.fullscreenElement) { document.exitFullscreen(); return; }
    if (element.requestFullscreen) element.requestFullscreen().catch(function () {});
  }

  // Small trend line drawn as SVG from values returned by R (no axes, no statistics).
  function sparkline(svg, values, color) {
    if (!svg) return;
    svg.replaceChildren();
    var v = (values || []).filter(function (x) { return typeof x === 'number' && isFinite(x); });
    if (v.length < 2) return;
    var lo = Math.min.apply(null, v), hi = Math.max.apply(null, v);
    var span = hi - lo || 1;
    var pts = v.map(function (y, i) {
      return [(i / (v.length - 1)) * 100, 29 - ((y - lo) / span) * 26];
    });
    var line = pts.map(function (p, i) { return (i ? 'L' : 'M') + p[0].toFixed(2) + ' ' + p[1].toFixed(2); }).join(' ');
    var ns = 'http://www.w3.org/2000/svg';
    var gradId = 'spark-' + (svg.closest('[id]') || { id: Math.random().toString(36).slice(2) }).id;
    var defs = document.createElementNS(ns, 'defs');
    var grad = document.createElementNS(ns, 'linearGradient');
    grad.setAttribute('id', gradId); grad.setAttribute('x1', '0'); grad.setAttribute('y1', '0');
    grad.setAttribute('x2', '0'); grad.setAttribute('y2', '1');
    [['0', '0.35'], ['1', '0']].forEach(function (s) {
      var stop = document.createElementNS(ns, 'stop');
      stop.setAttribute('offset', s[0]); stop.setAttribute('stop-color', color); stop.setAttribute('stop-opacity', s[1]);
      grad.appendChild(stop);
    });
    defs.appendChild(grad);
    var area = document.createElementNS(ns, 'path');
    area.setAttribute('d', line + ' L100 32 L0 32 Z');
    area.setAttribute('fill', 'url(#' + gradId + ')');
    var path = document.createElementNS(ns, 'path');
    path.setAttribute('d', line);
    path.setAttribute('fill', 'none');
    path.setAttribute('stroke', color);
    path.setAttribute('stroke-width', '1.6');
    path.setAttribute('vector-effect', 'non-scaling-stroke');
    path.setAttribute('stroke-linejoin', 'round');
    svg.appendChild(defs); svg.appendChild(area); svg.appendChild(path);
  }

  applyDefaults();

  FX.charts = {
    colors: colors, alpha: alpha, axis: axis, xy: xy, band: band, render: render, destroy: destroy,
    histogram: histogram, qq: qq, columns: columns,
    get: function (canvasId) { return registry.get(canvasId) || null; },
    downloadPNG: downloadPNG, toggleFullscreen: toggleFullscreen, sparkline: sparkline,
    destroyWithin: destroyWithin, legendFilter: function (item, data) {
      return !data.datasets[item.datasetIndex].legendHidden;
    }
  };
})();
