/* historical.js - Historical Analysis: GET /historical/summary + /historical/data,
   CSV upload (POST /historical/upload) and reset (POST /historical/reset). */
(function () {
  var ui, fmt;
  var state = { busy: false, version: -1 };
  var uploadState = { busy: false };
  var lastDataset = null;

  function qualityTable(q) {
    return ui.kvTable('Data validation report (computed in R when the file was loaded)', [
      ['Rows read from the file', fmt.int(q.rows_read)],
      ['Valid observations used', fmt.int(q.valid_observations)],
      ['Dates without a published value (excluded)', fmt.int(q.missing_values)],
      ['Invalid dates (excluded)', fmt.int(q.invalid_dates)],
      ['Non-numeric values (excluded)', fmt.int(q.non_numeric_values)],
      ['Zero or negative values (excluded)', fmt.int(q.non_positive_values)],
      ['Exact duplicate rows removed', fmt.int(q.duplicate_rows_removed)],
      ['Rows already in chronological order', q.sorted_chronologically ? 'Yes' : 'No (sorted by R)'],
      ['Observations on weekends', fmt.int(q.weekend_observations)],
      ['Longest gap between observations', fmt.int(q.largest_gap_days) + ' calendar days'],
      ['Largest one-day move', fmt.pct(q.largest_abs_daily_log_change_pct, 2) + ' (absolute log change)'],
      ['Columns used', (q.columns_used || []).join(', ')]
    ]);
  }

  function lineChart(id, labels, values, label, yTitle, color) {
    FX.charts.render(id, {
      type: 'line',
      data: { labels: labels, datasets: [{ label: label, data: values, borderColor: color, borderWidth: 1.2, spanGaps: false }] },
      options: {
        plugins: { legend: { display: false } },
        scales: { x: FX.charts.axis('Date', { type: 'category', maxTicks: 8 }), y: FX.charts.axis(yTitle) }
      }
    });
  }

  function render(summary, data, root) {
    var ds = summary.dataset, w = summary.window, v = summary.volatility, ch = summary.change, c = FX.charts.colors();
    lastDataset = ds;
    var form = document.getElementById('hist-form');
    form.querySelector('[name="start_date"]').value = w.first_observation;
    form.querySelector('[name="end_date"]').value = w.last_observation;

    root.appendChild(ui.banner(ds.kind === 'uploaded' ? 'warn' : 'ok', null, [
      (ds.kind === 'uploaded' ? 'Using your uploaded file "' + ds.file_name + '" (loaded ' + ds.loaded_at + '). ' : 'Using the bundled dataset (' + ds.file_name + '). ') +
      ds.name + '. Source: ' + ds.source + '. The file covers ' + ds.first_date + ' to ' + ds.last_date + '.'
    ]));
    root.appendChild(ui.cards([
      { label: 'Observations in window', value: fmt.int(w.observations), sub: w.first_observation + ' to ' + w.last_observation },
      { label: 'Daily returns', value: fmt.int(w.returns), sub: 'between consecutive valid observations' },
      { label: 'First → last rate', value: fmt.num(ch.first_rate, 2) + ' → ' + fmt.num(ch.last_rate, 2), sub: (ch.percent >= 0 ? '+' : '') + fmt.pct(ch.percent) + ' (log change ' + fmt.pct(ch.log_percent) + ')' },
      { label: 'Annualised volatility', value: fmt.pct(v.annualised_vol_pct), sub: 'daily SD ' + fmt.pct(v.daily_sd_pct, 4) },
      v.rolling_max_pct != null ? { label: 'Highest ' + v.rolling_window + '-day volatility', value: fmt.pct(v.rolling_max_pct), sub: 'annualised, ' + v.rolling_max_date } : null,
      v.rolling_min_pct != null ? { label: 'Lowest ' + v.rolling_window + '-day volatility', value: fmt.pct(v.rolling_min_pct), sub: 'annualised, ' + v.rolling_min_date } : null
    ]));
    root.appendChild(ui.warnings(summary.warnings, 'Notes for this window'));

    if (data.note) root.appendChild(ui.el('p', { class: 'hint', text: data.note }));
    root.appendChild(ui.chartCard({ id: 'hist-rate-chart', title: 'USD/INR exchange rate (INR per USD)' }));
    lineChart('hist-rate-chart', data.series.date, data.series.rate, 'USD/INR', 'INR per USD', c.s1);
    var grid = ui.el('div', { class: 'grid-2' });
    root.appendChild(grid);
    grid.appendChild(ui.chartCard({ id: 'hist-ret-chart', title: 'Daily log returns (%)', note: 'r = 100 · ln(S_t / S_t−1). Large spikes are big one-day moves.' }));
    lineChart('hist-ret-chart', data.series.date, data.series.log_return_pct, 'Daily log return (%)', 'Log return (%)', c.s1);
    grid.appendChild(ui.chartCard({ id: 'hist-vol-chart', title: data.rolling_window + '-day rolling volatility (annualised %)',
      note: 'SD of the last ' + data.rolling_window + ' daily log returns × √252. Clusters of high volatility are visible.' }));
    lineChart('hist-vol-chart', data.series.date, data.series.rolling_ann_vol_pct, 'Rolling volatility (%)', 'Annualised volatility (%)', c.s1);

    root.appendChild(ui.table({
      caption: 'Calendar-year summary', compact: summary.annual.length > 12,
      columns: [
        { key: 'year', label: 'Year', format: String },
        { key: 'observations', label: 'Obs.', num: true, format: fmt.int },
        { key: 'open_rate', label: 'First', num: true, format: function (x) { return fmt.num(x, 2); } },
        { key: 'close_rate', label: 'Last', num: true, format: function (x) { return fmt.num(x, 2); } },
        { key: 'change_pct', label: 'Change', num: true, format: function (x) { return fmt.pct(x); } },
        { key: 'mean_rate', label: 'Mean', num: true, format: function (x) { return fmt.num(x, 2); } },
        { key: 'min_rate', label: 'Min', num: true, format: function (x) { return fmt.num(x, 2); } },
        { key: 'max_rate', label: 'Max', num: true, format: function (x) { return fmt.num(x, 2); } },
        { key: 'annualised_vol_pct', label: 'Volatility (ann.)', num: true, format: function (x) { return fmt.pct(x); } }
      ],
      rows: summary.annual
    }));
    var movesCols = [
      { key: 'date', label: 'Date' },
      { key: 'log_return_pct', label: 'Log return', num: true, format: function (x) { return fmt.pct(x, 3); } },
      { key: 'previous_rate', label: 'Previous rate', num: true, format: function (x) { return fmt.num(x, 4); } },
      { key: 'rate', label: 'Rate', num: true, format: function (x) { return fmt.num(x, 4); } }
    ];
    var moves = ui.el('div', { class: 'grid-2' }, [
      ui.table({ caption: 'Largest one-day rises of USD/INR (rupee weakened)', columns: movesCols, rows: summary.largest_rises }),
      ui.table({ caption: 'Largest one-day falls of USD/INR (rupee strengthened)', columns: movesCols, rows: summary.largest_falls })
    ]);
    root.appendChild(moves);
    root.appendChild(qualityTable(ds.quality));
    root.appendChild(ui.warnings(ds.quality.warnings, 'Validation notes for the whole file'));
  }

  function load(params) {
    var form = document.getElementById('hist-form');
    var results = document.getElementById('hist-results');
    state.version = FX.dataVersion;
    return ui.runAnalysis({
      state: state, form: form, results: results, loadingText: 'Loading historical data from R…',
      request: function () {
        var p = params || ui.readForm(form);
        return Promise.all([FX.api.get('/historical/summary', p), FX.api.get('/historical/data', p)]);
      },
      render: function (res, root) { render(res[0], res[1], root); }
    }).then(function (ok) { if (ok === false) state.version = -1; });
  }

  function upload() {
    var form = document.getElementById('upload-form');
    var msg = document.getElementById('upload-message');
    var file = form.querySelector('input[type="file"]').files[0];
    if (uploadState.busy) return;
    if (!file) { msg.replaceChildren(ui.banner('error', null, ['Choose a CSV file first.'])); return; }
    if (file.size > 5 * 1024 * 1024) { msg.replaceChildren(ui.banner('error', null, ['The file is larger than 5 MB.'])); return; }
    uploadState.busy = true;
    var button = form.querySelector('button[type="submit"]');
    button.disabled = true;
    msg.replaceChildren(ui.el('p', { class: 'loading-note', text: 'Sending the file to R for validation…' }));
    file.text().then(function (text) {
      return FX.api.post('/historical/upload', { csv_text: text, file_name: file.name });
    }).then(function (info) {
      msg.replaceChildren(ui.banner('ok', 'R accepted "' + info.file_name + '": ' + fmt.int(info.observations) + ' valid observations from ' +
        info.first_date + ' to ' + info.last_date + '. All analyses now use this file.', info.quality.warnings || []));
      FX.dataVersion += 1;
      document.getElementById('hist-form').reset();
      return load({});
    }).catch(function (err) {
      msg.replaceChildren(ui.banner('error', 'The file was not accepted. Nothing was changed.', [err.message]));
    }).finally(function () { uploadState.busy = false; button.disabled = false; });
  }

  function resetData() {
    var msg = document.getElementById('upload-message');
    if (uploadState.busy) return;
    uploadState.busy = true;
    FX.api.post('/historical/reset', {}).then(function (r) {
      msg.replaceChildren(ui.banner('ok', null, [r.reset ? 'Switched back to the bundled FRED dataset.' : 'The bundled FRED dataset is already in use.']));
      if (r.reset) { FX.dataVersion += 1; document.getElementById('hist-form').reset(); return load({}); }
    }).catch(function (err) {
      msg.replaceChildren(ui.banner('error', null, [err.message]));
    }).finally(function () { uploadState.busy = false; });
  }

  FX.sections.register('historical', {
    init: function () {
      ui = FX.ui; fmt = FX.fmt;
      var form = document.getElementById('hist-form');
      form.addEventListener('submit', function (e) { e.preventDefault(); load(); });
      form.querySelector('[data-action="full"]').addEventListener('click', function () {
        if (lastDataset) load({ start_date: lastDataset.first_date, end_date: lastDataset.last_date });
      });
      var up = document.getElementById('upload-form');
      up.addEventListener('submit', function (e) { e.preventDefault(); upload(); });
      up.querySelector('[data-action="reset-data"]').addEventListener('click', resetData);
    },
    show: function () { if (state.version !== FX.dataVersion) load({}); }
  });
})();
