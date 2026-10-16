/* storage.js - the Storage page: a gallery of every chart you have uploaded, saved automatically by
   R (GET /storage) with its screenshot and full analysis. "Open" and "Report" make a saved chart the
   current analysis again (POST /storage/<id>/load) without re-uploading it; "Delete" and "Clear all"
   remove saved charts. This file only lists and acts on what R already computed and stored. */
(function () {
  var ui, fmt, el;

  function timeAgo(iso) {
    var d = new Date(iso);
    if (isNaN(d.getTime())) return '';
    var secs = Math.max(0, (Date.now() - d.getTime()) / 1000);
    var steps = [[60, 'second'], [60, 'minute'], [24, 'hour'], [7, 'day'], [4.345, 'week'], [12, 'month'], [Infinity, 'year']];
    var value = secs, unit = 'second';
    for (var i = 0; i < steps.length; i++) {
      if (value < steps[i][0]) { unit = steps[i][1]; break; }
      value /= steps[i][0]; unit = steps[i][1];
    }
    value = Math.max(1, Math.round(value));
    return value === 1 ? '1 ' + unit + ' ago' : value + ' ' + unit + 's ago';
  }

  function verdictLabel(v) { return { up: 'Up', down: 'Down', sideways: 'Close to mean' }[v] || v; }

  function card(entry) {
    var changeClass = entry.change_pct >= 0 ? 'is-up' : 'is-down';
    var thumb = el('div', { class: 'storage-thumb' }, [
      el('img', { loading: 'lazy', alt: 'Thumbnail of ' + entry.name, src: FX.config.apiBase + '/storage/' + entry.id + '/image' })
    ]);
    var openBtn = el('button', { class: 'btn btn-secondary btn-small', type: 'button', text: 'Open' });
    var reportBtn = el('button', { class: 'btn btn-secondary btn-small', type: 'button', text: 'Report' });
    var delBtn = el('button', { class: 'icon-btn', type: 'button', 'aria-label': 'Delete this saved chart' });
    delBtn.innerHTML = '<svg class="icon" aria-hidden="true"><use href="assets/icons.svg?v=3#i-trash-2"/></svg>';

    var node = el('article', { class: 'storage-card' }, [
      thumb,
      el('div', { class: 'storage-body' }, [
        el('p', { class: 'storage-name', title: entry.name, text: entry.name }),
        el('p', { class: 'storage-meta', text: FX.fmt.int(entry.n_points) + (entry.type === 'candles' ? ' candles' : ' points') +
          (entry.interval ? ' · ' + entry.interval : '') + ' · ' + timeAgo(entry.saved_at) }),
        el('div', { class: 'storage-stats' }, [
          el('span', { class: 'storage-price', text: fmt.price(entry.last_price) }),
          el('span', { class: 'storage-change ' + changeClass, text: fmt.signedPct(entry.change_pct) }),
          el('span', { class: 'verdict-chip verdict-' + entry.verdict, text: verdictLabel(entry.verdict) })
        ])
      ]),
      el('div', { class: 'storage-actions' }, [openBtn, reportBtn, delBtn])
    ]);
    openBtn.addEventListener('click', function () { act(entry.id, '#/dashboard', openBtn); });
    reportBtn.addEventListener('click', function () { act(entry.id, '#/report', reportBtn); });
    delBtn.addEventListener('click', function () { remove(entry.id, node); });
    return node;
  }

  function act(id, hash, btn) {
    btn.disabled = true;
    FX.chart.loadFromStorage(id).then(function () {
      location.hash = hash;
    }).catch(function (err) {
      showMessage(err.message || 'This saved chart could not be loaded.', true);
      btn.disabled = false;
    });
  }

  function remove(id, node) {
    if (!window.confirm('Remove this chart from storage? This cannot be undone.')) return;
    FX.api.post('/storage/' + id + '/delete', {}).then(function () {
      node.remove();
      refreshCount();
    }).catch(function (err) {
      showMessage(err.message || 'This chart could not be removed.', true);
    });
  }

  function showMessage(text, isError) {
    var box = document.getElementById('storage-message');
    if (box) box.replaceChildren(ui.banner(isError ? 'error' : 'ok', null, [text]));
  }

  function refreshCount() {
    var n = document.querySelectorAll('.storage-card').length;
    var countEl = document.getElementById('storage-count');
    if (countEl) countEl.textContent = n === 1 ? '1 saved chart' : n + ' saved charts';
    var clearBtn = document.getElementById('storage-clear');
    if (clearBtn) clearBtn.disabled = n === 0;
    var grid = document.getElementById('storage-grid');
    if (grid && n === 0) render({ charts: [], max: 0 }, document.getElementById('storage-results'));
  }

  function clearAll() {
    if (!window.confirm('Remove every saved chart? This cannot be undone.')) return;
    FX.api.post('/storage/clear', {}).then(function () { load(); })
      .catch(function (err) { showMessage(err.message || 'Storage could not be cleared.', true); });
  }

  function render(data, root) {
    root.replaceChildren();
    var charts = data.charts || [];
    root.appendChild(el('div', { class: 'storage-toolbar' }, [
      el('span', { class: 'storage-count', id: 'storage-count', text: charts.length === 1 ? '1 saved chart' : charts.length + ' saved charts' }),
      el('button', { class: 'btn btn-secondary btn-small', type: 'button', id: 'storage-clear', disabled: charts.length === 0, text: 'Clear all' })
    ]));
    root.appendChild(el('div', { id: 'storage-message' }));
    if (!charts.length) {
      root.appendChild(el('div', { class: 'empty empty-chart' }, [
        el('p', { class: 'empty-title', text: 'Nothing saved yet.' }),
        el('p', {}, ['Upload a chart in the ', el('a', { class: 'link', href: '#/dashboard', text: 'Chart Upload panel' }),
          ' and it will appear here automatically, with its full report.'])
      ]));
      return;
    }
    var grid = el('div', { class: 'storage-grid', id: 'storage-grid' }, charts.map(card));
    root.appendChild(grid);
    document.getElementById('storage-clear').addEventListener('click', clearAll);
  }

  // Always fetched fresh on every visit (a chart may have just been saved, or removed elsewhere).
  function load() {
    var root = document.getElementById('storage-results');
    if (!root.querySelector('.storage-grid, .empty-chart')) {
      root.replaceChildren(el('div', { class: 'empty', text: 'Loading your saved charts…' }));
    }
    return FX.api.get('/storage').then(function (data) {
      render(data, root);
    }).catch(function (err) {
      root.replaceChildren(ui.banner('error', null, [err.message || 'The storage library could not be loaded.']));
    });
  }

  FX.sections.register('storage', {
    init: function () { ui = FX.ui; fmt = FX.fmt; el = ui.el; },
    show: load
  });
})();
