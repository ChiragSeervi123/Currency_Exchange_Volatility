/* =============================================================================
   chart-upload.js - the "Chart Upload" panel of the dashboard (the only input of
   the application) and the shared chart analysis.
   - Choose, drop or paste (Ctrl+V) a PNG/JPEG screenshot; it is previewed here.
   - "Analyze Chart" sends it to R (POST /chart/analyze). R reads the prices from
     the image and returns every analysis; this file keeps that result in
     FX.chart, and every page renders its part of it.
   - On page load the last analysis is fetched from R (GET /chart/current), so a
     refresh does not lose it while the backend keeps running.
   ============================================================================= */
(function () {
  var ui = FX.ui;
  var MAX_BYTES = 8 * 1024 * 1024;

  var form = document.getElementById('upload-form');
  var fileInput = document.getElementById('chart-file');
  var dropzone = document.getElementById('dropzone');
  var preview = document.getElementById('chart-preview');
  var fileLabel = document.getElementById('chart-file-name');
  var message = document.getElementById('upload-message');
  var submit = form.querySelector('button[type="submit"]');

  var selected = null;          // the chosen File
  var analysis = null;          // the current result from R
  var version = 0;
  var busy = false;
  var listeners = [];
  var readyPromise = null;

  function formatSize(bytes) {
    return bytes >= 1024 * 1024 ? FX.fmt.num(bytes / 1024 / 1024, 1) + ' MB' : FX.fmt.int(bytes / 1024) + ' KB';
  }

  function showError(text, title) { message.replaceChildren(ui.banner('error', title || null, [text])); }

  // ---- Choosing a file ---------------------------------------------------------------------
  function choose(file) {
    message.replaceChildren();
    if (!file) return;
    if (!/^image\/(png|jpeg)$/.test(file.type)) {
      showError('"' + (file.name || 'This file') + '" is not a PNG or JPEG image. Save the chart as PNG (TradingView: camera icon, "Download image") and try again.');
      return;
    }
    if (file.size > MAX_BYTES) {
      showError('The image is ' + formatSize(file.size) + '; the maximum is 8 MB.');
      return;
    }
    selected = file;
    if (preview.src) URL.revokeObjectURL(preview.src);
    preview.src = URL.createObjectURL(file);
    preview.hidden = false;
    dropzone.classList.add('has-file');
    fileLabel.textContent = (file.name || 'Pasted image') + ' · ' + formatSize(file.size);
    submit.disabled = false;
  }

  fileInput.addEventListener('change', function () { choose(fileInput.files[0]); });
  ['dragenter', 'dragover'].forEach(function (t) {
    dropzone.addEventListener(t, function (e) { e.preventDefault(); dropzone.classList.add('is-drag'); });
  });
  ['dragleave', 'drop'].forEach(function (t) {
    dropzone.addEventListener(t, function () { dropzone.classList.remove('is-drag'); });
  });
  dropzone.addEventListener('drop', function (e) {
    e.preventDefault();
    if (e.dataTransfer && e.dataTransfer.files.length) choose(e.dataTransfer.files[0]);
  });
  // A screenshot copied to the clipboard can be pasted straight onto the dashboard.
  document.addEventListener('paste', function (e) {
    if (document.getElementById('view-dashboard').hidden || !e.clipboardData) return;
    var item = Array.prototype.filter.call(e.clipboardData.items, function (i) { return /^image\/(png|jpeg)$/.test(i.type); })[0];
    if (!item) return;
    var blob = item.getAsFile();
    choose(new File([blob], 'pasted-chart.' + (blob.type === 'image/png' ? 'png' : 'jpg'), { type: blob.type }));
  });

  function readAsDataUrl(file) {
    return new Promise(function (resolve, reject) {
      var r = new FileReader();
      r.onload = function () { resolve(r.result); };
      r.onerror = function () { reject(new FX.ApiError('The image file could not be read by the browser.')); };
      r.readAsDataURL(file);
    });
  }

  // ---- Sending it to R ---------------------------------------------------------------------
  function setBusy(on) {
    busy = on;
    submit.disabled = on || !selected;
    submit.replaceChildren();
    if (on) {
      submit.appendChild(ui.el('span', { class: 'spinner', 'aria-hidden': 'true' }));
      submit.appendChild(ui.el('span', { text: 'R is reading the chart…' }));
    } else {
      submit.insertAdjacentHTML('beforeend', '<svg class="icon icon-line" aria-hidden="true"><use href="assets/icons.svg?v=3#i-scan-line"/></svg>');
      submit.appendChild(ui.el('span', { text: 'Analyze Chart' }));
    }
    document.getElementById('view-dashboard').classList.toggle('is-busy', on);
  }

  function analyze() {
    if (busy || !selected) return Promise.resolve(null);
    ui.clearFieldErrors(form);
    message.replaceChildren();
    var options = ui.readForm(form);
    setBusy(true);
    return readAsDataUrl(selected)
      .then(function (dataUrl) {
        return FX.api.upload('/chart/analyze', Object.assign({ image_base64: dataUrl, file_name: selected.name }, options));
      })
      .then(function (data) {
        set(data);
        var c = data.chart;
        message.replaceChildren(ui.banner('ok', null, ['R read ' + FX.fmt.int(c.n_points) + (c.type === 'candles' ? ' candles' : ' points') +
          ' from the chart. ' + c.scale.note]));
        return data;
      })
      .catch(function (err) {
        var field = err.field === 'image' ? null : err.field;
        if (field) {
          form.querySelector('details.advanced').open = true;
          ui.showFieldError(form, field, err.message);
        }
        showError(err.message + (err.code && !err.network ? ' [' + err.code + ']' : ''), field ? 'Please check the highlighted setting.' : null);
        return null;
      })
      .finally(function () { setBusy(false); });
  }

  form.addEventListener('submit', function (e) { e.preventDefault(); analyze(); });
  form.querySelector('[data-action="clear"]').addEventListener('click', function () {
    selected = null;
    form.reset();
    ui.clearFieldErrors(form);
    if (preview.src) URL.revokeObjectURL(preview.src);
    preview.removeAttribute('src');
    preview.hidden = true;
    dropzone.classList.remove('has-file');
    fileLabel.textContent = '';
    submit.disabled = true;
    message.replaceChildren();
    if (analysis) FX.api.post('/chart/reset').catch(function () {}).then(function () { set(null); });
  });

  // ---- The shared analysis ------------------------------------------------------------------
  function set(data) {
    analysis = data;
    version += 1;
    FX.dataVersion += 1;
    updatePill();
    listeners.forEach(function (fn) { fn(data); });
  }

  function updatePill() {
    var label = document.getElementById('pair-label');
    var title = document.getElementById('pair-title');
    var note = document.getElementById('pair-data-note');
    if (!analysis) {
      label.textContent = 'No chart yet';
      title.textContent = 'No chart uploaded yet';
      note.textContent = 'Upload a chart screenshot in the Chart Upload panel on the dashboard. Prices are read from the image; they are not a live feed.';
      return;
    }
    var c = analysis.chart;
    label.textContent = shortName(c.name) + (c.interval ? ' · ' + c.interval : '');
    title.textContent = c.name;
    note.textContent = FX.fmt.int(c.n_points) + (c.type === 'candles' ? ' candles' : ' points') + ' read from ' +
      (c.file_name || 'the uploaded image') + '. ' + c.scale.note + ' Prices are read from the image, not a live feed.';
  }

  function shortName(name) {
    var first = String(name || 'Chart').split(' · ')[0];
    return first.length > 22 ? first.slice(0, 21) + '…' : first;
  }

  // Fetch the analysis R still holds (after a page refresh). Resolves either way.
  function ready() {
    if (!readyPromise) {
      readyPromise = FX.api.get('/chart/current')
        .then(function (data) { if (data && !analysis) set(data); })
        .catch(function () { readyPromise = null; });
    }
    return readyPromise || Promise.resolve();
  }

  // Render a page section from the analysis: an empty state until a chart is uploaded,
  // and only again when the analysis has changed.
  function renderInto(rootId, state, renderFn) {
    return ready().then(function () {
      var root = document.getElementById(rootId);
      if (!analysis) {
        if (state.version === 'empty') return;
        FX.charts.destroyWithin(root);
        root.replaceChildren(emptyState());
        state.version = 'empty';
        return;
      }
      if (state.version === version) return;
      FX.charts.destroyWithin(root);
      root.replaceChildren();
      try {
        renderFn(analysis, root);
        state.version = version;
      } catch (err) {
        console.error(err);
        root.replaceChildren(ui.banner('error', null, ['The page could not display this result: ' + err.message]));
        state.version = -1;
      }
    });
  }

  function emptyState() {
    return ui.el('div', { class: 'empty empty-chart' }, [
      ui.el('p', { class: 'empty-title', text: 'No chart has been analysed yet.' }),
      ui.el('p', {}, ['Upload a chart screenshot in the ', ui.el('a', { class: 'link', href: '#/dashboard', text: 'Chart Upload panel' }),
        ' on the dashboard. Every result on this page is then computed by R from that chart.'])
    ]);
  }

  FX.chart = {
    current: function () { return analysis; },
    version: function () { return version; },
    onChange: function (fn) { listeners.push(fn); },
    ready: ready,
    renderInto: renderInto,
    analyze: analyze,
    choose: choose,                 // used by the browser tests
    isBusy: function () { return busy; }
  };
})();
