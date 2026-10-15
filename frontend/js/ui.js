/* =============================================================================
   ui.js - small DOM helpers shared by all sections: building tables and cards,
   reading forms, showing validation messages, and running a request with
   loading / error / empty states. Text from the server is always inserted with
   textContent (never as HTML).
   ============================================================================= */
(function () {
  function el(tag, attrs, children) {
    var node = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      var v = attrs[k];
      if (v == null || v === false) return;
      if (k === 'text') node.textContent = v;
      else if (k === 'class') node.className = v;
      else node.setAttribute(k, v === true ? '' : v);
    });
    (children || []).forEach(function (c) {
      if (c == null) return;
      node.appendChild(typeof c === 'string' ? document.createTextNode(c) : c);
    });
    return node;
  }

  function cards(items) {
    return el('div', { class: 'cards' }, items.filter(Boolean).map(function (it) {
      return el('div', { class: 'card' }, [
        el('p', { class: 'card-label', text: it.label }),
        el('p', { class: 'card-value', text: it.value }),
        it.sub ? el('p', { class: 'card-sub', text: it.sub }) : null
      ]);
    }));
  }

  // columns: [{ key, label, num (bool), format (fn(value,row)) }]
  function table(opts) {
    var thead = el('thead', {}, [el('tr', {}, opts.columns.map(function (c) {
      return el('th', { class: c.num ? 'num' : null, scope: 'col', text: c.label });
    }))]);
    var tbody = el('tbody', {}, (opts.rows || []).map(function (row) {
      return el('tr', {}, opts.columns.map(function (c) {
        var v = row[c.key];
        var shown = c.format ? c.format(v, row) : (typeof v === 'number' ? FX.fmt.sig(v, 6) : FX.fmt.text(v));
        return el('td', { class: c.num ? 'num' : null, text: shown });
      }));
    }));
    return el('div', { class: 'table-wrap' + (opts.compact ? ' compact' : '') }, [
      el('table', {}, [opts.caption ? el('caption', { text: opts.caption }) : null, thead, tbody])
    ]);
  }

  // Two-column "measure | value" table from [[label, value], ...]
  function kvTable(caption, pairs) {
    return table({
      caption: caption,
      columns: [{ key: 'k', label: 'Measure' }, { key: 'v', label: 'Value', num: true, format: function (v) { return v; } }],
      rows: pairs.filter(Boolean).map(function (p) { return { k: p[0], v: p[1] }; })
    });
  }

  function banner(kind, title, items) {
    var children = [];
    if (title) children.push(el('p', { text: title }));
    if (items && items.length) children.push(el('ul', {}, items.map(function (t) { return el('li', { text: t }); })));
    return el('div', { class: 'banner banner-' + kind, role: kind === 'error' ? 'alert' : null }, children);
  }

  // Returns an empty fragment when there is nothing to show, so callers can always appendChild().
  function warnings(list, title) {
    var items = (list || []).filter(Boolean);
    if (!items.length) return document.createDocumentFragment();
    return banner('warn', title || 'Notes and warnings from R', items);
  }

  function chartCard(opts) {
    return el('figure', { class: 'chart-card' }, [
      el('figcaption', { text: opts.title }),
      opts.note ? el('p', { class: 'chart-note', text: opts.note }) : null,
      el('div', { class: 'chart-box' + (opts.tall ? ' tall' : '') }, [
        el('canvas', { id: opts.id, role: 'img', 'aria-label': opts.title })
      ]),
      el('details', { class: 'table-twin' }, [
        el('summary', { text: 'Show the chart data as a table' }),
        el('div', { 'data-twin-for': opts.id })
      ])
    ]);
  }

  function section(title, nodes) {
    return el('div', {}, [el('h3', { class: 'subhead', text: title })].concat(nodes));
  }

  function setVisible(node, visible) { if (node) node.hidden = !visible; }

  // Show/hide fields that declare data-<attr>="value1 value2" depending on the current value.
  function toggleBy(form, attr, value) {
    var key = 'hidden' + attr.replace(/-/g, '');
    form.querySelectorAll('[data-' + attr + ']').forEach(function (node) {
      var allowed = node.getAttribute('data-' + attr).split(/\s+/);
      node.dataset[key] = allowed.indexOf(value) === -1 ? '1' : '';
      refreshHidden(node);
    });
  }
  function refreshHidden(node) {
    var hide = Object.keys(node.dataset).some(function (k) { return k.indexOf('hidden') === 0 && node.dataset[k] === '1'; });
    node.hidden = hide;
  }

  function setNested(obj, name, value) {
    var parts = name.split('.');
    var cur = obj;
    for (var i = 0; i < parts.length - 1; i++) { cur[parts[i]] = cur[parts[i]] || {}; cur = cur[parts[i]]; }
    cur[parts[parts.length - 1]] = value;
  }

  // Read the visible named controls of a form into a JSON-ready object.
  // Empty optional fields are omitted so that R applies its defaults.
  function readForm(form) {
    var out = {};
    Array.prototype.forEach.call(form.elements, function (ctrl) {
      if (!ctrl.name || ctrl.disabled || ctrl.type === 'file' || ctrl.closest('[hidden]')) return;
      if ((ctrl.type === 'radio' || ctrl.type === 'checkbox') && !ctrl.checked) return;
      var v = ctrl.value;
      if (v === '' || v == null) return;
      if (ctrl.type === 'number' || ctrl.hasAttribute('data-number')) {
        var n = Number(v);
        if (!isFinite(n)) return;
        v = n;
      } else if (typeof v === 'string') {
        v = v.trim();
      }
      setNested(out, ctrl.name, v);
    });
    return out;
  }

  function clearFieldErrors(form) {
    form.querySelectorAll('.field-error').forEach(function (n) { n.remove(); });
    form.querySelectorAll('[aria-invalid="true"]').forEach(function (n) { n.removeAttribute('aria-invalid'); });
  }

  function showFieldError(form, name, message) {
    var ctrl = form.querySelector('[name="' + name + '"]');
    if (!ctrl || ctrl.closest('[hidden]')) return false;
    ctrl.setAttribute('aria-invalid', 'true');
    var msg = el('p', { class: 'field-error', id: ctrl.id + '-error', text: message });
    ctrl.setAttribute('aria-describedby', msg.id);
    ctrl.closest('.field').appendChild(msg);
    return true;
  }

  // Immediate feedback using the browser's constraint validation (min, max, step, required).
  // The R backend validates everything again; this only saves a round trip.
  function clientValidate(form) {
    clearFieldErrors(form);
    var first = null;
    Array.prototype.forEach.call(form.elements, function (ctrl) {
      if (!ctrl.name || !ctrl.willValidate || ctrl.closest('[hidden]')) return;
      if (!ctrl.checkValidity()) {
        var label = form.querySelector('label[for="' + ctrl.id + '"]');
        var name = label ? label.childNodes[0].textContent.trim() : ctrl.name;
        var m;
        if (ctrl.validity.valueMissing) m = name + ' is required.';
        else if (ctrl.validity.badInput) m = name + ' must be a number.';
        else if (ctrl.validity.rangeUnderflow) m = name + ' must be at least ' + ctrl.min + '.';
        else if (ctrl.validity.rangeOverflow) m = name + ' must be at most ' + ctrl.max + '.';
        else if (ctrl.validity.stepMismatch) m = name + ' must be a whole number.';
        else m = ctrl.validationMessage;
        showFieldError(form, ctrl.name, m);
        if (!first) first = ctrl;
      }
    });
    if (first) first.focus();
    return !first;
  }

  // Run one analysis: guards against double submission, shows loading state,
  // renders the R result or an actionable error. Resolves to true on success,
  // false on failure and null when nothing was sent.
  function runAnalysis(opts) {
    var form = opts.form, results = opts.results;
    if (opts.state.busy) return Promise.resolve(null);
    if (form && opts.validate !== false && !clientValidate(form)) return Promise.resolve(null);
    opts.state.busy = true;
    var button = form ? form.querySelector('button[type="submit"]') : null;
    var label = button ? button.textContent : '';
    if (button) { button.disabled = true; button.textContent = 'Running in R…'; }
    results.classList.add('is-loading');
    results.setAttribute('aria-busy', 'true');
    if (!results.querySelector('.card, .chart-card, .table-wrap')) {
      results.replaceChildren(el('div', { class: 'empty', text: opts.loadingText || 'R is calculating…' }));
    }
    return Promise.resolve()
      .then(opts.request)
      .then(function (data) {
        FX.charts.destroyWithin(results);
        results.replaceChildren();
        opts.render(data, results);
        return true;
      })
      .catch(function (err) {
        FX.charts.destroyWithin(results);
        if (!(err instanceof FX.ApiError)) {
          console.error(err);
          err = { message: 'The page could not display this result: ' + (err && err.message), code: 'CLIENT_ERROR' };
        }
        var shownOnField = form && err.field ? showFieldError(form, err.field, err.message) : false;
        results.replaceChildren(banner('error', shownOnField ? 'Please correct the highlighted input.' : null,
          [err.message + (err.code && !err.network && err.code !== 'CLIENT_ERROR' ? ' [' + err.code + ']' : '')]));
        return false;
      })
      .finally(function () {
        opts.state.busy = false;
        if (button) { button.disabled = false; button.textContent = label; }
        results.classList.remove('is-loading');
        results.removeAttribute('aria-busy');
      });
  }

  FX.ui = {
    el: el, cards: cards, table: table, kvTable: kvTable, banner: banner, warnings: warnings,
    chartCard: chartCard, section: section, setVisible: setVisible, toggleBy: toggleBy,
    readForm: readForm, clientValidate: clientValidate, clearFieldErrors: clearFieldErrors,
    showFieldError: showFieldError, runAnalysis: runAnalysis
  };
})();
