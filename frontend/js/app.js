/* =============================================================================
   app.js - navigation (hash routes such as #/hypothesis), the floating rail's
   active state, the R backend status button, the chart popover, the clock,
   and the shared /defaults request.
   ============================================================================= */
(function () {
  var ui = FX.ui;
  var VIEWS = ['dashboard', 'descriptive', 'distributions', 'hypothesis', 'regression', 'data', 'report', 'about'];
  var initialised = {};
  var current = null;

  // ---- Shared defaults (GET /defaults), cached after the first success ----------------
  var defaultsPromise = null;
  FX.defaults = function () {
    if (!defaultsPromise) {
      defaultsPromise = FX.api.get('/defaults')
        .then(function (d) { FX.lastDefaults = d; return d; })
        .catch(function (err) { defaultsPromise = null; throw err; });
    }
    return defaultsPromise;
  };

  // ---- Popovers (uploaded chart, backend status) ----------------------------------------
  function bindPopover(buttonId, popId) {
    var btn = document.getElementById(buttonId), pop = document.getElementById(popId);
    btn.addEventListener('click', function () {
      var open = pop.hidden;
      closePopovers();
      pop.hidden = !open;
      btn.setAttribute('aria-expanded', String(open));
    });
    return { btn: btn, pop: pop };
  }
  var popovers = [bindPopover('pair-button', 'pair-pop'), bindPopover('status-button', 'status-pop')];
  function closePopovers() {
    popovers.forEach(function (p) { p.pop.hidden = true; p.btn.setAttribute('aria-expanded', 'false'); });
  }
  document.addEventListener('click', function (e) { if (!e.target.closest('.popover-wrap')) closePopovers(); });
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') closePopovers(); });
  document.querySelectorAll('.popover a').forEach(function (a) { a.addEventListener('click', closePopovers); });

  // ---- Backend status ---------------------------------------------------------------------
  var statusBtn = document.getElementById('status-button');
  var statusTitle = document.getElementById('status-title');
  var statusDetail = document.getElementById('status-detail');
  var online = null;

  function setStatus(isOnline, title, detail) {
    var changed = online !== isOnline;
    online = isOnline;
    statusBtn.classList.toggle('is-online', isOnline === true);
    statusBtn.classList.toggle('is-offline', isOnline === false);
    statusBtn.setAttribute('aria-label', 'R backend status: ' + title);
    statusTitle.textContent = title;
    statusDetail.textContent = detail;
    var banner = document.getElementById('global-banner');
    if (isOnline === false) {
      banner.replaceChildren(ui.el('p', { text: FX.config.openedFromFile
        ? 'This page was opened directly from disk. Start the R backend and open http://127.0.0.1:8000/ instead (the backend serves this page).'
        : FX.api.offlineMessage() }));
      banner.hidden = false;
    } else {
      banner.hidden = true;
    }
    // When the backend comes back, let the visible section reload what it needs.
    if (changed && isOnline === true && current && FX.sections.list[current] && initialised[current]) {
      FX.sections.list[current].show();
    }
  }

  function checkHealth() {
    return FX.api.health().then(function (h) {
      setStatus(true, 'R backend connected',
        h.r_version.replace(/ \(.*\)/, '') + ' · plumber ' + h.plumber_version + ' · ' + h.ocr_engine + ' · ' +
        (h.chart_loaded ? 'a chart is loaded' : 'no chart uploaded yet') + '. All statistics are computed in R.');
    }).catch(function () {
      setStatus(false, 'R backend offline', FX.api.offlineMessage());
    });
  }

  FX.status = {
    markOffline: function () { if (online !== false) setStatus(false, 'R backend offline', FX.api.offlineMessage()); },
    markOnline: function () { if (online !== true) checkHealth(); },
    check: checkHealth
  };
  document.getElementById('status-retry').addEventListener('click', function () {
    statusTitle.textContent = 'Checking the R backend…';
    checkHealth();
  });

  // ---- Clock (the visitor's own date and time; not related to the data's date) ------------
  function tick() {
    var now = new Date();
    document.getElementById('clock-date').textContent =
      now.toLocaleDateString('en-GB', { weekday: 'short', day: 'numeric', month: 'short', year: 'numeric' }).replace(/^(\w+)/, '$1,').replace(',,', ',');
    document.getElementById('clock-time').textContent =
      now.toLocaleTimeString('en-US', { hour: 'numeric', minute: '2-digit' });
  }
  tick();
  setInterval(tick, 20000);

  // ---- Navigation ------------------------------------------------------------------------
  var nav = document.getElementById('app-nav');

  function route() {
    var name = (location.hash.replace(/^#\/?/, '') || 'dashboard').split('?')[0];
    if (VIEWS.indexOf(name) === -1) name = 'dashboard';
    VIEWS.forEach(function (v) { document.getElementById('view-' + v).hidden = v !== name; });
    nav.querySelectorAll('a[data-view]').forEach(function (a) {
      if (a.dataset.view.split(' ').indexOf(name) >= 0) a.setAttribute('aria-current', 'page');
      else a.removeAttribute('aria-current');
    });
    document.body.classList.toggle('stage', name === 'dashboard');
    var view = document.getElementById('view-' + name);
    document.title = 'FXLab · ' + view.dataset.title;
    if (current !== null && current !== name) document.getElementById('main').focus({ preventScroll: true });
    current = name;
    window.scrollTo(0, 0);
    closePopovers();
    var section = FX.sections.list[name];
    if (section) {
      if (!initialised[name]) { section.init(); initialised[name] = true; }
      section.show();
    }
  }

  window.addEventListener('hashchange', route);
  route();
  checkHealth();
  setInterval(checkHealth, FX.config.healthIntervalMs);
})();
