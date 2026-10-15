/* =============================================================================
   sim-params.js - the "Simulation Parameters" panel of the Exchange Rate
   Dashboard. It is a fixed part of that page (not a pop-up): it has no
   open/close behaviour and does not appear on any other page.
   - Loads the default inputs from R (GET /defaults).
   - Run Simulation validates the inputs and sends them to R (POST /simulate)
     through FX.simulation.run(); Reset Parameters restores the defaults.
   Also defines FX.simForm, small form helpers (modelLabel is also used by the
   Dashboard and Monte Carlo pages).
   ============================================================================= */
(function () {
  var ui = FX.ui;

  // ---- Simulation-form helpers --------------------------------------------------------------
  FX.simForm = {
    // Fill a simulation form's model list and values from GET /defaults.
    setup: function (form, defaults, opts) {
      var select = form.querySelector('select[name="model"]');
      select.replaceChildren();
      defaults.simulation_models.forEach(function (m) {
        select.appendChild(FX.ui.el('option', { value: m.id, text: m.label }));
      });
      select.onchange = function () { FX.simForm.toggleModel(form); };
      FX.simForm.fill(form, defaults, opts);
    },
    fill: function (form, defaults, opts) {
      var s = defaults.simulation;
      var values = Object.assign({
        start_rate: s.start_rate, mu_daily_pct: Number(s.mu_daily_pct.toPrecision(4)),
        sigma_daily_pct: Number(s.sigma_daily_pct.toPrecision(4)), days: s.days, n_paths: s.n_paths,
        model: s.model, t_df: s.t_df, seed: s.seed, threshold: s.threshold, n_display_paths: s.n_display_paths
      }, opts && opts.overrides);
      Object.keys(values).forEach(function (k) {
        var ctrl = form.querySelector('[name="' + k + '"]');
        if (ctrl && values[k] != null) ctrl.value = values[k];
      });
      FX.ui.clearFieldErrors(form);
      FX.simForm.toggleModel(form);
    },
    toggleModel: function (form) {
      FX.ui.toggleBy(form, 'show-when', form.querySelector('select[name="model"]').value);
    },
    modelLabel: function (defaults, id) {
      var m = (defaults.simulation_models || []).filter(function (x) { return x.id === id; })[0];
      return m ? m.label : id;
    }
  };

  // ---- The panel -----------------------------------------------------------------------------
  var form = document.getElementById('params-form');
  var message = document.getElementById('params-message');
  var periodInput = form.querySelector('[name="period"]');
  var unitSelect = form.querySelector('[name="period_unit"]');
  var periodHint = document.getElementById('sp-period-hint');
  var defaults = null;
  var defaultsPromise = null;
  var running = false;

  function tradingDays() {
    return Math.round(Number(periodInput.value) * Number(unitSelect.value || 1));
  }

  function updatePeriodHint() {
    var unit = Number(unitSelect.value || 1);
    var n = Number(periodInput.value);
    periodHint.textContent = unit > 1 && isFinite(n) && n > 0 ? '= ' + FX.fmt.int(n * unit) + ' trading days' : '';
  }

  function fillFromDefaults() {
    FX.simForm.setup(form, defaults);
    periodInput.value = defaults.simulation.days;
    unitSelect.value = '1';
    updatePeriodHint();
    message.replaceChildren();
  }

  // Load GET /defaults once (retried later if the backend was offline).
  function ready() {
    if (defaults) return Promise.resolve(defaults);
    if (!defaultsPromise) {
      defaultsPromise = FX.defaults().then(function (d) {
        defaults = d;
        fillFromDefaults();
        return d;
      }).catch(function (err) {
        defaultsPromise = null;
        message.replaceChildren(ui.banner('error', 'Defaults could not be loaded.', [err.message]));
        throw err;
      });
    }
    return defaultsPromise;
  }

  // The request body for POST /simulate (all statistics are computed by R). Values are read from
  // the inputs directly, so this also works while the Dashboard page is not on screen (for example
  // when the Monte Carlo page triggers the first, default simulation).
  function body() {
    function num(name) {
      var v = form.elements[name].value;
      return v === '' ? undefined : Number(v);
    }
    var model = form.elements.model.value;
    var nPaths = num('n_paths');
    var b = {
      start_rate: num('start_rate'), mu_daily_pct: num('mu_daily_pct'), sigma_daily_pct: num('sigma_daily_pct'),
      days: tradingDays(), n_paths: nPaths, model: model,
      n_display_paths: Math.min(nPaths || 1, defaults ? defaults.limits.sim_max_display_paths : 500)
    };
    if (model === 'gbm_student_t' && num('t_df') !== undefined) b.t_df = num('t_df');
    if (num('seed') !== undefined) b.seed = num('seed');
    return b;
  }

  function setRunning(on) {
    running = on;
    var btn = form.querySelector('button[type="submit"]');
    btn.disabled = on;
    btn.replaceChildren();
    if (on) {
      btn.appendChild(ui.el('span', { class: 'spinner', 'aria-hidden': 'true' }));
      btn.appendChild(ui.el('span', { text: 'Running in R…' }));
    } else {
      btn.insertAdjacentHTML('beforeend', '<svg class="icon" aria-hidden="true"><use href="assets/icons.svg#i-play"/></svg>');
      btn.appendChild(ui.el('span', { text: 'Run Simulation' }));
    }
  }

  // Validate, send to R, and let the Dashboard (and Monte Carlo page) render the result.
  function run(opts) {
    opts = opts || {};
    if (running) return Promise.resolve(null);
    message.replaceChildren();
    if (!opts.auto && !ui.clientValidate(form)) return Promise.resolve(null);
    setRunning(true);
    return FX.simulation.run(body())
      .then(function (data) {
        if (!opts.auto) {
          message.replaceChildren(ui.banner('ok', null, ['Simulation completed in R: ' + FX.fmt.int(data.inputs.n_paths) +
            ' paths × ' + FX.fmt.int(data.inputs.days) + ' trading days (seed ' + data.seed + ').']));
        }
        return data;
      })
      .catch(function (err) {
        var field = err.field === 'days' ? 'period' : err.field;
        var shown = field ? ui.showFieldError(form, field, err.message) : false;
        message.replaceChildren(ui.banner('error', shown ? 'Please correct the highlighted input.' : null, [err.message]));
        return null;
      })
      .finally(function () { setRunning(false); });
  }

  form.addEventListener('submit', function (e) { e.preventDefault(); run(); });
  form.querySelector('[data-action="reset"]').addEventListener('click', function () {
    if (defaults) fillFromDefaults(); else ready().catch(function () {});
  });
  periodInput.addEventListener('input', updatePeriodHint);
  unitSelect.addEventListener('change', updatePeriodHint);

  FX.simParams = {
    ready: ready,
    run: run,
    requestBody: body             // the exact JSON sent to POST /simulate (used by the browser tests)
  };
})();
