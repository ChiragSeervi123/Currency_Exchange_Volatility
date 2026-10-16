/* =============================================================================
   config.js - the ONE place where the frontend learns where the R API lives.
   - Served by the R backend (http://127.0.0.1:8000/app/): same origin is used.
   - Served by another local server (e.g. an editor's live preview): the API is
     expected at DEFAULT_API_BASE. Override by defining window.FXLAB_API_BASE
     before this script, e.g. <script>window.FXLAB_API_BASE = "http://127.0.0.1:8001"</script>
   ============================================================================= */
window.FX = window.FX || {};

(function () {
  var DEFAULT_API_BASE = 'http://127.0.0.1:8000';
  var servedByBackend = /^https?:$/.test(location.protocol) && location.pathname.indexOf('/app') === 0;

  FX.config = {
    apiBase: window.FXLAB_API_BASE || (servedByBackend ? location.origin : DEFAULT_API_BASE),
    requestTimeoutMs: 90000,      // reading a large screenshot (OCR) can take several seconds
    healthIntervalMs: 30000,      // how often the connection indicator re-checks the backend
    openedFromFile: location.protocol === 'file:',
    startCommand: '"C:\\Program Files\\R\\R-4.6.1\\bin\\Rscript.exe" backend/main.R'
  };

  // Registry of page sections (each file in js/sections/ registers itself).
  FX.sections = {
    list: {},
    register: function (name, section) { FX.sections.list[name] = section; }
  };
  // Increases whenever the analysed chart changes (upload / clear), so sections re-render.
  FX.dataVersion = 0;
})();
