/* =============================================================================
   api.js - the client for the R Plumber API.
   Every request returns the "data" part of the R response, or throws an
   FX.ApiError with a message that is safe to show to the user.
   Identical requests that are already running are shared, not sent twice.
   ============================================================================= */
(function () {
  function ApiError(message, opts) {
    this.name = 'ApiError';
    this.message = message;
    opts = opts || {};
    this.status = opts.status || 0;
    this.code = opts.code || 'UNKNOWN';
    this.field = opts.field || null;
    this.network = !!opts.network;
  }
  ApiError.prototype = Object.create(Error.prototype);
  FX.ApiError = ApiError;

  var inFlight = new Map();

  function offlineMessage() {
    return 'Cannot reach the R backend at ' + FX.config.apiBase + '. Start it from the project folder with ' +
      FX.config.startCommand + ' (or double-click start_fxlab.bat), then try again.';
  }

  async function send(method, path, body) {
    var controller = new AbortController();
    var timer = setTimeout(function () { controller.abort(); }, FX.config.requestTimeoutMs);
    var response;
    try {
      response = await fetch(FX.config.apiBase + path, {
        method: method,
        headers: body !== undefined ? { 'Content-Type': 'application/json' } : {},
        body: body !== undefined ? JSON.stringify(body) : undefined,
        signal: controller.signal
      });
    } catch (err) {
      if (err && err.name === 'AbortError') {
        throw new ApiError('The R backend did not answer within ' + FX.config.requestTimeoutMs / 1000 +
          ' seconds. Try a smaller screenshot.', { code: 'TIMEOUT', network: true });
      }
      FX.status && FX.status.markOffline();
      throw new ApiError(offlineMessage(), { code: 'NETWORK', network: true });
    } finally {
      clearTimeout(timer);
    }

    var payload = null;
    try {
      payload = await response.json();
    } catch (e) {
      throw new ApiError('The server answered with something that is not FXLab JSON (HTTP ' + response.status + ').',
        { status: response.status, code: 'BAD_RESPONSE' });
    }
    FX.status && FX.status.markOnline();
    if (!response.ok || !payload || payload.success !== true) {
      var err = (payload && payload.error) || {};
      throw new ApiError((payload && payload.message) || ('Request failed (HTTP ' + response.status + ').'),
        { status: response.status, code: err.code, field: err.field });
    }
    return payload.data;
  }

  function request(method, path, body) {
    var key = method + ' ' + path + ' ' + (body === undefined ? '' : JSON.stringify(body));
    if (inFlight.has(key)) return inFlight.get(key);
    var p = send(method, path, body).finally(function () { inFlight.delete(key); });
    inFlight.set(key, p);
    return p;
  }

  function query(params) {
    var parts = [];
    Object.keys(params || {}).forEach(function (k) {
      var v = params[k];
      if (v !== undefined && v !== null && v !== '') parts.push(encodeURIComponent(k) + '=' + encodeURIComponent(v));
    });
    return parts.length ? '?' + parts.join('&') : '';
  }

  FX.api = {
    get: function (path, params) { return request('GET', path + query(params)); },
    post: function (path, body) { return request('POST', path, body || {}); },
    // Large one-off requests (the chart image): sent directly, without the duplicate-request cache.
    upload: function (path, body) { return send('POST', path, body); },
    health: function () { return send('GET', '/health'); },
    offlineMessage: offlineMessage
  };
})();
