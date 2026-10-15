/* format.js - display formatting only (rounding for the screen). No statistics here. */
(function () {
  function isNum(x) { return typeof x === 'number' && isFinite(x); }

  FX.fmt = {
    // Fixed number of decimals with thousands separators.
    num: function (x, digits) {
      if (!isNum(x)) return '—';
      var d = digits == null ? 4 : digits;
      return x.toLocaleString('en-US', { minimumFractionDigits: d, maximumFractionDigits: d });
    },
    // Significant figures, switching to scientific notation for very small/large values.
    sig: function (x, s) {
      if (!isNum(x)) return '—';
      var sf = s || 4;
      if (x === 0) return '0';
      var a = Math.abs(x);
      if (a < 1e-4 || a >= 1e7) return x.toExponential(sf - 1);
      return Number(x.toPrecision(sf)).toLocaleString('en-US', { maximumFractionDigits: 10 });
    },
    int: function (x) { return isNum(x) ? Math.round(x).toLocaleString('en-US') : '—'; },
    pct: function (x, digits) { return isNum(x) ? FX.fmt.num(x, digits == null ? 2 : digits) + '%' : '—'; },
    // Probabilities in [0, 1] shown as percentages.
    prob: function (x, digits) { return isNum(x) ? FX.fmt.num(100 * x, digits == null ? 2 : digits) + '%' : '—'; },
    pval: function (p) {
      if (!isNum(p)) return '—';
      if (p < 0.0001) return '< 0.0001';
      return p.toFixed(4);
    },
    // "p = 0.0123" or "p < 0.0001"
    pText: function (p) {
      if (!isNum(p)) return 'p = —';
      return p < 0.0001 ? 'p < 0.0001' : 'p = ' + p.toFixed(4);
    },
    // R writes hypotheses in ASCII ("H1: mu != 0"); show the usual symbols.
    symbols: function (s) {
      return String(s).replace(/!=/g, '≠').replace(/<=/g, '≤').replace(/>=/g, '≥')
        .replace(/\bmu(\d?)\b/g, 'μ$1').replace(/\bsigma(\d?)\^2/g, 'σ$1²').replace(/\bsigma(\d?)\b/g, 'σ$1');
    },
    rate: function (x) { return isNum(x) ? FX.fmt.num(x, 4) : '—'; },
    text: function (x) { return x == null || x === '' ? '—' : String(x); }
  };
})();
