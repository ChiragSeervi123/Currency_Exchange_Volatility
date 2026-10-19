/* =============================================================================
   splash.js - the 3D welcome page (splash.html), built with Three.js.
   One set of 484 instanced boxes morphs between five syllabus "scenes":
     0 candlesticks read from a chart   1 a histogram stacked from observations
     2 a bivariate normal density        3 a two-tailed Z test (critical region)
     4 scatter points around a regression line
   Interaction: drag to orbit (inertia), Ctrl+wheel / pinch / buttons to zoom, hover for a
   tooltip, click for a ripple, chips or keys 1-5 to switch, sliders per scene, auto tour.
   The shapes are illustrations of the topics; the app's analysis itself is computed in R.
   ============================================================================= */
import * as THREE from '../assets/vendor/three.module.min.js';

var reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
var $ = function (id) { return document.getElementById(id); };

/* ---- Page features that work without WebGL ------------------------------------------ */
(function checkBackend() {
  var pill = $('backend-pill'), label = $('backend-label');
  fetch('/health', { cache: 'no-store' }).then(function (r) {
    if (!r.ok) throw new Error('HTTP ' + r.status);
    pill.classList.add('is-online'); label.textContent = 'R backend online';
    pill.title = 'R backend online';
  }).catch(function () {
    pill.classList.add('is-offline'); label.textContent = 'R backend offline';
    pill.title = 'R backend offline: start it with start_fxlab.bat or splash.bat';
  });
})();

(function revealOnScroll() {
  var items = document.querySelectorAll('.reveal');
  if (!('IntersectionObserver' in window) || reduceMotion) {
    items.forEach(function (el) { el.classList.add('is-in'); });
    return;
  }
  var io = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (e.isIntersecting) { e.target.classList.add('is-in'); io.unobserve(e.target); }
    });
  }, { threshold: 0.18 });
  items.forEach(function (el) { io.observe(el); });
})();

(function tiltCards() {
  if (reduceMotion) return;
  document.querySelectorAll('[data-tilt]').forEach(function (card) {
    card.addEventListener('pointermove', function (e) {
      var r = card.getBoundingClientRect();
      var px = (e.clientX - r.left) / r.width, py = (e.clientY - r.top) / r.height;
      card.style.setProperty('--ry', ((px - 0.5) * 16).toFixed(2) + 'deg');
      card.style.setProperty('--rx', ((0.5 - py) * 12).toFixed(2) + 'deg');
      card.style.setProperty('--gx', (px * 100).toFixed(1) + '%');
      card.style.setProperty('--gy', (py * 100).toFixed(1) + '%');
    });
    card.addEventListener('pointerleave', function () {
      card.style.setProperty('--rx', '0deg'); card.style.setProperty('--ry', '0deg');
    });
  });
})();

/* ---- Small helpers ---------------------------------------------------------------------- */
function mulberry32(seed) {
  return function () {
    seed |= 0; seed = seed + 0x6D2B79F5 | 0;
    var t = Math.imul(seed ^ seed >>> 15, 1 | seed);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}
function gauss(rng) {
  var u = 1 - rng(), v = rng();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}
// Standard normal CDF (Abramowitz-Stegun 7.1.26), only used to shape the illustration.
function phiCdf(z) {
  var t = 1 / (1 + 0.3275911 * Math.abs(z) / Math.SQRT2);
  var y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-z * z / 2);
  return z >= 0 ? (1 + y) / 2 : (1 - y) / 2;
}
var fmt = function (x, d) { return (x < 0 ? '−' : '') + Math.abs(x).toFixed(d === undefined ? 2 : d); };
var lerp = function (a, b, t) { return a + (b - a) * t; };
var clamp = function (x, a, b) { return Math.min(b, Math.max(a, x)); };

var C = {
  up: new THREE.Color(0x4ade80), down: new THREE.Color(0xff7a85), blue: new THREE.Color(0x4c8dff),
  accent: new THREE.Color(0x5b9bff), violet: new THREE.Color(0xa78bfa), warm: new THREE.Color(0xfcd38a),
  deep: new THREE.Color(0x1f3f9a), white: new THREE.Color(0xffffff)
};
function ramp(t) {  // deep blue -> blue -> violet -> warm
  var c = new THREE.Color();
  if (t < 0.35) return c.copy(C.deep).lerp(C.blue, t / 0.35);
  if (t < 0.7) return c.copy(C.blue).lerp(C.violet, (t - 0.35) / 0.35);
  return c.copy(C.violet).lerp(C.warm, (t - 0.7) / 0.3);
}

/* ---- Morphing instanced boxes --------------------------------------------------------------
   Each instance has a current and a target state [x, y, z, sx, sy, sz, r, g, b]; y is the
   bottom of the box. Instances start moving at their own delay, which gives the waves. */
var HIDDEN = 0.0001;
function Morph(n, material) {
  var geo = new THREE.BoxGeometry(1, 1, 1);
  geo.translate(0, 0.5, 0);
  this.n = n;
  this.mesh = new THREE.InstancedMesh(geo, material, n);
  this.mesh.instanceMatrix.setUsage(THREE.DynamicDrawUsage);
  this.mesh.frustumCulled = false;
  this.cur = new Float32Array(n * 9);
  this.tgt = new Float32Array(n * 9);
  this.start = new Float32Array(n);
  this.m = new THREE.Matrix4();
  this.c = new THREE.Color();
  for (var i = 0; i < n; i++) {
    var o = i * 9;
    this.cur[o + 3] = this.cur[o + 4] = this.cur[o + 5] = HIDDEN;
    this.cur[o + 6] = this.cur[o + 7] = this.cur[o + 8] = 1;
    this.tgt.set(this.cur.subarray(o, o + 9), o);
    this.mesh.setColorAt(i, C.white);
  }
}
Morph.prototype.set = function (i, x, y, z, sx, sy, sz, color, at) {
  var t = this.tgt, o = i * 9;
  t[o] = x; t[o + 1] = y; t[o + 2] = z; t[o + 3] = sx; t[o + 4] = sy; t[o + 5] = sz;
  t[o + 6] = color.r; t[o + 7] = color.g; t[o + 8] = color.b;
  this.start[i] = at;
};
Morph.prototype.hide = function (i, at) {
  var o = i * 9;
  this.tgt[o + 3] = this.tgt[o + 4] = this.tgt[o + 5] = HIDDEN;
  this.start[i] = at;
};
Morph.prototype.visible = function (i) { return this.tgt[i * 9 + 4] > 0.001; };
Morph.prototype.step = function (dt, now, offsetY, hoverId) {
  var k = 1 - Math.exp(-dt * (reduceMotion ? 30 : 5.2));
  var cur = this.cur, tgt = this.tgt, m = this.m, c = this.c;
  for (var i = 0; i < this.n; i++) {
    var o = i * 9;
    if (now >= this.start[i]) {
      for (var j = 0; j < 9; j++) cur[o + j] += (tgt[o + j] - cur[o + j]) * k;
    }
    m.makeScale(cur[o + 3], cur[o + 4], cur[o + 5]);
    m.setPosition(cur[o], cur[o + 1] + (offsetY ? offsetY(cur[o], cur[o + 2]) : 0), cur[o + 2]);
    this.mesh.setMatrixAt(i, m);
    c.setRGB(cur[o + 6], cur[o + 7], cur[o + 8]);
    if (i === hoverId) c.lerp(C.white, 0.55);
    this.mesh.setColorAt(i, c);
  }
  this.mesh.instanceMatrix.needsUpdate = true;
  this.mesh.instanceColor.needsUpdate = true;
  this.mesh.boundingSphere = null;      // instances moved: let the raycaster recompute it
};

/* ---- The scene -------------------------------------------------------------------------------- */
function startScene() {
  var canvas = $('scene');
  var renderer;
  try {
    renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true, alpha: true, powerPreference: 'high-performance' });
  } catch (err) {
    document.body.classList.add('no-webgl');
    $('no-webgl').hidden = false; $('hero-hint').hidden = true;
    return null;
  }
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  renderer.setClearColor(0x000000, 0);

  var scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera(38, 1, 0.1, 400);

  scene.add(new THREE.HemisphereLight(0xc4d8ff, 0x1a2540, 1.25));
  var sun = new THREE.DirectionalLight(0xffdcc0, 1.9); sun.position.set(-9, 15, 11); scene.add(sun);
  var rim = new THREE.DirectionalLight(0x6f9bff, 0.9); rim.position.set(11, 6, -12); scene.add(rim);

  // floor: faint grid + soft glow
  var grid = new THREE.GridHelper(26, 26, 0x5b9bff, 0x5b9bff);
  grid.material.transparent = true; grid.material.opacity = 0.13; grid.material.depthWrite = false;
  scene.add(grid);
  var glowTex = (function () {
    var cv = document.createElement('canvas'); cv.width = cv.height = 128;
    var g = cv.getContext('2d'), rg = g.createRadialGradient(64, 64, 0, 64, 64, 64);
    rg.addColorStop(0, 'rgba(91,155,255,0.55)'); rg.addColorStop(0.5, 'rgba(91,155,255,0.12)'); rg.addColorStop(1, 'rgba(91,155,255,0)');
    g.fillStyle = rg; g.fillRect(0, 0, 128, 128);
    return new THREE.CanvasTexture(cv);
  })();
  var glow = new THREE.Mesh(new THREE.PlaneGeometry(34, 34),
    new THREE.MeshBasicMaterial({ map: glowTex, transparent: true, depthWrite: false, blending: THREE.AdditiveBlending }));
  glow.rotation.x = -Math.PI / 2; glow.position.y = -0.01; scene.add(glow);

  // floating "data dust"
  var dustN = 700, dustPos = new Float32Array(dustN * 3), drng = mulberry32(7);
  for (var d = 0; d < dustN; d++) {
    dustPos[d * 3] = (drng() - 0.5) * 50; dustPos[d * 3 + 1] = drng() * 20; dustPos[d * 3 + 2] = (drng() - 0.5) * 50;
  }
  var dustGeo = new THREE.BufferGeometry(); dustGeo.setAttribute('position', new THREE.BufferAttribute(dustPos, 3));
  var dust = new THREE.Points(dustGeo, new THREE.PointsMaterial({ color: 0xb8cfff, size: 0.09, transparent: true, opacity: 0.55, depthWrite: false }));
  scene.add(dust);

  var mat = new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.36, metalness: 0.12, emissive: 0x0d1838, emissiveIntensity: 0.55 });
  var bars = new Morph(484, mat);
  var wicks = new Morph(40, new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.6, emissive: 0x101a33 }));
  scene.add(bars.mesh); scene.add(wicks.mesh);

  // one line (mean line / regression line) and one pillar (test statistic)
  var line = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1),
    new THREE.MeshStandardMaterial({ color: 0xfcd38a, emissive: 0xfcd38a, emissiveIntensity: 0.55, roughness: 0.3 }));
  var lineState = { x: 0, y: 4, rot: 0, len: 0.001, tx: 0, ty: 4, trot: 0, tlen: 0.001, color: new THREE.Color(0xfcd38a) };
  scene.add(line);
  var pillarGeo = new THREE.BoxGeometry(1, 1, 1); pillarGeo.translate(0, 0.5, 0);
  var pillar = new THREE.Mesh(pillarGeo,
    new THREE.MeshStandardMaterial({ color: 0xfcd38a, emissive: 0xfcd38a, emissiveIntensity: 0.7, transparent: true, opacity: 0.9 }));
  var pillarState = { x: 0, h: 0.001, tx: 0, th: 0.001 };
  scene.add(pillar);

  /* ---- Layouts (one per syllabus scene) ---- */
  var info = [];          // per instance: tooltip text for the current scene
  var params = { seed: 11, sampleSeed: 0, rho: 0.6, alpha: 0.05, z: 1.2, slope: 0.22, noise: 1.1 };
  var ZCRIT = { '0.1': 1.645, '0.05': 1.960, '0.01': 2.576 };

  function hideFrom(m, from, now) { for (var i = from; i < m.n; i++) m.hide(i, now); }
  function setLine(on, y, rot, len, color) {
    lineState.ty = y; lineState.trot = rot; lineState.tlen = on ? len : 0.001;
    if (color) lineState.color.copy(color);
  }

  function layoutCandles(now) {
    var rng = mulberry32(params.seed), n = 40, p = 100, c = [], lo = Infinity, hi = -Infinity;
    for (var i = 0; i < n; i++) {
      var o = p, cl = o * (1 + 0.012 * gauss(rng) + 0.0012);
      var h = Math.max(o, cl) * (1 + Math.abs(gauss(rng)) * 0.004), l = Math.min(o, cl) * (1 - Math.abs(gauss(rng)) * 0.004);
      c.push({ o: o, h: h, l: l, c: cl }); lo = Math.min(lo, l); hi = Math.max(hi, h); p = cl;
    }
    var Y = function (v) { return 1 + (v - lo) / (hi - lo) * 7.5; };
    var mean = c.reduce(function (s, k) { return s + k.c; }, 0) / n;
    for (i = 0; i < n; i++) {
      var k = c[i], x = -10.4 + i * 0.533, up = k.c >= k.o, b = Y(Math.min(k.o, k.c));
      var hgt = Math.max(Y(Math.max(k.o, k.c)) - b, 0.07);
      bars.set(i, x, b, 0, 0.36, hgt, 0.36, up ? C.up : C.down, now + i * 0.03);
      wicks.set(i, x, Y(k.l), 0, 0.06, Y(k.h) - Y(k.l), 0.06, up ? C.up : C.down, now + i * 0.03);
      info[i] = '<b>Candle ' + (i + 1) + (up ? ' · up' : ' · down') + '</b>O ' + k.o.toFixed(2) + ' · H ' + k.h.toFixed(2) +
        '<br>L ' + k.l.toFixed(2) + ' · C ' + k.c.toFixed(2);
    }
    hideFrom(bars, n, now); hideFrom(wicks, n, now);
    setLine(true, Y(mean), 0, 22, C.accent);
    pillarState.th = 0.001;
  }

  function layoutHistogram(now, drop) {
    var n = 360, bins = 24, lo = -3.6, w = 0.3, counts = new Array(bins).fill(0);
    if (!params.sampleSeed) {      // expected counts, largest remainder so they add up to n
      var raw = [], sum = 0;
      for (var b = 0; b < bins; b++) raw.push(n * (phiCdf(lo + (b + 1) * w) - phiCdf(lo + b * w)));
      raw.forEach(function (r, b2) { counts[b2] = Math.floor(r); sum += counts[b2]; });
      raw.map(function (r, b2) { return [r - Math.floor(r), b2]; }).sort(function (a, z) { return z[0] - a[0]; })
        .slice(0, n - sum).forEach(function (p) { counts[p[1]]++; });
    } else {
      var rng = mulberry32(params.sampleSeed);
      for (var s = 0; s < n; s++) counts[clamp(Math.floor((gauss(rng) - lo) / w), 0, bins - 1)]++;
    }
    var idx = 0, pitch = 22 / bins;
    for (b = 0; b < bins; b++) {
      var cx = -11 + (b + 0.5) * pitch, tone = new THREE.Color().copy(C.blue).lerp(C.violet, Math.abs(b - 11.5) / 12);
      for (var k = 0; k < counts[b]; k++, idx++) {
        var layer = Math.floor(k / 4), slot = k % 4;
        var x = cx + ((slot % 2) - 0.5) * 0.42, z = (Math.floor(slot / 2) - 0.5) * 0.42, y = layer * 0.42;
        var at = now + (drop ? layer * 0.05 + Math.random() * 0.25 : idx * 0.002);
        bars.set(idx, x, y, z, 0.36, 0.36, 0.36, tone, at);
        if (drop) {             // let the cubes rain into their bins
          var o = idx * 9;
          bars.cur[o] = x; bars.cur[o + 1] = y + 9 + layer * 0.35; bars.cur[o + 2] = z;
          bars.cur[o + 3] = bars.cur[o + 4] = bars.cur[o + 5] = 0.36;
        }
        info[idx] = '<b>Bin [' + fmt(lo + b * w, 1) + ', ' + fmt(lo + (b + 1) * w, 1) + ')</b>' + counts[b] + ' of ' + n +
          ' observations · relative frequency ' + (counts[b] / n).toFixed(3);
      }
    }
    hideFrom(bars, idx, now); hideFrom(wicks, 0, now);
    setLine(false, 0.2, 0, 0); pillarState.th = 0.001;
  }

  function layoutBivariate(now) {
    var rho = params.rho, q = 1 - rho * rho, norm = 1 / (2 * Math.PI * Math.sqrt(q)), i = 0;
    for (var a = 0; a < 22; a++) {
      for (var b = 0; b < 22; b++, i++) {
        var u = -3 + a * (6 / 21), v = -3 + b * (6 / 21);
        var f = Math.exp(-(u * u - 2 * rho * u * v + v * v) / (2 * q));
        var x = -10.5 + a, z = -10.5 + b, d = Math.hypot(x, z);
        bars.set(i, x, 0, z, 0.82, 0.06 + 8.5 * f, 0.82, ramp(f), now + d * 0.025);
        info[i] = '<b>x = ' + fmt(u) + ', y = ' + fmt(v) + '</b>f(x, y) = ' + (norm * f).toFixed(4) + ' · ρ = ' + fmt(rho);
      }
    }
    hideFrom(wicks, 0, now); setLine(false, 0.2, 0, 0); pillarState.th = 0.001;
  }

  function zToX(z) { return z * (10.4 / 3.6); }
  function layoutZTest(now) {
    var zc = ZCRIT[String(params.alpha)], i = 0;
    for (var s = 0; s < 61; s++) {
      var z = -3.6 + s * 0.12, h = 0.05 + 8 * Math.exp(-z * z / 2), crit = Math.abs(z) > zc;
      for (var r = 0; r < 3; r++, i++) {
        var col = new THREE.Color().copy(crit ? C.down : C.blue).multiplyScalar(r === 1 ? 1 : 0.78);
        bars.set(i, zToX(z), 0, (r - 1) * 0.9, 0.3, h, 0.8, col, now + s * 0.012);
        info[i] = '<b>z = ' + fmt(z) + '</b>φ(z) = ' + (Math.exp(-z * z / 2) / Math.sqrt(2 * Math.PI)).toFixed(4) +
          '<br>' + (crit ? 'critical region: reject H₀' : 'acceptance region');
      }
    }
    hideFrom(bars, i, now); hideFrom(wicks, 0, now);
    setLine(false, 0.2, 0, 0);
    pillarState.tx = zToX(params.z); pillarState.th = 9.6;
  }

  var regRng = mulberry32(5), regPts = [];
  for (var rp = 0; rp < 140; rp++) regPts.push({ x: -10 + regRng() * 20, e: gauss(regRng), z: (regRng() - 0.5) * 3 });
  function layoutRegression(now) {
    var a = 4.6, b = params.slope;
    regPts.forEach(function (p, i) {
      var y = clamp(a + b * p.x + params.noise * p.e, 0.2, 11), res = y - (a + b * p.x);
      var col = new THREE.Color().copy(res >= 0 ? C.up : C.down).lerp(C.white, 0.1);
      bars.set(i, p.x, y - 0.17, p.z, 0.34, 0.34, 0.34, col, now + i * 0.004);
      info[i] = '<b>Point ' + (i + 1) + '</b>x = ' + fmt(p.x) + ', y = ' + fmt(y) + '<br>residual y − ŷ = ' + fmt(res);
    });
    hideFrom(bars, regPts.length, now); hideFrom(wicks, 0, now);
    setLine(true, a, Math.atan(b), 21 / Math.cos(Math.atan(b)), C.warm);
    pillarState.th = 0.001;
  }

  var layouts = [layoutCandles, layoutHistogram, layoutBivariate, layoutZTest, layoutRegression];
  var presets = [{ phi: 0.32, r: 1.0 }, { phi: 0.38, r: 0.95 }, { phi: 0.62, r: 1.12 }, { phi: 0.3, r: 1.0 }, { phi: 0.34, r: 1.02 }];

  /* ---- Camera: orbit with inertia ---- */
  var cam = { theta: 0.55, phi: 0.36, r: 1, tPhi: 0.36, tR: 1, vTheta: 0, vPhi: 0, zoom: 1, base: 30 };
  var target = new THREE.Vector3(0, 3.4, 0), parallax = { x: 0, y: 0, tx: 0, ty: 0 };
  var lastInteraction = -10, scrollK = 0;

  function resize() {
    var w = window.innerWidth, h = window.innerHeight, wide = w >= 900;
    renderer.setSize(w, h, false);
    // Wide screens: the scene sits right of the copy. Narrow: below the title.
    var fw = wide ? w * 1.26 : w, fh = wide ? h : h * 1.3;
    camera.aspect = fw / fh;
    camera.setViewOffset(fw, fh, 0, 0, w, h);
    cam.base = wide ? 31 : (w / h < 0.7 ? 52 : 40);
    camera.updateProjectionMatrix();
  }
  window.addEventListener('resize', resize);
  resize();

  // pointer: drag, pinch, hover, click
  var pointers = new Map(), drag = null, pinch = 0, ndc = new THREE.Vector2(9, 9), hoverDirty = false;
  function toNdc(e) { ndc.set(e.clientX / window.innerWidth * 2 - 1, -(e.clientY / window.innerHeight) * 2 + 1); }
  canvas.addEventListener('pointerdown', function (e) {
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    if (pointers.size === 1) drag = { x: e.clientX, y: e.clientY, moved: 0, type: e.pointerType };
    if (pointers.size === 2) {
      var pts = Array.from(pointers.values()); pinch = Math.hypot(pts[0].x - pts[1].x, pts[0].y - pts[1].y);
    }
    if (e.pointerType === 'mouse') canvas.setPointerCapture(e.pointerId);
    lastInteraction = clock.elapsedTime;
  });
  canvas.addEventListener('pointermove', function (e) {
    toNdc(e); hoverDirty = true;
    parallax.tx = ndc.x; parallax.ty = ndc.y;
    tipPos(e.clientX, e.clientY);
    if (!pointers.has(e.pointerId)) return;
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    if (pointers.size === 2) {
      var pts = Array.from(pointers.values()), dist = Math.hypot(pts[0].x - pts[1].x, pts[0].y - pts[1].y);
      if (pinch) cam.zoom = clamp(cam.zoom * pinch / dist, 0.55, 1.7);
      pinch = dist; return;
    }
    if (!drag) return;
    var dx = e.clientX - drag.x, dy = e.clientY - drag.y;
    drag.x = e.clientX; drag.y = e.clientY; drag.moved += Math.abs(dx) + Math.abs(dy);
    // touch: only horizontal drags orbit, vertical ones scroll the page (touch-action: pan-y)
    cam.vTheta = -dx * 0.0055;
    if (drag.type === 'mouse') cam.vPhi = dy * 0.004;
    if (drag.moved > 4) canvas.classList.add('is-dragging');
    lastInteraction = clock.elapsedTime;
  });
  function endPointer(e) {
    var wasClick = drag && drag.moved < 5 && pointers.size === 1;
    pointers.delete(e.pointerId);
    if (pointers.size < 2) pinch = 0;
    if (pointers.size === 0) {
      if (wasClick && e.type === 'pointerup') ripple(e);
      drag = null; canvas.classList.remove('is-dragging');
    }
  }
  canvas.addEventListener('pointerup', endPointer);
  canvas.addEventListener('pointercancel', endPointer);
  canvas.addEventListener('pointerleave', function () { ndc.set(9, 9); hoverDirty = true; parallax.tx = parallax.ty = 0; });
  canvas.addEventListener('wheel', function (e) {     // plain wheel scrolls the page; Ctrl+wheel or a trackpad pinch zooms
    if (!e.ctrlKey) return;
    e.preventDefault();
    cam.zoom = clamp(cam.zoom * (1 + e.deltaY * 0.004), 0.55, 1.7);
    lastInteraction = clock.elapsedTime;
  }, { passive: false });
  $('zoom-in').addEventListener('click', function () { cam.zoom = clamp(cam.zoom / 1.18, 0.55, 1.7); });
  $('zoom-out').addEventListener('click', function () { cam.zoom = clamp(cam.zoom * 1.18, 0.55, 1.7); });
  $('view-reset').addEventListener('click', function () { cam.zoom = 1; cam.theta = 0.55; cam.vTheta = cam.vPhi = 0; cam.tPhi = presets[mode].phi; });

  // ripples
  var ripples = [], raycaster = new THREE.Raycaster(), floor = new THREE.Plane(new THREE.Vector3(0, 1, 0), 0), hitPt = new THREE.Vector3();
  function ripple(e) {
    toNdc(e); raycaster.setFromCamera(ndc, camera);
    var hit = raycaster.intersectObject(bars.mesh)[0];
    if (hit) hitPt.copy(hit.point); else if (!raycaster.ray.intersectPlane(floor, hitPt)) return;
    ripples.push({ x: hitPt.x, z: hitPt.z, t0: clock.elapsedTime });
    if (ripples.length > 4) ripples.shift();
  }
  function rippleOffset(x, z) {
    var off = 0, now = clock.elapsedTime;
    for (var i = 0; i < ripples.length; i++) {
      var r = ripples[i], age = now - r.t0, dist = Math.hypot(x - r.x, z - r.z), front = dist - age * 9;
      off += 1.3 * Math.exp(-age * 1.1) * Math.exp(-front * front / 3);
    }
    return off;
  }

  // tooltip
  var tip = $('scene-tip'), hoverId = -1, tipXY = [0, 0];
  function tipPos(x, y) { tipXY = [x, y]; }
  function updateHover() {
    if (!hoverDirty) return;
    hoverDirty = false;
    var id = -1;
    if (Math.abs(ndc.x) <= 1 && !drag) {
      raycaster.setFromCamera(ndc, camera);
      var hits = raycaster.intersectObject(bars.mesh);
      for (var i = 0; i < hits.length; i++) if (bars.visible(hits[i].instanceId)) { id = hits[i].instanceId; break; }
    }
    hoverId = id;
    if (id < 0 || !info[id]) { tip.hidden = true; return; }
    tip.innerHTML = info[id]; tip.hidden = false;
  }

  /* ---- Mode switching, captions and controls ---- */
  var mode = 0, touring = !reduceMotion, tourMs = 8000, modeStarted = 0;
  var dock = $('mode-dock'), chips = Array.from(document.querySelectorAll('.mode-chip'));
  var captions = [
    { unit: 'Unit · Reading the chart', title: 'Read the graph',
      text: 'R finds every candle in your screenshot by colour and turns pixel rows into prices using the labels on the price axis. The yellow line is the mean.',
      formula: 'price = p<sub>top</sub> − (y − y<sub>top</sub>) · (p<sub>top</sub> − p<sub>bottom</sub>) / (y<sub>bottom</sub> − y<sub>top</sub>)' },
    { unit: 'Unit · Descriptive statistics & moments', title: 'Every cube is one observation',
      text: 'Stack the returns into bins and the histogram appears. From it R reports the mean, variance, moments, skewness and kurtosis.',
      formula: 'x̄ = Σx / n &nbsp;·&nbsp; s² = Σ(x − x̄)² / (n − 1) &nbsp;·&nbsp; β<sub>2</sub> = μ<sub>4</sub> / μ<sub>2</sub>²' },
    { unit: 'Unit · Bivariate distributions', title: 'Joint distribution of two returns',
      text: 'A bivariate normal density for today’s and tomorrow’s return. Change the correlation ρ and watch the hill stretch and turn.',
      formula: 'f(x, y) = e<sup>−Q/2</sup> / (2π√(1 − ρ²)), &nbsp; Q = (x² − 2ρxy + y²) / (1 − ρ²)' },
    { unit: 'Unit · Testing of hypothesis', title: 'Two-tailed Z test',
      text: 'Red slices are the critical region for the chosen α; the glowing pillar is the calculated z. Move it and read the decision.',
      formula: 'z = (x̄ − μ<sub>0</sub>) / (σ / √n) &nbsp;·&nbsp; reject H<sub>0</sub> when |z| > z<sub>α/2</sub>' },
    { unit: 'Unit · Correlation & regression', title: 'Least squares through a cloud',
      text: 'Points above the regression line are green, points below are red: the residuals. Change the slope and the noise.',
      formula: 'ŷ = a + b·x, &nbsp; b = S<sub>xy</sub> / S<sub>xx</sub>, &nbsp; a = ȳ − b·x̄' }
  ];

  function rangeControl(label, min, max, step, value, show, onInput) {
    var el = document.createElement('label');
    el.innerHTML = '<span>' + label + '</span><output></output><input type="range">';
    var input = el.querySelector('input'), out = el.querySelector('output');
    input.min = min; input.max = max; input.step = step; input.value = value; out.textContent = show(value);
    input.addEventListener('input', function () { var v = Number(input.value); out.textContent = show(v); stopTour(); onInput(v); });
    return el;
  }
  function buttonControl(text, icon, onClick) {
    var b = document.createElement('button');
    b.type = 'button'; b.className = 'btn btn-small btn-secondary';
    b.innerHTML = '<svg class="icon icon-sm" aria-hidden="true"><use href="assets/icons.svg?v=3#' + icon + '"/></svg>' + text;
    b.addEventListener('click', function () { stopTour(); onClick(); });
    return b;
  }
  function decisionText() {
    var zc = ZCRIT[String(params.alpha)], rej = Math.abs(params.z) > zc;
    return { cls: rej ? 'reject' : 'accept',
      text: '|z| = ' + Math.abs(params.z).toFixed(2) + (rej ? ' > ' : ' ≤ ') + zc.toFixed(3) + ' → ' + (rej ? 'reject H₀' : 'H₀ is not rejected') };
  }
  function renderControls(m) {
    var box = $('mode-controls'); box.innerHTML = '';
    var now = clock.elapsedTime;
    if (m === 0) box.appendChild(buttonControl('New chart', 'i-refresh-cw', function () { params.seed = (params.seed * 7 + 13) % 9973; layoutCandles(clock.elapsedTime); }));
    if (m === 1) box.appendChild(buttonControl('Draw a new sample', 'i-refresh-cw', function () { params.sampleSeed = 1 + Math.floor(Math.random() * 1e6); layoutHistogram(clock.elapsedTime, true); }));
    if (m === 2) box.appendChild(rangeControl('Correlation ρ', -0.9, 0.9, 0.05, params.rho, function (v) { return fmt(v); },
      function (v) { params.rho = v; layoutBivariate(clock.elapsedTime - 1); }));
    if (m === 3) {
      var seg = document.createElement('div'); seg.className = 'mode-seg'; seg.setAttribute('role', 'group'); seg.setAttribute('aria-label', 'Significance level');
      [0.1, 0.05, 0.01].forEach(function (a) {
        var b = document.createElement('button'); b.type = 'button'; b.textContent = 'α = ' + a;
        b.setAttribute('aria-pressed', String(a === params.alpha));
        b.addEventListener('click', function () {
          params.alpha = a; stopTour();
          seg.querySelectorAll('button').forEach(function (x) { x.setAttribute('aria-pressed', String(x === b)); });
          layoutZTest(clock.elapsedTime); showDecision();
        });
        seg.appendChild(b);
      });
      box.appendChild(seg);
      box.appendChild(rangeControl('Calculated z', -4, 4, 0.05, params.z, function (v) { return fmt(v); },
        function (v) { params.z = v; pillarState.tx = zToX(v); showDecision(); }));
      var dec = document.createElement('p'); dec.className = 'mode-decision'; dec.id = 'mode-decision';
      box.appendChild(dec); showDecision();
    }
    if (m === 4) {
      box.appendChild(rangeControl('Slope b', -0.45, 0.45, 0.01, params.slope, function (v) { return fmt(v); },
        function (v) { params.slope = v; layoutRegression(clock.elapsedTime - 1); }));
      box.appendChild(rangeControl('Noise σ', 0.1, 3, 0.05, params.noise, function (v) { return v.toFixed(2); },
        function (v) { params.noise = v; layoutRegression(clock.elapsedTime - 1); }));
    }
    return now;
  }
  function showDecision() {
    var el = $('mode-decision'); if (!el) return;
    var d = decisionText(); el.className = 'mode-decision ' + d.cls; el.textContent = d.text;
  }

  function setMode(m) {
    mode = (m + 5) % 5; modeStarted = clock.elapsedTime;
    chips.forEach(function (c, i) {
      c.setAttribute('aria-pressed', String(i === mode));
      var bar = c.querySelector('i'); bar.style.animation = 'none'; void bar.offsetWidth; bar.style.animation = '';
    });
    var cap = captions[mode];
    $('mode-unit').textContent = cap.unit; $('mode-title').textContent = cap.title;
    $('mode-text').textContent = cap.text; $('mode-formula').innerHTML = cap.formula;
    renderControls(mode);
    layouts[mode](clock.elapsedTime, mode === 1);
    cam.tPhi = presets[mode].phi; cam.tR = presets[mode].r;
    hoverDirty = true;
  }
  function stopTour() { if (touring) setTour(false); }
  function setTour(on) {
    touring = on; modeStarted = clock.elapsedTime;
    dock.classList.toggle('touring', on);
    var btn = $('tour-toggle'); btn.setAttribute('aria-pressed', String(on));
    btn.querySelector('span').textContent = on ? 'Pause tour' : 'Play tour';
    if (on) setMode(mode);
  }
  dock.style.setProperty('--tour-ms', tourMs + 'ms');
  chips.forEach(function (c, i) { c.addEventListener('click', function () { stopTour(); setMode(i); }); });
  $('tour-toggle').addEventListener('click', function () { setTour(!touring); });
  document.addEventListener('keydown', function (e) {
    if (e.target.closest && e.target.closest('input, textarea, select')) return;
    if (e.key >= '1' && e.key <= '5') { stopTour(); setMode(Number(e.key) - 1); }
    else if (e.key === 'ArrowRight' && window.scrollY < window.innerHeight * 0.5) { stopTour(); setMode(mode + 1); }
    else if (e.key === 'ArrowLeft' && window.scrollY < window.innerHeight * 0.5) { stopTour(); setMode(mode - 1); }
  });

  // scrolling away from the hero dims the scene and pulls the camera back
  function onScroll() {
    scrollK = clamp(window.scrollY / window.innerHeight, 0, 1.5);
    canvas.style.opacity = String(1 - Math.min(scrollK, 1) * 0.7);
    canvas.style.filter = scrollK > 0.05 ? 'blur(' + (Math.min(scrollK, 1) * 3).toFixed(1) + 'px)' : '';
  }
  window.addEventListener('scroll', onScroll, { passive: true });
  onScroll();

  /* ---- Render loop ---- */
  var clock = new THREE.Clock();
  setTour(touring);
  if (!touring) setMode(0);

  function frame() {
    var dt = Math.min(clock.getDelta(), 0.05), now = clock.elapsedTime;

    if (touring && now - modeStarted > tourMs / 1000 && scrollK < 0.5 && !document.hidden) setMode(mode + 1);

    // camera
    var idle = now - lastInteraction > 2.5 && !drag;
    if (idle && !reduceMotion) cam.vTheta = lerp(cam.vTheta, 0.0011, 0.02);
    cam.theta += cam.vTheta; cam.phi += cam.vPhi;
    if (!drag) { cam.vTheta *= 0.94; cam.vPhi *= 0.9; }
    if (idle) cam.phi = lerp(cam.phi, cam.tPhi, 0.02);
    cam.phi = clamp(cam.phi, 0.06, 1.3);
    cam.r = lerp(cam.r, cam.tR, 0.04);
    parallax.x = lerp(parallax.x, parallax.tx, 0.05); parallax.y = lerp(parallax.y, parallax.ty, 0.05);
    var rad = cam.base * cam.r * cam.zoom * (1 + scrollK * 0.35);
    camera.position.set(rad * Math.cos(cam.phi) * Math.sin(cam.theta), target.y + rad * Math.sin(cam.phi),
      rad * Math.cos(cam.phi) * Math.cos(cam.theta));
    camera.lookAt(target.x - parallax.x * 0.8, target.y - parallax.y * 0.5, target.z);

    // objects
    ripples = ripples.filter(function (r) { return now - r.t0 < 3.5; });
    var off = ripples.length ? rippleOffset : null;
    bars.step(dt, now, off, hoverId);
    wicks.step(dt, now, off, -1);

    var k = 1 - Math.exp(-dt * (reduceMotion ? 30 : 4.5));
    lineState.y = lerp(lineState.y, lineState.ty, k); lineState.rot = lerp(lineState.rot, lineState.trot, k);
    lineState.len = lerp(lineState.len, lineState.tlen, k);
    line.position.set(0, lineState.y, 0); line.rotation.z = lineState.rot;
    line.scale.set(Math.max(lineState.len, 0.001), lineState.len > 0.05 ? 0.07 : 0.001, lineState.len > 0.05 ? 0.07 : 0.001);
    line.material.color.lerp(lineState.color, k); line.material.emissive.lerp(lineState.color, k);
    pillarState.x = lerp(pillarState.x, pillarState.tx, k * 1.6); pillarState.h = lerp(pillarState.h, pillarState.th, k);
    pillar.position.set(pillarState.x, 0, 0);
    pillar.scale.set(pillarState.h > 0.05 ? 0.16 : 0.001, Math.max(pillarState.h, 0.001), pillarState.h > 0.05 ? 3.2 : 0.001);

    if (!reduceMotion) { dust.rotation.y = now * 0.012; dust.position.y = Math.sin(now * 0.3) * 0.3; }

    updateHover();
    if (!tip.hidden) tip.style.transform = 'translate(' + Math.min(tipXY[0] + 16, window.innerWidth - tip.offsetWidth - 8) + 'px,' +
      Math.min(tipXY[1] + 16, window.innerHeight - tip.offsetHeight - 8) + 'px)';

    renderer.render(scene, camera);
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);
  return { setMode: setMode };
}

window.chartReaderSplash = startScene();
