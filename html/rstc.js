"use strict";

function rad2deg(rad) {
  return rad / Math.PI * 180;
}

function hasClass(element, name) {
  if (element.nodeType != 1)
    return false;

  var regexp = new RegExp("(^| )" + name + "\W*");

  if (typeof element.className != "undefined")
    return regexp.test(element.className);

  return regexp.test(element.getAttribute("class"));
}

function isArray(input) {
  return typeof(input) == "object" && (input instanceof Array);
}

function removeChildren(ids) {
  if (!isArray(ids))
    ids = new Array(ids);
  for (var i = 0; i < ids.length; ++i) {
    var e = document.getElementById(ids[i]);
    for (var n = e.firstChild; n; ) {
      if (!hasClass(n, "unremovable")) {
        e.removeChild(n);
        n = e.firstChild;
      } else {
        n = n.nextSibling;
      }
    }
  }
}

/* A timer that synchronously executes a number of handlers.
 * Similar to setInterval() except that all handlers are executed
 * right after another and thus are kept in sync over a long period. */
function Timer() {
  this.events = new Array(); /* { ticks: int, func: func() { ... } } */
  this.now = 0;
  this.doPause = false;

  var PERIOD = 10;
  var timer = this;
  function loop() {
    if (timer.doPause) {
      return;
    }
    for (var i = timer.events.length - 1; i >= 0; --i) {
      if (timer.events[i] != undefined &&
          timer.now >= timer.events[i].ticks) {
        timer.events[i].func();
        timer.events.splice(i, 1);
      }
    }
    timer.now += PERIOD;
  }

  this.interval = setInterval(loop, PERIOD);

  this.secs = function () { return this.now / 1000; }
  this.play = function () { this.doPause = false; }
  this.pause = function () { this.doPause = true; }
  this.stop = function () { clearInterval(this.interval); }
  this.addEvent = function (func, secs) {
    var ticks = secs * 1000;
    this.events.push({ ticks: this.now + ticks, func: func });
  }
}

var SCALE = 0.065;

function Rstc(measurements, start) {
  var cars = {};
  var ntg = {};
  var ttc = {};

  for (var b in measurements) {
    cars[b] = true;
    for (var c in measurements[b]) {
      cars[c] = true;
    }
  }

  for (var b in cars) {
    ntg[b] = {};
    ttc[b] = {};
  }

  for (var b in cars) {
    for (var c in cars) {
      if (measurements[b] != undefined && measurements[b][c] != undefined) {
        if (measurements[b][c].ntg != undefined) {
          ntg[b][c] = measurements[b][c].ntg;
        }
        if (measurements[b][c].ttc != undefined) {
          ttc[b][c] = measurements[b][c].ttc;
        }
      }
    }
  }

  var change = false;
  do {
    change = false;
    for (var b in cars) {
      for (var c in cars) {
        if (b != c && ntg[b][c] != undefined && ttc[b][c] != undefined && ntg[c][b] == undefined) {
          ntg[c][b] = -1 / (1 - ntg[b][c] / ttc[b][c]) * ntg[b][c];
          change = true;
        }
        if (b != c && ntg[b][c] != undefined && ttc[b][c] != undefined && ttc[c][b] == undefined) {
          ttc[c][b] = ttc[b][c];
          change = true;
        }
      }
    }

    for (var b in cars) {
      for (var c in cars) {
        for (var d in cars) {
          if (b != c && c != d && b != d &&
              ntg[b][c] != undefined && ttc[b][c] != undefined &&
              ntg[c][d] != undefined && ttc[c][d] != undefined &&
              ntg[b][d] == undefined) {
            ntg[b][d] = ntg[b][c] + (1 - ntg[b][c] / ttc[b][c]) * ntg[c][d];
            change = true;
          }
          if (b != c && c != d && b != d &&
              ntg[b][c] != undefined && ttc[b][c] != undefined &&
              ntg[c][d] != undefined && ttc[c][d] != undefined &&
              ttc[b][d] == undefined) {
            var l1 = (ttc[c][d] * ntg[b][c]) / (ntg[c][d] * ttc[b][c] + ttc[c][d] * ntg[b][c] - ntg[c][d] * ntg[b][c]);
            var l2 = (ttc[b][c] * ntg[c][d] - ntg[b][c] * ntg[c][d]) / (ntg[b][c] * ttc[c][d] + ttc[b][c] * ntg[c][d] - ntg[b][c] * ntg[c][d]);
            ttc[b][d] = l1 * ttc[b][c] + l2 * ttc[c][d];
            change = true;
          }
        }
      }
    }
  } while (change);

  this.cars = cars;
  this.start = function () { return (start) ? start : 0; };
  this.ntg = function (b, c) { return ntg[b][c]; };
  this.ttc = function (b, c) { /*alert("t = 0 ==> "+ (ttc[b][c]));*/ return ttc[b][c]; };
};

// The x position of b if c is the reference car.
// If c is undefined, the reference car is determined automatically.
Rstc.prototype.x = function (b, c) {
  if (c == undefined) {
    c = this.computeReferenceCar();
  }
  if (b == c) {
    return 0;
  }
  var ntg = this.ntg;
  var ttc = this.ttc;
  var ret = -1 * ntg(b,c) * (1 - ntg(c,b) / ttc(c,b));
  return isNaN(ret) ? 0 : ret;
};

Rstc.prototype.v = function (b) {
  var c = this.computeReferenceCar();
  if (b == c) {
    return 1;
  }
  var ntg = this.ntg;
  var ttc = this.ttc;
  var ret = 1 - ntg(c,b) / ttc(c,b);
  return isNaN(ret) ? 0 : ret;
};

// Reference car for visualization is:
// arg min_c max_b |x(b,c)|
Rstc.prototype.computeReferenceCar = function () {
  if (this.referenceCar != undefined) {
    return this.referenceCar;
  }
  var min = undefined;
  for (var c in this.cars) {
    var max = undefined;
    for (var b in this.cars) {
      var tmp = Math.abs(this.x(b, c));
      if (max == undefined || tmp > max) {
        max = tmp;
      }
    }
    if (min == undefined || max < min) {
      min = max;
      this.referenceCar = c;
    }
  }
  return this.referenceCar;
};

Rstc.prototype.forceReferenceCar = function (b) {
  this.referenceCar = b;
};

Rstc.prototype.progress = function () {
  function EmptyRstc() {};
  EmptyRstc.prototype = Rstc.prototype;
  var rstc = new EmptyRstc();
  var start = this.start();
  var ntg = {};
  var ttc = {};
  for (var b in this.cars) {
    ntg[b] = {};
    ttc[b] = {};
    for (var c in this.cars) {
      ntg[b][c] = this.ntg(b,c);
      ttc[b][c] = this.ttc(b,c);
    }
  }
  rstc.cars = this.cars;
  rstc.start = function () { return start; };
  rstc.ntg = function (b, c) { return ntg[b][c]; };
  rstc.ttc = function (b, c) { return ttc[b][c]; };
  rstc.referenceCar = this.computeReferenceCar();
  return rstc;
}

Rstc.prototype.wait = function (t) {
  var start = this.start;
  var ntg = this.ntg;
  var ttc = this.ttc;
  function WaitRstc() {};
  WaitRstc.prototype = Rstc.prototype;
  var rstc = new WaitRstc();
  rstc.t = t;
  rstc.time = function () { return this.t; };
  rstc.start = function () { return start() + rstc.time(); };
  rstc.cars = this.cars;
  rstc.ntg = function (b, c) { return ntg(b,c) - rstc.time() * ntg(b,c) / ttc(b,c); };
  rstc.ttc = function (b, c) { return ttc(b,c) - rstc.time(); };
  rstc.wait = function (t) { this.t += t; return rstc; };
  rstc.referenceCar = this.computeReferenceCar();
  return rstc;
}

Rstc.prototype.accel = function (bb, q) {
  var start = this.start;
  var ntg = this.ntg;
  var ttc = this.ttc;
  function AccelRstc() {};
  AccelRstc.prototype = Rstc.prototype;
  var rstc = new AccelRstc();
  rstc.cars = this.cars;
  rstc.start = function () { return start(); };
  rstc.ntg = function (b, c) {
    if (b == bb) {
      return 1/q * ntg(b,c);
    } else {
      return ntg(b,c);
    }
  };
  rstc.ttc = function (b, c) {
    if (b == bb) {
      return 1 / ((q - 1) * ttc(b,c) / ntg(b,c) + 1) * ttc(b,c);
    } else if (c == bb) {
      return 1 / ((1 - q) * ttc(b,c) / ntg(b,c) + q) * ttc(b,c);
    } else {
      return ttc(b,c);
    }
  };
  rstc.referenceCar = this.computeReferenceCar();
  return rstc;
}

function ChangingRstc(rstc) {
  this.rstc = rstc;
  this.cars = this.rstc.cars;
  this.computeReferenceCar = function () { this.referenceCar = this.rstc.computeReferenceCar(); return this.referenceCar; };
  this.forceReferenceCar = function (b) { this.rstc.forceReferenceCar(b); };
  this.progress = function () { this.rstc = this.rstc.progress(); return this; };
  this.wait = function (t) { this.rstc = this.rstc.wait(t); return this; };
  this.accel = function (b, q) { this.rstc = this.rstc.accel(b, q); return this; };
  this.ntg = function (b, c) { return this.rstc.ntg(b, c); };
  this.ttc = function (b, c) { return this.rstc.ttc(b, c); };
  this.x = function (b, c) { return this.rstc.x(b, c); };
  this.v = function (b) { return this.rstc.v(b); };
  this.start = function () { return this.rstc.start(); };
  this.time = function () { return this.rstc.time(); };
}

function InterpolatingRstc(obs) {
  var now = obs[0].time;
  this.cars = obs[0].rstc.cars;
  this.computeReferenceCar = function () {
    var ref = obs[0].rstc.computeReferenceCar();
    for (var i = 1; i < obs.length; ++i) {
      obs[i].rstc.forceReferenceCar(ref);
    }
    return ref;
  };
  this.forceReferenceCar = function (b) {
    for (var i = 0; i < obs.length; ++i) {
      obs[i].rstc.forceReferenceCar(b);
    }
  };
  this.wait = function (t) { now += t; };
  /*this.accel = function (b, q) { ; };*/
  this.ntg = function (b, c) {
    for (var i = 0; i < obs.length; ++i) {
      if (now >= obs[i].time) {
        if (i+1 < obs.length && now < obs[i+1].time) {
          var r0 = obs[i].rstc;
          var r1 = obs[i+1].rstc;
          var len = obs[i+1].time - obs[i].time;
          var rel = now - obs[i].time;
          return r0.ntg(b, c) + rel / len * (r1.ntg(b, c) - r0.ntg(b, c));
        } else if (i+1 == obs.length) {
          return obs[i].rstc.wait(now - obs[i].time).ntg(b, c);
        }
      }
    }
  };
  this.ttc = function (b, c) {
    for (var i = 0; i < obs.length; ++i) {
      if (now >= obs[i].time) {
        if (i+1 < obs.length && now < obs[i+1].time) {
          var r0 = obs[i].rstc;
          var r1 = obs[i+1].rstc;
          var len = obs[i+1].time - obs[i].time;
          var rel = now - obs[i].time;
          return r0.ttc(b, c) + rel / len * (r1.ttc(b, c) - r0.ttc(b, c));
        } else if (i+1 == obs.length) {
          return obs[i].rstc.wait(now - obs[i].time).ttc(b, c);
        }
      }
    }
  };
  this.v = Rstc.prototype.v;
  this.x = Rstc.prototype.x;
  this.time = function () { return now; };
}

var DEFAULT_FPS = 50;

/* Street object. Utility object for my animations. */
function Street(streetId, timer, lanes, extra) {
  if (extra == undefined) {
    extra = { };
  }
  if (extra.offset == undefined) {
    extra.offset = 0;
  }
  if (extra.offsetTop == undefined) {
    extra.offsetTop = extra.offset;
  }
  if (extra.offsetBottom == undefined) {
    extra.offsetBottom = extra.offset;
  }
  if (extra.lines == undefined) {
    extra.lines = 0;
  }
  if (extra.linesTop == undefined) {
    extra.linesTop = extra.lines;
  }
  if (extra.linesBottom == undefined) {
    extra.linesBottom = extra.lines;
  }
  if (extra.textHeight == undefined) {
    extra.textHeight = bodyFontSizeInPixel();
  }
  if (extra.fps == undefined) {
    extra.fps = DEFAULT_FPS;
  }
  if (extra.relWidth == undefined) {
    extra.relWidth = 1.0;
  }
  if (extra.relHeight == undefined) {
    extra.relHeight = 1.0;
  }

  var WIDTH_TO_HEIGHT = 14 / lanes / extra.relHeight;
  var LINE_SPACING = 0.2;
  var elem = document.getElementById(streetId);
  var canvas = document.createElement("canvas");
  canvas.setAttribute("width", "100%");
  elem.appendChild(canvas);
  var ctx = this.ctx = canvas.getContext("2d");
  var streetWidth = extra.relWidth * window.innerWidth * 0.95;
  var streetHeight = Math.min(window.innerHeight, streetWidth / WIDTH_TO_HEIGHT);

  extra.offsetTop = sizeToPixel(extra.offsetTop, streetHeight);
  extra.offsetBottom = sizeToPixel(extra.offsetBottom, streetHeight);
  extra.textHeight = sizeToPixel(extra.textHeight, bodyFontSizeInPixel());

  var pavementTopHeight = extra.offsetTop + extra.linesTop * extra.textHeight * (1 + LINE_SPACING);
  var pavementBottomHeight = extra.offsetBottom + extra.linesBottom * extra.textHeight * (1 + LINE_SPACING);
  ctx.canvas.width = streetWidth;
  ctx.canvas.height = streetHeight + pavementTopHeight + pavementBottomHeight;

  var markHeight = streetHeight / (10 * lanes);
  var markWidth = streetHeight / lanes / 1.5;

  this.timer = function () { return timer; };
  this.streetWidth = function () { return streetWidth; };
  this.streetHeight = function () { return streetHeight; };
  this.offsetTop = function () { return extra.offsetTop; };
  this.offsetBottom = function () { return extra.offsetBottom; };
  this.pavementTopHeight = function () { return pavementTopHeight; };
  this.pavementBottomHeight = function () { return pavementBottomHeight; };
  this.laneHeight = function () { return (streetHeight - markHeight) / lanes; };
  this.textHeight = function () { return extra.textHeight; };

  var markRelOffset = 0;
  this.scroll = function (rel) { markRelOffset = rel; };

  this.x = function (xRel) {
    return xRel * streetWidth;
  };
  this.y = function (yRel) {
    return pavementTopHeight + (-yRel + Math.floor(lanes / 2)) * streetHeight / lanes + streetHeight / lanes / 2;
  };
  this.textY = function (top, line) {
    var y0 = pavementTopHeight + (top ? -1 * extra.offsetTop : streetHeight + extra.offsetBottom);
    var y1 = (top ? -1 : 1) * (line + (top ? 0 : 1)) * extra.textHeight * (1 + LINE_SPACING);
    var y2 = -1 * LINE_SPACING * extra.textHeight;
    return y0 + y1 + y2;
  };

  var redrawHooks = {};
  var redrawHookId = 0;
  this.addRedrawHook = function (hook) { redrawHooks[++redrawHookId] = hook; return redrawHookId; };
  this.removeRedrawHook = function (id) { delete redrawHooks[id]; };

  function redraw() {
    ctx.fillStyle = "white";
    ctx.fillRect(0, 0, streetWidth, pavementTopHeight);
    ctx.fillRect(0, pavementTopHeight+streetHeight, streetWidth, pavementBottomHeight);

    ctx.fillStyle = "#999999";
    ctx.fillRect(0, pavementTopHeight, streetWidth, streetHeight);

    var markOffset = markWidth / 2 + markRelOffset * streetWidth;
    if (Math.abs(markOffset) > 2 * markWidth) {
      markOffset = markOffset % (2 * markWidth);
    }
    for (var i = 0; i < lanes - 1; ++i) {
      var markYPos = pavementTopHeight + (i + 1) * streetHeight / lanes - markHeight / 2;
      for (var x = markOffset; x < streetWidth; x += 2 * markWidth) {
        ctx.fillStyle = "white";
        ctx.fillRect(x, markYPos, markWidth, markHeight);
      }
    }

    for (var id in redrawHooks) {
      redrawHooks[id](timer.secs());
    }

    timer.addEvent(redraw, 1 / extra.fps);
  }

  redraw();
}


/* A image moving along the street. 
 * For cars, the height and width params can be left undefined. */
function Elem(id, street, xf, yf, extra) {
  if (extra == undefined) {
    extra = {};
  }
  if (extra.img == undefined) {
    extra.img = "car-"+ carToColor(id);
  }
  if (extra.radf == undefined) {
    extra.radf = function () { return 0; };
  }
  if (extra.height == undefined) {
    extra.height = street.laneHeight() * 0.75;
  }
  if (extra.width == undefined) {
    extra.width = extra.height * 2;
  }

  var elem = document.getElementById(extra.img);
  this.id = id;
  this.xf = xf;
  this.yf = yf;
  this.radf = extra.radf;
  this.width = extra.width;
  this.height = extra.height;

  var self = this;
  var i = street.addRedrawHook(function (t) {
    street.ctx.save();
    street.ctx.translate(street.x(self.xf(t)), street.y(self.yf(t)));
    street.ctx.rotate(self.radf(t));
    street.ctx.drawImage(elem, -self.width/2, -self.height/2, self.width, self.height);
    street.ctx.restore();
  });

  this.kill = function () { street.removeRedrawHook(i); };

  if (extra.timeout != undefined) {
    street.timer().addEvent(function () { street.removeRedrawHook(i); }, extra.timeout);
  }
}

var LINE_WIDTH = 5;
var ARROW_WIDTH = 3.5;

function RstcVis(rstc, carMap, street, ms, extra) {
  if (extra == undefined) {
    extra = {};
  }
  if (extra.color == undefined) {
    extra.color = "#000000";
  }
  if (extra.line == undefined) {
    extra.line = 0;
  }
  if (extra.arrowWidth == undefined) {
    extra.arrowWidth = 10;
  }
  if (extra.arrowStumpOffset == undefined) {
    extra.arrowStumpOffset = 0;
  }
  if (extra.arrowHeadOffset == undefined) {
    extra.arrowHeadOffset = 0;
  }
  if (extra.filledArrow == undefined) {
    extra.filledArrow = true;
  }
  if (extra.shadow == undefined) {
    extra.shadow = true;
  }
  this.i = undefined;
  function drawEdge(t, b, c, top) {
    if (top == undefined) {
      top = true;
    }
    var offsetY = (top ? -1 : 1) * 5;
    var x1 = street.x(b.xf(t)) + extra.arrowStumpOffset;
    var y1 = street.y(b.yf(t)) + offsetY;
    var x2 = street.x(c.xf(t)) + extra.arrowHeadOffset;
    var y2 = street.y(c.yf(t)) + offsetY;
    var ctrl = top
      ? street.pavementTopHeight() - street.offsetTop()
      : street.pavementTopHeight() + street.streetHeight() + street.offsetBottom();
    street.ctx.save();
    street.ctx.strokeStyle = extra.color;
    street.ctx.lineWidth = LINE_WIDTH;
    street.ctx.beginPath();
    street.ctx.moveTo(x1, y1);
    street.ctx.bezierCurveTo(x1, ctrl, x2, ctrl, x2, y2);
    street.ctx.stroke();
    if (!extra.filledArrow) {
      street.ctx.lineWidth = ARROW_WIDTH;
      street.ctx.strokeStyle = extra.color;
      street.ctx.moveTo(x2, y2);
      street.ctx.lineTo(x2 - extra.arrowWidth, y2 + (top ? -1 : 1) * extra.arrowWidth);
      street.ctx.moveTo(x2, y2);
      street.ctx.lineTo(x2 + extra.arrowWidth, y2 + (top ? -1 : 1) * extra.arrowWidth);
      street.ctx.stroke();
    } else {
      street.ctx.beginPath();
      street.ctx.fillStyle = extra.color;
      street.ctx.moveTo(x2, y2 + (top ? 3 : -3));
      street.ctx.lineTo(x2 - extra.arrowWidth, y2 + (top ? -1 : 1) * extra.arrowWidth);
      street.ctx.lineTo(x2 + extra.arrowWidth, y2 + (top ? -1 : 1) * extra.arrowWidth);
      street.ctx.closePath();
      street.ctx.fill();
    }
    if (street.textHeight() > 0) {
      var ntgStr = "NTG = "+ rstc.ntg(b.id, c.id).toFixed(1) +" s";
      var ttcStr = "TTC = "+ rstc.ttc(b.id, c.id).toFixed(1) +" s";
      street.ctx.font = "bold "+ street.textHeight() +"px sans-serif";
      var str = ntgStr +", "+ ttcStr;
      var width = street.ctx.measureText(str).width;
      var textX = x1 + (x2-x1) / 2 - width / 2;
      var textY = street.textY(top, extra.line);
      //street.ctx.fillStyle = "#333333";
      //street.ctx.fillText(ntgStr +", "+ ttcStr, textX + 2, textY + 2);
      street.ctx.fillStyle = extra.color;
      street.ctx.fillText(str, textX, textY);
      if (extra.shadow) {
        street.ctx.strokeStyle = "#333333";
        street.ctx.lineWidth = 1;
        street.ctx.strokeText(str, textX, textY);
      }
    }
    street.ctx.restore();
  }
  function draw(t) {
    var m = typeof ms == "function" ? ms(t) : ms;
    for (var b in m) {
      var bb = carMap[b];
      for (var c in m[b]) {
        var cc = carMap[c];
        if (b != c) {
          drawEdge(t, bb, cc, b < c);
        }
      }
    }
  }
  this.enable = function () { if (this.i == undefined) { this.i = street.addRedrawHook(draw); } };
  this.disable = function () { if (this.i != undefined) { street.removeRedrawHook(this.i); this.i = undefined; } };
}

function addS(data, b, ms) {
  if (ms == undefined) {
    ms = b;
  }
  if (b[0]) {
    for (var i = 0; i < b.length; ++i) {
      data[b[i].id] = ms[i];
    }
  } else {
    data[b.id] = ms;
  }
  return data;
}

function addM(data, from, to, ms) {
  if (ms == undefined) {
    ms = true;
  }
  if (data[from.id] == undefined) {
    data[from.id] = {};
  }
  data[from.id][to.id] = ms;
  return data;
}

function addAccelSymbol(street, b, accel, timeout) {
  if (accel >= 1) {
    var flameHeight = 0.5 * street.laneHeight();
    var flameWidth = 2 * flameHeight;
    var flame = new Elem("flame", "flame", street,
      function (t) { return b.xf(t) - (b.width + flame.width) / street.streetWidth() / 2; },
      function (t) { return b.yf(t); },
      { width: flameWidth
      , height: flameHeight
      , timeout: (timeout != undefined ? timeout : Math.abs(accel - 1) * 3) }
    );
    /*
    var dustHeight = 0.5 * street.laneHeight();
    var dustWidth = dustHeight;
    var dust = new Elem("cloud", "cloud", street,
      function (t) { return b.xf(t) - (b.width + dust.width) / street.streetWidth() / 2; },
      function (t) { return b.yf(t); },
      { width: dustWidth
      , height: dustHeight
      , timeout: (timeout != undefined ? timeout : Math.abs(accel - 1) * 3) }
    );
    dustHeight *= 0.5;
    dustWidth *= 0.5;
    var smallDust = new Elem("cloud", "cloud", street,
      function (t) { return b.xf(t) - (b.width / 2 + smallDust.width / 2 + dust.width) / street.streetWidth(); },
      function (t) { return b.yf(t); },
      { width: dustWidth
      , height: dustHeight
      , timeout: (timeout != undefined ? timeout : Math.abs(accel - 1) * 3) }
    );
    */
  } else {
    var parachuteAspectRatio = 431 / 289;
    var parachuteHeight = 0.65 * street.laneHeight();
    var parachuteWidth = parachuteHeight * parachuteAspectRatio;
    var parachute = new Elem("parachute", "parachute", street,
      function (t) { return b.xf(t) - (b.width + parachute.width) / 2 / street.streetWidth(); },
      function (t) { return b.yf(t); },
      { width: parachuteWidth, height: parachuteHeight
      , timeout: (timeout != undefined ? timeout : Math.abs(accel - 1) * 10) }
    );
    /*
    var skidMarks = new Elem("skid-marks", "skid-marks", street,
      function (t) { return b.xf(t) - (b.width + skidMarks.width) / 2 / street.streetWidth(); },
      function (t) { return b.yf(t); },
      { width: Math.abs(accel - 1) * 0.2 * street.streetWidth()
      , timeout: (timeout != undefined ? timeout : Math.abs(accel - 1) * 10) }
    );
    */
  }
}

function addActionToSit(sit, rstc, name, args, extra) {
  if (extra == undefined) {
    extra = {};
  }
  if (extra.carToColor == undefined) {
    extra.carToColor = function () { return undefined; }
  }
  if (extra.stepInPixels == undefined) {
    extra.stepInPixels = 5;
  }
  if (extra.stepDuration == undefined) {
    extra.stepDuration = 30;
  }
  if (extra.precision == undefined) {
    extra.precision = 1;
  }

  if (name == "wait" && args == undefined) {
    args = [ "<div class=\"wait-action-timestamp\">"+ rstc.time().toFixed(extra.precision) +"</div>" ];
  }
  var act = document.createElement("div");
  var str = name;
  if (args != undefined) {
    str += "(";
    for (var i = 0; i < args.length; ++i) {
      if (i > 0) {
        str += ", ";
      }
      if (typeof args[i] === "number") {
        str += args[i].toFixed(extra.precision);
      } else if (typeof args[i] === "string") {
        var color = extra.carToColor(args[i]);
        if (color != undefined) {
          str += "<span style=\"color: "+ color +";\">"+ args[i] +"</span>";
        } else {
          str += args[i];
        }
      } else {
        str += args[i];
      }
    }
    str += ")";
  }
  act.innerHTML = str;
  act.className = "action";
  sit.insertBefore(act, sit.firstChild);
  var invisiblePos = -1 * act.offsetWidth;
  act.style.marginLeft = invisiblePos +"px";
  act.style.visibility = "hidden";
  var sliderIntervalId = setInterval(function () {
    // when another action is already sliding in, we wait for it being completely slided in
    if (!act.nextSibling || (act.nextSibling.style && act.nextSibling.style.marginLeft && parseInt(act.nextSibling.style.marginLeft.replace(/[^-0-9]/g, "")) >= 0)) {
      clearInterval(sliderIntervalId);
      act.style.visibility = "visible";
      slideElement(act, invisiblePos, 0, extra.stepInPixels, extra.stepDuration);
    }
  }, 50);
}

function updateTimeInSit(sit, rstc) {
  for (var actionNode = sit.firstChild; actionNode; actionNode = actionNode.nextSibling) {
    for (var timeStampNode = actionNode.firstChild; timeStampNode; timeStampNode = timeStampNode.nextSibling) {
      if (timeStampNode.className == "wait-action-timestamp") {
        timeStampNode.innerHTML = rstc.time().toFixed(1);
        return;
      }
    }
  }
}

function sizeToPixel(str, relative) {
  if (typeof str == "string") {
    if (str.indexOf("px") != -1) {
      return parseInt(str.replace(/[^-0-9]/g, ""));
    } else if (str.indexOf("pt") != -1) {
      return 96 / 72 * parseInt(str.replace(/[^-0-9]/g, ""));
    } else if (str.indexOf("%") != -1) {
      return relative * parseFloat(str.replace(/[^-0-9]/g, "")) / 100;
    } else {
      return parseFloat(str);
    }
  } else {
    return str;
  }
}

function bodyFontSizeInPixel() {
  var fontSize = document.body.style.fontSize;
  if (fontSize != undefined &&
      typeof fontSize == "string" &&
      fontSize.length > 0) {
    return sizeToPixel(fontSize, 18);
  } else {
    return 18;
  }
}


function carToColor(car) {
  car = car.toLowerCase();
  var color = undefined;
  switch (car) {
    case "b": return "blue";
    case "c": return "pink";
    case "d": return "green";
    case "e": return "aqua";
    case "f": return "maroon";
    case "g": return "purple";
    case "h": return "red";
  }
  return color;
}

// vim:textwidth=80:shiftwidth=2:softtabstop=2:expandtab

