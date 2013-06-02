"use strict";

function createTable(json, contId, prop) {
  if (!json[prop] || json[prop].length == 0) {
    return;
  }

  function addTable() {
    var table = document.createElement("div");
    table.className = "table";
    table.id = "table-"+ prop;
    var parent = document.getElementById(contId);
    parent.insertBefore(table, parent.firstChild);
    return table;
  }
  function addTr(table) {
    var tr = document.createElement("div");
    tr.className = "tr";
    table.appendChild(tr);
    return tr;
  }
  function addTh(tr, car) {
    var th = document.createElement("div");
    th.className = "td";
    th.style.color = carToColor(car);
    th.style.textAlign = "center";
    if (car) {
      th.innerHTML = car;
    }
    tr.appendChild(th);
    return th;
  }
  function addTd(tr, id) {
    var td = document.createElement("div");
    td.className = "td";
    td.innerHTML = "&nbsp;";
    td.id = id;
    tr.appendChild(td);
    return td;
  }

  var cars = jsonCars(json, [prop]);

  var table = addTable();
  var tr = addTr(table);
  addTd(tr, prop +"-time");
  for (var i = 0; i < cars.length; ++i) {
    addTh(tr, cars[i]);
  }
  for (var i = 0; i < cars.length; ++i) {
    var tr = addTr(table);
    addTh(tr, cars[i]);
    for (var j = 0; j < cars.length; ++j) {
      var td = addTd(tr, prop +"-"+ cars[i] +"-"+ cars[j]);
      if (i == j) {
        td.style.backgroundColor = "lightgrey";
      }
    }
  }
  return table;
}


var TABLE_NUM_PREC = 3;

function populateTableFromJson(json, prop) {
  if (!json[prop]) {
    return;
  }

  var xs = json[prop];
  var time = document.getElementById(prop +"-time");
  if (time) {
    time.innerHTML = "T = "+ json.time.toFixed(1);
  }
  for (var i = 0; i < xs.length; ++i) {
    var b = json[prop][i].b;
    var c = json[prop][i].c;
    var t = json[prop][i].t;
    var s;
    if (t == 0) {
      s = t.toFixed(TABLE_NUM_PREC);
    } else if (t < 0) {
      s = "&#8722;"+ (-1*t).toFixed(TABLE_NUM_PREC);
    } else if (t > 0) {
      s = "+"+ t.toFixed(TABLE_NUM_PREC);
    }
    document.getElementById(prop +"-"+ b +"-"+ c).innerHTML = s;
  }
}


function populateTableFromRstc(rstc, prop) {
  if (!rstc[prop]) {
    return;
  }

  var time = document.getElementById(prop +"-time");
  if (time) {
    time.innerHTML = "T = "+ rstc.start().toFixed(1);
  }
  for (var b in rstc.cars) {
    for (var c in rstc.cars) {
      if (b == c) continue;
      var t = rstc[prop](b, c);
      var s;
      if (t == 0) {
        s = t.toFixed(TABLE_NUM_PREC);
      } else if (t < 0) {
        s = "&#8722;"+ (-1*t).toFixed(TABLE_NUM_PREC);
      } else if (t > 0) {
        s = "+"+ t.toFixed(TABLE_NUM_PREC);
      }
      var e = document.getElementById(prop +"-"+ b +"-"+ c);
      if (e) {
        e.innerHTML = s;
      }
    }
  }
}


// vim:textwidth=80:shiftwidth=2:softtabstop=2:expandtab

