"use strict";

function createXMLHttpRequest() {
  try { return new XMLHttpRequest(); } catch(e) {}
  try { return new ActiveXObject('Msxml2.XMLHTTP'); } catch (e) {}
  alert('XMLHttpRequest not supported');
  return null;
}

function request(i, async, succHandler, failHandler) {
  var xhr = createXMLHttpRequest();
  var url = "http://localhost:8080/"+ i;
  //console.log("XMLHttpRequest URL: "+ url);
  xhr.open("GET", url, async);
  xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
  if (async) {
    xhr.onreadystatechange = function () {
      if (xhr.readyState === 4 && xhr.status === 200) {
        var text = xhr.responseText;
        var json = JSON.parse(text);
        if (succHandler) succHandler(json);
      } else {
        if (failHandler) failHandler();
      }
    };
  }
  xhr.send();
  if (!async) {
    if (xhr.readyState === 4 && xhr.status === 200) {
      var text = xhr.responseText;
      var json = JSON.parse(text);
      return json;
    } else {
      return null;
    }
  }
}

function requestAll(callback) {
  function r(i) {
    request(i, true, function (json) {
      callback(json);
      setTimeout(function() { r(i+1); }, 10);
    });
  }
  setTimeout(function() { r(0); }, 10);
}

// Given: { ... ntg : [ { b: "D", c: "E", t: 3.0 }, ... ], ... }
// Want: { "D", "E", ... }
function jsonCars(json, props) {
  var set = {};
  for (var j = 0; j < props.length; ++j) {
    var p = props[j];
    for (var i = 0; i < json[p].length; ++i) {
      var b = json[p][i].b;
      var c = json[p][i].c;
      set[b] = true;
      set[c] = true;
    }
  }
  return Object.keys(set).sort();
}


// Given: { ... ntg : [ { b: "D", c: "E", t: 3.0 }, ... ], ... }
// Want: { "D": { "E": 3.0, ... }, ... }
function jsonToMeasurements(json, props) {
  var ms = {};
  for (var j = 0; j < props.length; ++j) {
    var p = props[j];
    if (json[p]) {
      for (var i = 0; i < json[p].length; ++i) {
        var b = json[p][i].b;
        var c = json[p][i].c;
        var t = json[p][i].t;
        if (!ms[b]) {
          ms[b] = {};
        }
        if (!ms[b][c]) {
          ms[b][c] = {};
        }
        ms[b][c][p] = t;
      }
    }
  }
  return ms;
}

function jsonToLanes(json) {
  var lanes = {};
  for (var i = 0; i < json.lane.length; ++i) {
    lanes[json.lane[i].b] = json.lane[i].l;
  }
  return lanes;
}

// vim:textwidth=80:shiftwidth=2:softtabstop=2:expandtab

