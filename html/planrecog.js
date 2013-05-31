"use strict";

function createXMLHttpRequest() {
  try { return new XMLHttpRequest(); } catch(e) {}
  try { return new ActiveXObject('Msxml2.XMLHTTP'); } catch (e) {}
  alert('XMLHttpRequest not supported');
  return null;
}

function request(i) {
  var xhr = createXMLHttpRequest();
  var url = "http://localhost:8080/"+ i;
  //console.log("XMLHttpRequest URL: "+ url);
  xhr.open("GET", url, false);
  xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
  /*
  xhr.onreadystatechange = function () {
    console.log("XMLHttpRequest state: "+ xhr.readyState);
    if (xhr.readyState === 4) {
      console.log(xhr.responseText);
      var json = JSON.parse(xhr.responseText);
      processNewSituation(json);
    }
  };
  */
  xhr.send();
  if (xhr.readyState === 4 && xhr.status === 200) {
    var text = xhr.responseText;
    var json = JSON.parse(text);
    return json;
  } else {
    return null;
  }
}

function requestAll(callback) {
  function r(i) {
    var json = request(i);
    if (!json) {
      console.log("request() failed");
      return;
    }
    if (!callback(json)) {
      console.log("callback() failed");
      return;
    }
    setTimeout(function() { r(i+1); }, 10);
  }
  setTimeout(function() { r(0); }, 10);
}


// Given: { ... ntg : [ { b: "D", c: "E", t: 3.0 }, ... ], ... }
// Want: { "D": { "E": 3.0, ... }, ... }
function toMeasurements(json, props) {
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

// vim:textwidth=80:shiftwidth=2:softtabstop=2:expandtab

