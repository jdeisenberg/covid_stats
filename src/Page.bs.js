// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_SortArray = require("bs-platform/lib/js/belt_SortArray.js");
var Data$Covid_stats = require("./Data.bs.js");

function columnToString(col) {
  switch (col) {
    case /* State */0 :
        return "State";
    case /* Total */1 :
        return "Total";
    case /* Per100K */2 :
        return "Per100K";
    
  }
}

function columnFromString(s) {
  switch (s) {
    case "Per100K" :
        return /* Per100K */2;
    case "Total" :
        return /* Total */1;
    default:
      return /* State */0;
  }
}

function directionToString(dir) {
  if (dir) {
    return "Descending";
  } else {
    return "Ascending";
  }
}

function directionFromString(s) {
  if (s === "Ascending") {
    return /* Ascending */0;
  } else {
    return /* Descending */1;
  }
}

function periodToString(period) {
  if (period) {
    return "PastWeek";
  } else {
    return "All";
  }
}

function periodFromString(s) {
  if (s === "PastWeek") {
    return /* PastWeek */1;
  } else {
    return /* All */0;
  }
}

var pageState = {
  column: /* Per100K */2,
  direction: /* Descending */1,
  period: /* PastWeek */1,
  indices: []
};

function to100K(x, index) {
  return x / Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).population * 100000.0;
}

function byStateName(indexA, indexB) {
  var states = Data$Covid_stats.csv.states;
  var result = Caml_array.caml_array_get(states, indexA).name > Caml_array.caml_array_get(states, indexB).name ? 1 : (
      Caml_array.caml_array_get(states, indexA).name < Caml_array.caml_array_get(states, indexB).name ? -1 : 0
    );
  if (pageState.direction === /* Ascending */0) {
    return result;
  } else {
    return -result | 0;
  }
}

function byTotal(indexA, indexB) {
  var states = Data$Covid_stats.csv.states;
  var totalA = pageState.period === /* All */0 ? Caml_array.caml_array_get(states, indexA).totalCases : Caml_array.caml_array_get(states, indexA).pastWeekCases;
  var totalB = pageState.period === /* All */0 ? Caml_array.caml_array_get(states, indexB).totalCases : Caml_array.caml_array_get(states, indexB).pastWeekCases;
  var valueA = totalA / (
    pageState.column === /* Total */1 ? 1.0 : Caml_array.caml_array_get(states, indexA).population / 100000.0
  );
  var valueB = totalB / (
    pageState.column === /* Total */1 ? 1.0 : Caml_array.caml_array_get(states, indexB).population / 100000.0
  );
  var result = valueA > valueB ? 1 : (
      valueA < valueB ? -1 : 0
    );
  if (pageState.direction === /* Ascending */0) {
    return result;
  } else {
    return -result | 0;
  }
}

function sortIndices(indices) {
  console.log(columnToString(pageState.column));
  var match = pageState.column;
  var result = match !== 0 ? Belt_SortArray.stableSortBy(indices, byTotal) : Belt_SortArray.stableSortBy(indices, byStateName);
  console.log(result);
  return result;
}

function makeRow(index) {
  var tr = document.createElement("tr");
  var td1 = document.createElement("td");
  td1.innerHTML = Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).name;
  var td2 = document.createElement("td");
  td2.setAttribute("class", "rightAlign");
  var total = pageState.column === /* Total */1 ? Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).totalCases : Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).pastWeekCases;
  td2.innerHTML = total.toFixed(0);
  var td3 = document.createElement("td");
  td3.setAttribute("class", "rightAlign");
  var perCapita = pageState.column === /* Total */1 ? Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).totalCases : Caml_array.caml_array_get(Data$Covid_stats.csv.states, index).pastWeekCases;
  td3.innerHTML = to100K(perCapita, index).toFixed(2);
  tr.appendChild(td1);
  tr.appendChild(td2);
  tr.appendChild(td3);
  var table = document.getElementById("tableBody");
  if (!(table == null)) {
    table.appendChild(tr);
    return ;
  }
  
}

function removeChildren(parent) {
  while(true) {
    var lastChild = parent.lastChild;
    if (lastChild == null) {
      return ;
    }
    parent.removeChild(lastChild);
    continue ;
  };
}

function drawTable(param) {
  var tableElement = document.getElementById("tableBody");
  if (tableElement == null) {
    return ;
  }
  removeChildren(tableElement);
  var newIndices = sortIndices(Belt_Array.makeBy(Data$Covid_stats.csv.states.length, (function (i) {
              return i;
            })));
  console.log("sorted: ", newIndices);
  Belt_Array.map(newIndices, makeRow);
  
}

exports.columnToString = columnToString;
exports.columnFromString = columnFromString;
exports.directionToString = directionToString;
exports.directionFromString = directionFromString;
exports.periodToString = periodToString;
exports.periodFromString = periodFromString;
exports.pageState = pageState;
exports.to100K = to100K;
exports.byStateName = byStateName;
exports.byTotal = byTotal;
exports.sortIndices = sortIndices;
exports.makeRow = makeRow;
exports.removeChildren = removeChildren;
exports.drawTable = drawTable;
/* No side effect */
