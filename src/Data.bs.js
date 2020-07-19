// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var csv = {
  dates: [],
  states: []
};

var monthNames = [
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec"
];

function analyzeData(cases, population) {
  var processPopulation = function (lines, _acc, _total, _state, _index) {
    while(true) {
      var index = _index;
      var state = _state;
      var total = _total;
      var acc = _acc;
      if (index === lines.length) {
        return acc.concat([[
                      state,
                      total
                    ]]);
      }
      var items = Caml_array.caml_array_get(lines, index).split(",");
      if (items.length === 4) {
        var countyState = Caml_array.caml_array_get(items, 2);
        var countyPop = Caml_format.caml_float_of_string(Caml_array.caml_array_get(items, 3));
        if (countyState === state) {
          _index = index + 1 | 0;
          _total = total + countyPop;
          continue ;
        }
        if (state !== "") {
          _index = index + 1 | 0;
          _state = countyState;
          _total = countyPop;
          _acc = acc.concat([[
                  state,
                  total
                ]]);
          continue ;
        }
        _index = index + 1 | 0;
        _state = countyState;
        continue ;
      }
      _index = index + 1 | 0;
      continue ;
    };
  };
  var addCases = function (total, items) {
    return Belt_Array.zipByU(total, items, (function (f, s) {
                  return f + Caml_format.caml_float_of_string(s);
                }));
  };
  var processCases = function (lines, _acc, _total, _state, _index) {
    while(true) {
      var index = _index;
      var state = _state;
      var total = _total;
      var acc = _acc;
      if (index === lines.length) {
        return acc.concat([total]);
      }
      var items = Caml_array.caml_array_get(lines, index).split(",");
      if (items.length >= total.length) {
        var countyState = Caml_array.caml_array_get(items, 2);
        if (countyState === state) {
          _index = index + 1 | 0;
          _total = addCases(total, items.slice(4));
          continue ;
        }
        if (state !== "") {
          var zeros = Belt_Array.make(total.length, 0.0);
          _index = index + 1 | 0;
          _state = countyState;
          _total = addCases(zeros, items.slice(4));
          _acc = acc.concat([total]);
          continue ;
        }
        _index = index + 1 | 0;
        _state = countyState;
        continue ;
      }
      _index = index + 1 | 0;
      continue ;
    };
  };
  var makeStateArray = function (statePopulations, stateCases) {
    var _acc = [];
    var _index = 0;
    while(true) {
      var index = _index;
      var acc = _acc;
      if (index === stateCases.length) {
        return acc;
      }
      console.log(index, Caml_array.caml_array_get(statePopulations, index)[0]);
      var len = Caml_array.caml_array_get(stateCases, index).length;
      var total = Caml_array.caml_array_get(Caml_array.caml_array_get(stateCases, index), len - 1 | 0);
      _index = index + 1 | 0;
      _acc = acc.concat([{
              name: Caml_array.caml_array_get(statePopulations, index)[0],
              population: Caml_array.caml_array_get(statePopulations, index)[1],
              cumulativeCasesPerDay: Caml_array.caml_array_get(stateCases, index),
              totalCases: total,
              pastWeekCases: total - Caml_array.caml_array_get(Caml_array.caml_array_get(stateCases, index), len - 8 | 0)
            }]);
      continue ;
    };
  };
  var popLines = population.split("\n");
  var statePopulations = processPopulation(popLines, [], 0.0, "", 1);
  var caseLines = cases.split("\n");
  var caseHeader = Caml_array.caml_array_get(caseLines, 0);
  var dates = caseHeader.split(",").slice(4);
  var emptyTotals = Belt_Array.make(dates.length, 0.0);
  var stateCases = processCases(caseLines, [], emptyTotals, "", 1);
  csv.dates = dates;
  csv.states = makeStateArray(statePopulations, stateCases);
  
}

function getPopulation(cases) {
  return fetch("covid_county_population_usafacts.csv").then(function (prim) {
                return prim.text();
              }).then(function (population) {
              return Promise.resolve(analyzeData(cases, population));
            });
}

function fetchData(param) {
  return fetch("covid_confirmed_usafacts.csv").then(function (prim) {
                return prim.text();
              }).then(function (cases) {
              return Promise.resolve(getPopulation(cases));
            });
}

exports.csv = csv;
exports.monthNames = monthNames;
exports.analyzeData = analyzeData;
exports.getPopulation = getPopulation;
exports.fetchData = fetchData;
/* No side effect */
