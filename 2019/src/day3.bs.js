// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Lib$AdventOfCode19 = require("./Lib.bs.js");

function splitOnComma(param) {
  return param.split(",");
}

function splitOnNewLines(param) {
  return param.split("\n");
}

var directions = Curry._1(Lib$AdventOfCode19.Lib.openFile, "test3.txt").split("\n");

var wire1 = Caml_array.caml_array_get(directions, 0);

var wire2 = Caml_array.caml_array_get(directions, 1);

var wire1Steps = wire1.split(",");

var wire2Steps = wire2.split(",");

function parseInstruction(instruction) {
  var direction = $$String.sub(instruction, 0, 1);
  var amount = Curry._1(Lib$AdventOfCode19.Lib.fromString, $$String.sub(instruction, 1, instruction.length - 1 | 0));
  return /* tuple */[
          direction,
          amount
        ];
}

function addFst(param, amount) {
  return /* tuple */[
          param[0] + amount | 0,
          param[1]
        ];
}

function subFst(param, amount) {
  return /* tuple */[
          param[0] - amount | 0,
          param[1]
        ];
}

function subSnd(param, amount) {
  return /* tuple */[
          param[0],
          param[1] - amount | 0
        ];
}

function addSnd(param, amount) {
  return /* tuple */[
          param[0],
          param[1] + amount | 0
        ];
}

var start = /* tuple */[
  0,
  0
];

function stepsToCoords(steps) {
  return $$Array.fold_left((function (points, instruction) {
                var curr = List.hd(points);
                var match = parseInstruction(instruction);
                var all = Belt_List.make(match[1], /* tuple */[
                      0,
                      0
                    ]);
                var paths;
                switch (match[0]) {
                  case "D" :
                      paths = List.fold_left((function (acc, param) {
                              var match = List.hd(acc);
                              return /* :: */[
                                      /* tuple */[
                                        match[0],
                                        match[1] - 1 | 0
                                      ],
                                      acc
                                    ];
                            }), /* :: */[
                            curr,
                            /* [] */0
                          ], all);
                      break;
                  case "L" :
                      paths = List.fold_left((function (acc, param) {
                              var match = List.hd(acc);
                              return /* :: */[
                                      /* tuple */[
                                        match[0] - 1 | 0,
                                        match[1]
                                      ],
                                      acc
                                    ];
                            }), /* :: */[
                            curr,
                            /* [] */0
                          ], all);
                      break;
                  case "R" :
                      paths = List.fold_left((function (acc, param) {
                              var match = List.hd(acc);
                              return /* :: */[
                                      /* tuple */[
                                        match[0] + 1 | 0,
                                        match[1]
                                      ],
                                      acc
                                    ];
                            }), /* :: */[
                            curr,
                            /* [] */0
                          ], all);
                      break;
                  case "U" :
                      paths = List.fold_left((function (acc, param) {
                              var match = List.hd(acc);
                              return /* :: */[
                                      /* tuple */[
                                        match[0],
                                        match[1] + 1 | 0
                                      ],
                                      acc
                                    ];
                            }), /* :: */[
                            curr,
                            /* [] */0
                          ], all);
                      break;
                  default:
                    paths = /* [] */0;
                }
                return Pervasives.$at(paths, points);
              }), /* :: */[
              start,
              /* [] */0
            ], steps);
}

var wire1Coords = stepsToCoords(wire1Steps);

var wire2Coords = stepsToCoords(wire2Steps);

function findIntersections(xs, ys) {
  return List.filter((function (pair) {
                  return Caml_obj.caml_notequal(pair, /* tuple */[
                              0,
                              0
                            ]);
                }))(List.filter((function (coord1) {
                      return Belt_List.some(ys, (function (coord2) {
                                    return Caml_obj.caml_equal(coord1, coord2);
                                  }));
                    }))(xs));
}

var intersections = findIntersections(wire1Coords, wire2Coords);

console.log(intersections);

var answer = List.fold_left((function (shortest, pair) {
        var distance = Math.abs(-pair[0] | 0) + Math.abs(-pair[1] | 0) | 0;
        console.log("Distance:", distance);
        var match = distance < shortest;
        if (match) {
          return distance;
        } else {
          return shortest;
        }
      }), 100000000, intersections);

console.log(answer);

var Lib = Lib$AdventOfCode19.Lib;

exports.Lib = Lib;
exports.splitOnComma = splitOnComma;
exports.splitOnNewLines = splitOnNewLines;
exports.directions = directions;
exports.wire1 = wire1;
exports.wire2 = wire2;
exports.wire1Steps = wire1Steps;
exports.wire2Steps = wire2Steps;
exports.parseInstruction = parseInstruction;
exports.addFst = addFst;
exports.subFst = subFst;
exports.subSnd = subSnd;
exports.addSnd = addSnd;
exports.start = start;
exports.stepsToCoords = stepsToCoords;
exports.wire1Coords = wire1Coords;
exports.wire2Coords = wire2Coords;
exports.findIntersections = findIntersections;
exports.intersections = intersections;
exports.answer = answer;
/* directions Not a pure module */
