// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Lib$AdventOfCode19 = require("./Lib.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function add(x, y) {
  return x + y | 0;
}

var mult = Caml_int32.imul;

function next(step, list) {
  var match = Belt_List.drop(list, step);
  if (match !== undefined) {
    return match;
  } else {
    return /* [] */0;
  }
}

function parseOpcode(op) {
  var opa = String(op);
  var len = opa.length;
  var opcode = opa.slice(-2, len);
  if (len !== 3) {
    if (len !== 4) {
      return /* tuple */[
              Curry._1(Lib$AdventOfCode19.Lib.fromString, opcode),
              "00"
            ];
    } else {
      var modes = opa.slice(0, 2);
      return /* tuple */[
              Curry._1(Lib$AdventOfCode19.Lib.fromString, opcode),
              modes
            ];
    }
  } else {
    var mode = opa.slice(0, 1);
    return /* tuple */[
            Curry._1(Lib$AdventOfCode19.Lib.fromString, opcode),
            "0" + mode
          ];
  }
}

function run($staropt$star, instructions) {
  var inputValue = $staropt$star !== undefined ? $staropt$star : 1;
  var _currIdx = 0;
  var instructions$1 = instructions;
  while(true) {
    var currIdx = _currIdx;
    var match = parseOpcode(Caml_array.caml_array_get(instructions$1, currIdx));
    var modes = match[1];
    var op = match[0];
    if (op >= 9) {
      if (op !== 99) {
        console.log("Array was empty or did not match an opcode");
        return instructions$1;
      } else {
        return instructions$1;
      }
    } else if (op > 0) {
      switch (op - 1 | 0) {
        case 0 :
            var match$1 = Belt_Array.slice(instructions$1, currIdx, 4);
            if (match$1.length !== 4) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      35,
                      12
                    ]
                  ];
            }
            var pos1 = match$1[1];
            var pos2 = match$1[2];
            var idx = match$1[3];
            var match$2 = modes[1] === "0";
            var param1 = match$2 ? Caml_array.caml_array_get(instructions$1, pos1) : pos1;
            var match$3 = modes[0] === "0";
            var param2 = match$3 ? Caml_array.caml_array_get(instructions$1, pos2) : pos2;
            Caml_array.caml_array_set(instructions$1, idx, param1 + param2 | 0);
            _currIdx = currIdx + 4 | 0;
            continue ;
        case 1 :
            var match$4 = Belt_Array.slice(instructions$1, currIdx, 4);
            if (match$4.length !== 4) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      43,
                      12
                    ]
                  ];
            }
            var pos1$1 = match$4[1];
            var pos2$1 = match$4[2];
            var idx$1 = match$4[3];
            var match$5 = modes[1] === "0";
            var param1$1 = match$5 ? Caml_array.caml_array_get(instructions$1, pos1$1) : pos1$1;
            var match$6 = modes[0] === "0";
            var param2$1 = match$6 ? Caml_array.caml_array_get(instructions$1, pos2$1) : pos2$1;
            Caml_array.caml_array_set(instructions$1, idx$1, Caml_int32.imul(param1$1, param2$1));
            _currIdx = currIdx + 4 | 0;
            continue ;
        case 2 :
            var match$7 = Belt_Array.slice(instructions$1, currIdx, 2);
            if (match$7.length !== 2) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      51,
                      12
                    ]
                  ];
            }
            var param = match$7[1];
            Caml_array.caml_array_set(instructions$1, param, inputValue);
            _currIdx = currIdx + 2 | 0;
            continue ;
        case 3 :
            var match$8 = Belt_Array.slice(instructions$1, currIdx, 2);
            if (match$8.length !== 2) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      57,
                      12
                    ]
                  ];
            }
            var param$1 = match$8[1];
            var match$9 = modes[1] === "0";
            var output = match$9 ? Caml_array.caml_array_get(instructions$1, param$1) : param$1;
            if (output === 0) {
              console.log("Opcode 4 and all good!");
            } else {
              console.log("Received opcode 4. Outputting:", output);
            }
            _currIdx = currIdx + 2 | 0;
            continue ;
        case 4 :
            var match$10 = Belt_Array.slice(instructions$1, currIdx, 3);
            if (match$10.length !== 3) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      67,
                      12
                    ]
                  ];
            }
            var pos1$2 = match$10[1];
            var pos2$2 = match$10[2];
            var match$11 = modes[1] === "0";
            var param1$2 = match$11 ? Caml_array.caml_array_get(instructions$1, pos1$2) : pos1$2;
            var match$12 = modes[0] === "0";
            var param2$2 = match$12 ? Caml_array.caml_array_get(instructions$1, pos2$2) : pos2$2;
            var match$13 = param1$2 !== 0;
            if (match$13) {
              _currIdx = param2$2;
              continue ;
            } else {
              _currIdx = currIdx + 3 | 0;
              continue ;
            }
        case 5 :
            var match$14 = Belt_Array.slice(instructions$1, currIdx, 3);
            if (match$14.length !== 3) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      73,
                      12
                    ]
                  ];
            }
            var pos1$3 = match$14[1];
            var pos2$3 = match$14[2];
            var match$15 = modes[1] === "0";
            var param1$3 = match$15 ? Caml_array.caml_array_get(instructions$1, pos1$3) : pos1$3;
            var match$16 = modes[0] === "0";
            var param2$3 = match$16 ? Caml_array.caml_array_get(instructions$1, pos2$3) : pos2$3;
            var match$17 = param1$3 === 0;
            if (match$17) {
              _currIdx = param2$3;
              continue ;
            } else {
              _currIdx = currIdx + 3 | 0;
              continue ;
            }
        case 6 :
            var match$18 = Belt_Array.slice(instructions$1, currIdx, 4);
            if (match$18.length !== 4) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      79,
                      12
                    ]
                  ];
            }
            var pos1$4 = match$18[1];
            var pos2$4 = match$18[2];
            var pos3 = match$18[3];
            var match$19 = modes[1] === "0";
            var param1$4 = match$19 ? Caml_array.caml_array_get(instructions$1, pos1$4) : pos1$4;
            var match$20 = modes[0] === "0";
            var param2$4 = match$20 ? Caml_array.caml_array_get(instructions$1, pos2$4) : pos2$4;
            var match$21 = param1$4 < param2$4;
            Caml_array.caml_array_set(instructions$1, pos3, match$21 ? 1 : 0);
            _currIdx = currIdx + 4 | 0;
            continue ;
        case 7 :
            var match$22 = Belt_Array.slice(instructions$1, currIdx, 4);
            if (match$22.length !== 4) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    /* tuple */[
                      "Intcode.re",
                      86,
                      12
                    ]
                  ];
            }
            var pos1$5 = match$22[1];
            var pos2$5 = match$22[2];
            var pos3$1 = match$22[3];
            var match$23 = modes[1] === "0";
            var param1$5 = match$23 ? Caml_array.caml_array_get(instructions$1, pos1$5) : pos1$5;
            var match$24 = modes[0] === "0";
            var param2$5 = match$24 ? Caml_array.caml_array_get(instructions$1, pos2$5) : pos2$5;
            var match$25 = param1$5 === param2$5;
            Caml_array.caml_array_set(instructions$1, pos3$1, match$25 ? 1 : 0);
            _currIdx = currIdx + 4 | 0;
            continue ;
        
      }
    } else {
      console.log("Array was empty or did not match an opcode");
      return instructions$1;
    }
  };
}

var Intcode = {
  Lib: Lib$AdventOfCode19.Lib,
  add: add,
  mult: mult,
  next: next,
  parseOpcode: parseOpcode,
  run: run
};

var $$String$1 = 0;

exports.$$String = $$String$1;
exports.Intcode = Intcode;
/* Lib-AdventOfCode19 Not a pure module */
