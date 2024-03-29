// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Lib$AdventOfCode19 = require("./Lib.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function explode(nums) {
  return $$Array.to_list(nums.split(""));
}

function twoAdjacentDigits(pw) {
  var result = pw.match(new RegExp("(\\d){1}(\\1)+", "g"));
  if (result !== null) {
    return Belt_Array.some(result, (function (match_) {
                  return match_.length === 2;
                }));
  } else {
    return false;
  }
}

function neverDecreases(pw) {
  var nums = $$Array.to_list(pw.split(""));
  if (nums) {
    var match = nums[1];
    if (match) {
      var second = match[0];
      return List.fold_left((function (param, nextNum) {
                      var isMore = param[1];
                      return /* tuple */[
                              nextNum,
                              isMore ? param[0] <= nextNum : isMore
                            ];
                    }), /* tuple */[
                    second,
                    nums[0] <= second
                  ], match[1])[1];
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function test(pw) {
  if (twoAdjacentDigits(pw)) {
    return neverDecreases(pw);
  } else {
    return false;
  }
}

function makeArray(range) {
  if (range.length !== 2) {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "Day4.re",
            41,
            6
          ]
        ];
  }
  var start = range[0];
  var _end = range[1];
  return Belt_Array.range(Curry._1(Lib$AdventOfCode19.Lib.fromString, start), Curry._1(Lib$AdventOfCode19.Lib.fromString, _end));
}

function countValidPasswords(param) {
  return $$Array.fold_left((function (count, candidate) {
                var match = test(String(candidate));
                if (match) {
                  return count + 1 | 0;
                } else {
                  return count;
                }
              }), 0, param);
}

function main(input) {
  return countValidPasswords(makeArray(input.split("-")));
}

console.log(main("156218-652527"));

var Lib = Lib$AdventOfCode19.Lib;

var Re = 0;

var $$String$1 = 0;

exports.Lib = Lib;
exports.Re = Re;
exports.$$String = $$String$1;
exports.explode = explode;
exports.twoAdjacentDigits = twoAdjacentDigits;
exports.neverDecreases = neverDecreases;
exports.test = test;
exports.makeArray = makeArray;
exports.countValidPasswords = countValidPasswords;
exports.main = main;
/*  Not a pure module */
