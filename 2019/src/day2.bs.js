// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");
var Lib$AdventOfCode19 = require("./Lib.bs.js");

function add(x, y) {
  return x + y | 0;
}

var mult = Caml_int32.imul;

var input = $$Array.map(Lib$AdventOfCode19.Lib.fromString, Curry._1(Lib$AdventOfCode19.Lib.openFile, "/input2.txt").split(","));

function next(list) {
  var match = Belt_List.drop(list, 4);
  if (match !== undefined) {
    return match;
  } else {
    return /* [] */0;
  }
}

function main(_data) {
  while(true) {
    var data = _data;
    if (data) {
      var op = data[0];
      if (op !== 99) {
        var match = data[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var match$2 = match$1[1];
            if (match$2) {
              var match$3 = op === 1;
              var fn = match$3 ? add : mult;
              var result = Curry._2(fn, Caml_array.caml_array_get(input, match[0]), Caml_array.caml_array_get(input, match$1[0]));
              Caml_array.caml_array_set(input, match$2[0], result);
              var rest = next(data);
              var match$4 = List.length(rest) > 0;
              if (match$4) {
                _data = rest;
                continue ;
              } else {
                return input;
              }
            } else {
              console.log("Array was empty");
              return input;
            }
          } else {
            console.log("Array was empty");
            return input;
          }
        } else {
          console.log("Array was empty");
          return input;
        }
      } else {
        console.log("Terminating");
        return input;
      }
    } else {
      console.log("Array was empty");
      return input;
    }
  };
}

var answer = main($$Array.to_list(input));

console.log("Answer:");

Caml_splice_call.spliceApply(console.log, [answer]);

var Lib = Lib$AdventOfCode19.Lib;

exports.Lib = Lib;
exports.add = add;
exports.mult = mult;
exports.input = input;
exports.next = next;
exports.main = main;
exports.answer = answer;
/* input Not a pure module */
