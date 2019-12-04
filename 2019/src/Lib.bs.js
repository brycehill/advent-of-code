// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

var dirname = typeof __dirname === "undefined" ? undefined : __dirname;

function openFile(file) {
  var path = dirname !== undefined ? dirname + file : "";
  return Fs.readFileSync(path, "utf8");
}

function fromString(str) {
  var match = Belt_Int.fromString(str);
  if (match !== undefined) {
    return match;
  } else {
    return 0;
  }
}

var Lib_dirname = dirname === undefined ? undefined : Caml_option.some(dirname);

var Lib = {
  dirname: Lib_dirname,
  openFile: openFile,
  fromString: fromString
};

exports.Lib = Lib;
/* dirname Not a pure module */
