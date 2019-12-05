include Lib;
include Intcode;

let memory =
  Lib.openFile("input5.txt")
  |> Js.String.split(",")
  |> Array.map(Lib.fromString)
  |> Intcode.run(~inputValue=1);

Js.log("Done");

/* Pt1: 9654885 */;
