/* Intcode Computer */

include Lib;

let add = (x, y) => x + y;
let mult = (x, y) => x * y;

let input =
  Lib.openFile("/input2.txt")
  |> Js.String.split(",")
  |> Array.map(Lib.fromString);

let next = list =>
  switch (Belt.List.drop(list, 4)) {
  | Some(l) => l
  | None => []
  };

let rec main = data =>
  switch (data) {
  | [99, ..._] =>
    Js.log("Terminating");
    input;
  | [op, pos1, pos2, location, ..._] =>
    let fn = op == 1 ? add : mult;
    let result = fn(input[pos1], input[pos2]);
    input[location] = result;
    let rest = next(data);
    List.length(rest) > 0 ? main(rest) : input;
  | _ =>
    Js.log("Array was empty");
    input;
  };

let answer = main(Array.to_list(input));
Js.log("Answer:");
Js.logMany(answer);
/* 3085697 */
