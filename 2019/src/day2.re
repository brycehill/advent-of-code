/* Intcode Computer */

include Lib;

let add = (x, y) => x + y;
let mult = (x, y) => x * y;

let memory =
  Lib.openFile("input2.txt")
  |> Js.String.split(",")
  |> Array.map(Lib.fromString);

let next = list =>
  switch (Belt.List.drop(list, 4)) {
  | Some(l) => l
  | None => []
  };

let rec computer = (slice, original) =>
  switch (slice) {
  | [99, ..._] =>
    /* Js.log("Terminating"); */
    original
  | [op, pos1, pos2, location, ..._] =>
    let fn = op == 1 ? add : mult;
    let result = fn(original[pos1], original[pos2]);
    original[location] = result;
    let rest = next(slice);
    List.length(rest) > 0 ? computer(rest, original) : original;
  | _ =>
    Js.log("Array was empty");
    original;
  };

let main = () => {
  let goal = 19690720;
  let start = 0;
  let _end = 99;
  let noun = start;
  let verb = start;
  for (noun in start to _end) {
    for (verb in start to _end) {
      let input = Array.copy(memory);
      input[1] = noun;
      input[2] = verb;
      let answer = computer(Array.to_list(input), input);
      if (answer[0] == goal) {
        Js.log("Found It!");
        Js.log4("noun:", noun, "verb:", verb);
      };
    };
  };
};

main();
/* 3085697 */
