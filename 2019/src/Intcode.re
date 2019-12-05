module Intcode = {
  let add = (x, y) => x + y;
  let mult = (x, y) => x * y;

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
    | [1, pos1, pos2, location, ..._] =>
      let result = add(original[pos1], original[pos2]);
      original[location] = result;
      let rest = next(slice);
      List.length(rest) > 0 ? computer(rest, original) : original;
    | [2, pos1, pos2, location, ..._] =>
      let result = mult(original[pos1], original[pos2]);
      original[location] = result;
      let rest = next(slice);
      List.length(rest) > 0 ? computer(rest, original) : original;
    | _ =>
      Js.log("Array was empty");
      original;
    };
  ();
};
