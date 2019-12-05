include Lib;
module Re = Js.Re;
module String = Js.String;

let explode = nums => String.split("", nums) |> Array.to_list;

/* Validators */

let twoAdjacentDigits = pw => {
  let result =
    Re.fromStringWithFlags("(\\d){1}(\\1)+", "g")->String.match(pw);
  switch (result) {
  | Some(matches) =>
    Belt.Array.some(matches, match => String.length(match) == 2)
  | None => false
  };
};

/* Loop through each character and compare it to the previous character */
let neverDecreases = pw => {
  let nums = explode(pw);
  switch (nums) {
  | [first, second, ...rest] =>
    let (_, matches) =
      List.fold_left(
        ((prevNum, isMore), nextNum) => (
          nextNum,
          isMore ? prevNum <= nextNum : isMore,
        ),
        (second, first <= second),
        rest,
      );
    matches;
  | _ => false
  };
};

let test = pw => twoAdjacentDigits(pw) && neverDecreases(pw);

let makeArray = range => {
  let [|start, _end|] = range;
  Belt.Array.range(Lib.fromString(start), Lib.fromString(_end));
};

let countValidPasswords =
  Array.fold_left(
    (count, candidate) =>
      test(Belt.Int.toString(candidate)) ? count + 1 : count,
    0,
  );

let main = input =>
  String.split("-", input) |> makeArray |> countValidPasswords;

main("156218-652527") |> Js.log;

/* Pt1: 1694 Pt2: 1148 */
