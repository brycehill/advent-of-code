include Lib;
module Re = Js.Re;

let explode = nums => Js.String.split("", nums) |> Array.to_list;

/* Validators */
let twoAdjacentDigits = pw => Re.fromString("(\\d){1}(\\1)+")->Re.test_(pw);

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
  Js.String.split("-", input) |> makeArray |> countValidPasswords;

main("156218-652527") |> Js.log;

/* 1694 */
