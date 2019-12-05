/* Manhattan Distance */
include Lib;
module Set = Belt.Set;

let splitOnComma = Js.String.split(",");
let splitOnNewLines = Js.String.split("\n");
let addFst = (amount, (x, y)) => (x + amount, y);
let addSnd = (amount, (x, y)) => (x, y + amount);
/*
 * Taken from: https://en.wikipedia.org/wiki/Taxicab_geometry
 * |p1 - p2| + |q1 - q2|
 */
let measureDistance = (pair1, pair2) => {
  let (p1, p2) = pair1;
  let (q1, q2) = pair2;
  Js.Math.abs_int(p1 - q1) + Js.Math.abs_int(p2 - q2);
};

let start = (0, 0);
let directions = Lib.openFile("input3.txt") |> splitOnNewLines;

let wire1Steps = splitOnComma(directions[0]);
let wire2Steps = splitOnComma(directions[1]);

/* Return a pair of the direction and amount, eg ("U", 870) */
let parseInstruction = instruction => {
  let direction = String.sub(instruction, 0, 1);
  let amount =
    String.sub(instruction, 1, String.length(instruction) - 1)
    |> Lib.fromString;
  (direction, amount);
};

type coord = {
  pair: (int, int),
  totalSteps: int,
};

/* Build a list of pairs that includes every coordinate the wire passes through */
let buildCoords = (pairFn, curr, all) =>
  List.fold_left(
    (acc, _) => {
      let lastCoord = List.hd(acc);
      let newPair = pairFn(lastCoord.pair);
      [{pair: newPair, totalSteps: lastCoord.totalSteps + 1}, ...acc];
    },
    curr,
    all,
  );

/* Use a Set to take advantage of native `intersect` */

module CoordComparator =
  Belt.Id.MakeComparable({
    type t = coord;
    let cmp = (c1, c2) => {
      let (x1, x2) = c1.pair;
      let (y1, y2) = c2.pair;
      switch (compare(x1, y1)) {
      | 0 => compare(x2, y2)
      | x => x
      };
    };
  });

let toSet = list =>
  Array.of_list(list)
  ->Set.fromArray(~id=(module CoordComparator))
  ->Set.keep(coord => coord.pair != start);

let stepsToCoords = steps =>
  Array.fold_left(
    (cs, instruction) => {
      let lastStep = List.hd(cs);
      let (direction, amount) = parseInstruction(instruction);
      let all = Belt.List.make(amount, start);
      let newCoords =
        switch (direction) {
        | "U" => buildCoords(addSnd(1), [lastStep], all)
        | "D" => buildCoords(addSnd(-1), [lastStep], all)
        | "L" => buildCoords(addFst(-1), [lastStep], all)
        | "R" => buildCoords(addFst(1), [lastStep], all)
        | _ => []
        };
      newCoords @ cs;
    },
    [{pair: start, totalSteps: 0}],
    steps,
  )
  |> toSet;

let set1 = stepsToCoords(wire1Steps);
let set2 = stepsToCoords(wire2Steps);

let fewestSteps =
  Set.intersect(set1, set2)
  ->Set.reduce(
      1000000,
      (fewestSteps, coord) => {
        let found1 = Set.get(set1, coord);
        let found2 = Set.get(set2, coord);
        switch (found1) {
        | Some(c) =>
          switch (found2) {
          | Some(c2) =>
            let newTotal = c2.totalSteps + c.totalSteps;
            newTotal < fewestSteps ? newTotal : fewestSteps;
          | None => fewestSteps
          }
        | None => fewestSteps
        };
      },
    );

Js.log2("Fewest Steps:", fewestSteps);

/* 48012 */
