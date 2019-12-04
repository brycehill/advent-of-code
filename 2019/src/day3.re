/* Manhattan Distance */
include Lib;
module Set = Belt.Set;

let splitOnComma = Js.String.split(",");
let splitOnNewLines = Js.String.split("\n");
let addFst = (amount, (x, y)) => (x + amount, y);
let addSnd = (amount, (x, y)) => (x, y + amount);

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

let start = (0, 0);

/* Build a list of pairs that includes every coordinate the wire passes through */
let buildPairs = fn =>
  List.fold_left((acc, _) => {
    let lastCoord = List.hd(acc);
    [fn(lastCoord), ...acc];
  });

/* Use a Set to take advantage of native `intersect` */
module CoordComparator =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = (pair1, pair2) => {
      let (x1, x2) = pair1;
      let (y1, y2) = pair2;
      switch (compare(x1, y1)) {
      | 0 => compare(x2, y2)
      | x => x
      };
    };
  });

let stepsToCoords = steps =>
  Array.fold_left(
    (points, instruction) => {
      let curr = List.hd(points);
      let (direction, amount) = parseInstruction(instruction);
      let all = Belt.List.make(amount, (0, 0));
      let paths =
        switch (direction) {
        | "U" => buildPairs(addSnd(1), [curr], all)
        | "D" => buildPairs(addSnd(-1), [curr], all)
        | "L" => buildPairs(addFst(-1), [curr], all)
        | "R" => buildPairs(addFst(1), [curr], all)
        | _ => []
        };
      paths @ points;
    },
    [start],
    steps,
  );

let wire1Coords = stepsToCoords(wire1Steps);
let wire2Coords = stepsToCoords(wire2Steps);

let c1 =
  Set.fromArray(Array.of_list(wire1Coords), ~id=(module CoordComparator));

let c2 =
  Set.fromArray(Array.of_list(wire2Coords), ~id=(module CoordComparator));

let intersections =
  Set.toList(Set.intersect(c1, c2)) |> List.filter(pair => pair != start);

let answer =
  List.fold_left(
    (shortest, pair) => {
      let (p1, p2) = start;
      let (q1, q2) = pair;
      /*
       * Taken from: https://en.wikipedia.org/wiki/Taxicab_geometry
       * |p1 - p2| + |q1 - q2|
       */
      let distance = Js.Math.abs_int(p1 - q1) + Js.Math.abs_int(p2 - q2);
      distance < shortest ? distance : shortest;
    },
    100000000,
    intersections,
  );

Js.log(answer);
