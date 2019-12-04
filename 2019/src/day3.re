/* Manhattan Distance */
include Lib;

let splitOnComma = Js.String.split(",");
let splitOnNewLines = Js.String.split("\n");

let directions = Lib.openFile("test3.txt") |> splitOnNewLines;

let wire1 = directions[0];
let wire2 = directions[1];

let wire1Steps = splitOnComma(wire1);
let wire2Steps = splitOnComma(wire2);

/* Return a pair of the direction and amount, eg ("U", 870) */
let parseInstruction = instruction => {
  let direction = String.sub(instruction, 0, 1);
  let amount =
    String.sub(instruction, 1, String.length(instruction) - 1)
    |> Lib.fromString;
  (direction, amount);
};

let addFst = ((x, y), amount) => (x + amount, y);
let subFst = ((x, y), amount) => (x - amount, y);
let subSnd = ((x, y), amount) => (x, y - amount);
let addSnd = ((x, y), amount) => (x, y + amount);

let start = (0, 0);

let stepsToCoords = steps =>
  Array.fold_left(
    (points, instruction) => {
      let curr = List.hd(points);
      let (direction, amount) = parseInstruction(instruction);
      let all = Belt.List.make(amount, (0, 0));
      let paths =
        /*Get every coord it passes through*/
        switch (direction) {
        | "U" =>
          List.fold_left(
            (acc, (_, _)) => {
              let (x, y) = List.hd(acc);
              [(x, y + 1), ...acc];
            },
            [curr],
            all,
          )
        | "D" =>
          List.fold_left(
            (acc, (_, _)) => {
              let (x, y) = List.hd(acc);
              [(x, y - 1), ...acc];
            },
            [curr],
            all,
          )
        | "L" =>
          List.fold_left(
            (acc, (_, _)) => {
              let (x, y) = List.hd(acc);
              [(x - 1, y), ...acc];
            },
            [curr],
            all,
          )
        | "R" =>
          List.fold_left(
            (acc, (_, _)) => {
              let (x, y) = List.hd(acc);
              [(x + 1, y), ...acc];
            },
            [curr],
            all,
          )
        | _ => []
        };
      paths @ points;
    },
    [start],
    steps,
  );

let wire1Coords = stepsToCoords(wire1Steps);
let wire2Coords = stepsToCoords(wire2Steps);

/* Loop through 2 lists of pairs and find the matching pairs */
let findIntersections = (xs, ys) =>
  List.filter(coord1 => Belt.List.some(ys, coord2 => coord1 == coord2), xs)
  |> List.filter(pair => pair != (0, 0));

let intersections = findIntersections(wire1Coords, wire2Coords);

Js.log(intersections);

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
      Js.log2("Distance:", distance);
      distance < shortest ? distance : shortest;
    },
    100000000,
    intersections,
  );

Js.log(answer);
