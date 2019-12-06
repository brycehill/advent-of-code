include Lib;

module StringMap = Map.Make(String);
let sum = List.fold_left((+), 0);

let input =
  Lib.openFile("input6.txt")
  |> Js.String.split("\n")
  |> Array.to_list
  |> List.filter(s => s != "");

/* Build a map of node => list(node) */
let buildMap =
  List.fold_left(
    (map, t) => {
      let [|orbitee, orbiter|] = Js.String.split(")", t);
      let orbiters =
        StringMap.mem(orbitee, map) ?
          StringMap.find(orbitee, map) @ [orbiter] : [orbiter];
      StringMap.add(orbitee, orbiters, map);
    },
    StringMap.empty,
  );

let rec countOrbitsForNodes = (map, nodes) =>
  List.fold_left(
    (count, k) =>
      StringMap.mem(k, map) ?
        {
          let v = StringMap.find(k, map);
          let c = countOrbitsForNodes(map, v);
          count + c;
        } :
        count,
    List.length(nodes),
    nodes,
  );

/* Get all the values of the map, eg [["N1", "N2"], ["N4", "N5"]] */
let getEdges = map => StringMap.bindings(map) |> List.map(((_, v)) => v);

let countAllOrbits = map =>
  getEdges(map) |> List.map(countOrbitsForNodes(map)) |> sum;

buildMap(input) |> countAllOrbits |> Js.log;

/* Pt1: 358244 */
