include Lib

let rec calculateFuel = mass => {
  let fuel = (mass / 3) - 2
  fuel <= 0 ?  0 : fuel + calculateFuel(fuel)
}

Lib.openFile("/input1.txt")
  -> Js.String.split("\n", _)
  -> Array.to_list
  -> List.filter(str => str !== "", _)
  -> List.map(Lib.fromString, _)
  -> List.fold_left((total, mass) => total + calculateFuel(mass), 0, _)
  -> Js.log

/* 4856963 */
