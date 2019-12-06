module String = Js.String;

module Intcode = {
  include Lib;
  let add = (x, y) => x + y;
  let mult = (x, y) => x * y;

  let next = (step, list) =>
    switch (Belt.List.drop(list, step)) {
    | Some(l) => l
    | None => []
    };

  let parseOpcode = op => {
    let opa = Belt.Int.toString(op);
    let len = String.length(opa);
    let opcode = String.slice(-2, len, opa);
    switch (len) {
    | 3 =>
      let mode = String.slice(0, 1, opa);
      (Lib.fromString(opcode), "0" ++ mode);
    | 4 =>
      let modes = String.slice(0, 2, opa);
      (Lib.fromString(opcode), modes);
    | _ => (Lib.fromString(opcode), "00")
    };
  };

  let run = (~inputValue=1, instructions) => {
    let rec run = (currIdx, instructions) => {
      let (op, modes) = parseOpcode(instructions[currIdx]);
      switch (op) {
      | 1 =>
        let numOfValues = 4;
        let [|_, pos1, pos2, idx|] =
          Belt.Array.slice(instructions, currIdx, numOfValues);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        instructions[idx] = param1 + param2;
        run(currIdx + numOfValues, instructions);
      | 2 =>
        let numOfValues = 4;
        let [|_, pos1, pos2, idx|] =
          Belt.Array.slice(instructions, currIdx, numOfValues);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        instructions[idx] = param1 * param2;
        run(currIdx + numOfValues, instructions);
      | 3 =>
        let numOfValues = 2;
        let [|_, param|] =
          Belt.Array.slice(instructions, currIdx, numOfValues);
        instructions[param] = inputValue;
        run(currIdx + numOfValues, instructions);
      | 4 =>
        let numOfValues = 2;
        let [|_, param|] =
          Belt.Array.slice(instructions, currIdx, numOfValues);
        let output = modes.[1] == "0" ? instructions[param] : param;
        if (output == 0) {
          Js.log("Opcode 4 and all good!");
        } else {
          Js.log2("Received opcode 4. Outputting:", output);
        };
        run(currIdx + numOfValues, instructions);
      | 5 =>
        let [|_, pos1, pos2|] = Belt.Array.slice(instructions, currIdx, 3);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        param1 != 0 ?
          run(param2, instructions) : run(currIdx + 3, instructions);
      | 6 =>
        let [|_, pos1, pos2|] = Belt.Array.slice(instructions, currIdx, 3);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        param1 == 0 ?
          run(param2, instructions) : run(currIdx + 3, instructions);
      | 7 =>
        let [|_, pos1, pos2, pos3|] =
          Belt.Array.slice(instructions, currIdx, 4);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        instructions[pos3] = param1 < param2 ? 1 : 0;
        run(currIdx + 4, instructions);
      | 8 =>
        let [|_, pos1, pos2, pos3|] =
          Belt.Array.slice(instructions, currIdx, 4);
        let param1 = modes.[1] == "0" ? instructions[pos1] : pos1;
        let param2 = modes.[0] == "0" ? instructions[pos2] : pos2;
        instructions[pos3] = param1 == param2 ? 1 : 0;
        run(currIdx + 4, instructions);
      | 99 => instructions
      | _ =>
        Js.log("Array was empty or did not match an opcode");
        instructions;
      };
    };

    run(0, instructions);
  };
};

/* 9654885 */
