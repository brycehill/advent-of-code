include Lib;
include Intcode;

let memory =
  Lib.openFile("input2.txt")
  |> Js.String.split(",")
  |> Array.map(Lib.fromString);

let main = () => {
  let goal = 19690720;
  let start = 0;
  let _end = 99;
  let noun = start;
  for (noun in start to _end) {
    for (verb in start to _end) {
      let input = Array.copy(memory);
      input[1] = noun;
      input[2] = verb;
      let answer = Intcode.computer(Array.to_list(input), input);
      if (answer[0] == goal) {
        Js.log("Found It!");
        Js.log4("noun:", noun, "verb:", verb);
      };
    };
  };
};

main();
/* Pt1: 3085697 Pt2: 9425 */
