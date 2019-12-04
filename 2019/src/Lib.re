module Lib = {
  let dirname: option(string) = [%bs.node __dirname];

  let openFile = file => {
    let path =
      switch (dirname) {
      | Some(dirname) => dirname ++ file
      | None => ""
      };
    Node_fs.readFileSync(path, `utf8);
  };

  let fromString = str =>
    switch (Belt.Int.fromString(str)) {
    | None => 0
    | Some(num) => num
    };
};
