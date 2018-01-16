let stack = []

type instruction_set =
  | PSH of int
  | ADD
  | POP
  | HLT

let program = [
  PSH 5;
  PSH 6;
  ADD;
  POP;
  HLT
  ]

let eval stack = function
  | HLT -> 
    Printf.printf "done\n";
    stack
  | PSH value -> 
    List.cons value stack
  | POP -> 
    let val_popped = List.hd stack in
    Printf.printf "popped %d\n" val_popped;
    List.tl stack
  | ADD -> 
    let a = stack |> List.hd in
    let b = stack |> List.tl |> List.hd in
    let result = a + b in
    stack |> List.tl |> List.tl |> List.cons result

let result = List.fold_left eval stack program