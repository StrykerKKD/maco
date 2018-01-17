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

let rec fold_until_HLT eval stack = function
  | x :: xs when x = HLT -> 
    Printf.printf "done\n";
    stack
  | x :: xs -> 
    fold_until_HLT eval (eval stack x) xs
  | [] -> 
    stack

let result_stack = fold_until_HLT eval stack program