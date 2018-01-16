let running = ref true
let ip = ref 0
let sp = ref (-1)

let stack = Array.make 256 0

type instruction_set =
  | PSH
  | ADD
  | POP
  | HLT
  | Num of int

let program = [|
  PSH; Num 5;
  PSH; Num 6;
  ADD;
  POP;
  HLT 
  |]

let fetch () = program.(!ip)

let get_number = function
  | Num n -> n
  | _ -> failwith "You can only push numbers onto the stack!"

let eval = function
  | HLT -> 
    running := false;
    Printf.printf "done\n"
  | PSH ->
    sp := !sp + 1;
    ip := !ip + 1;
    Array.set stack !sp (get_number program.(!ip))
  | POP -> 
    let val_popped = stack.(!sp) in
    sp := !sp - 1;
    Printf.printf "popped %d\n" val_popped
  | ADD ->
    let a = stack.(!sp) in
    sp := !sp - 1;
    let b = stack.(!sp) in
    sp := !sp - 1;
    let result = a + b in
    sp := !sp + 1;
    Array.set stack !sp result
  | Num number ->
    Printf.printf "number %d\n" number

let main () =
  while !running do
    fetch () |> eval;
    ip := !ip + 1
  done

let _ = main ()