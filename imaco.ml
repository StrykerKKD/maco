let stack_size = 256

let stack = Array.make stack_size 0

type instruction_set =
  | HLT
  | PSH
  | POP
  | ADD
  | MUL
  | DIV
  | SUB
  | SLT
  | MOV
  | SET
  | LOG
  | IF
  | IFN
  | GLD
  | GPT
  | NOP

let instruction_index = function
  | 0 -> HLT
  | 1 -> PSH
  | 2 -> POP
  | 3 -> ADD
  | 4 -> MUL
  | 5 -> DIV
  | 6 -> SUB
  | 7 -> SLT
  | 8 -> MOV
  | 9 -> SET
  | 10 -> LOG
  | 11 -> IF
  | 12 -> IFN
  | 13 -> GLD
  | 14 -> GPT
  | 15 -> NOP
  | _ -> failwith "Bad instruction index!\n"

let register_size = 12

let registers = Array.make register_size 0

type register_set =
  | A | B | C | D | E | F | I | J
  | EX
  | EXA
  | IP
  | SP

let register_index = function
  | A -> 0 | B -> 1 | C -> 2 | D -> 3 | E -> 4 | F -> 5 | I -> 6 | J -> 7
  | EX -> 8
  | EXA -> 9
  | IP -> 10
  | SP -> 11 

let instructions = Array.make 1024 0

let instruction_count = ref 0

let instruction_space = ref 4

let running = ref true

let is_jmp = ref false

let get_sp () = registers.(register_index SP)

let set_sp value = registers.(register_index SP) <- value

let get_ip () = registers.(register_index IP)

let set_ip value = registers.(register_index IP) <- value

let fetch () = instructions.(get_ip ())

let get_register index = registers.(register_index index)

let set_register index value = registers.(register_index index) <- value

let get_stack index = stack.(index)

let set_stack index value = stack.(index) <- value

let eval = function
  | HLT -> 
    running := false;
    Printf.printf "Finished Execution\n"
  | PSH -> 
    get_sp () + 1 |> set_sp;
    get_ip () + 1 |> set_ip;
    set_stack (get_sp ()) instructions.(get_ip ())
  | POP -> 
    get_sp () - 1 |> set_sp
  | ADD -> 
    get_stack (get_sp()) |> set_register A;
    get_sp () - 1 |> set_sp;
    get_stack (get_sp()) |> set_register B;
    get_register B + get_register A |> set_register C;
    get_register C |> set_stack (get_sp());
    Printf.printf "%d + %d = %d\n" (get_register B) (get_register A) (get_register C)
  | MUL -> 
    get_stack (get_sp()) |> set_register A;
    get_sp () - 1 |> set_sp;
    get_stack (get_sp()) |> set_register B;
    get_register B * get_register A |> set_register C;
    get_register C |> set_stack (get_sp());
    Printf.printf "%d * %d = %d\n" (get_register B) (get_register A) (get_register C)
  | DIV -> 
    get_stack (get_sp()) |> set_register A;
    get_sp () - 1 |> set_sp;
    get_stack (get_sp()) |> set_register B;
    get_register B / get_register A |> set_register C;
    get_register C |> set_stack (get_sp());
    Printf.printf "%d / %d = %d\n" (get_register B) (get_register A) (get_register C)
  | SUB -> 
    get_stack (get_sp()) |> set_register A;
    get_sp () - 1 |> set_sp;
    get_stack (get_sp()) |> set_register B;
    get_register B - get_register A |> set_register C;
    get_register C |> set_stack (get_sp());
    Printf.printf "%d - %d = %d\n" (get_register B) (get_register A) (get_register C)
  | SLT -> 
    get_sp () + 1 |> set_sp;
    let curent_stack = get_stack (get_sp ()) in
    let next_stack = get_stack (get_sp () + 1) in
    if next_stack < curent_stack then
      set_stack curent_stack 1
    else
      set_stack curent_stack 0
  | MOV -> 
    let next_instruction = instructions.(get_ip () + 2) in
    let instruction = instructions.(get_ip () + 1) in
    registers.(next_instruction) <- registers.(instruction);
    get_ip () + 2 |> set_ip
  | SET -> 
    let next_instruction = instructions.(get_ip () + 2) in
    let instruction = instructions.(get_ip () + 1) in
    registers.(instruction) <- registers.(next_instruction);
    get_ip () + 2 |> set_ip
  | LOG -> 
    let instruction = instructions.(get_ip () + 1) in
    Printf.printf "%d\n" registers.(instruction);
    get_ip () + 1 |> set_ip
  | IF  ->
    let next_next_instruction = instructions.(get_ip () + 3) in
    let next_instruction = instructions.(get_ip () + 2) in
    let instruction = instructions.(get_ip () + 1) in
    if registers.(instruction) = instructions.(next_instruction) then
      let _ = set_ip instructions.(next_next_instruction) in
      is_jmp := true
    else
      get_ip () + 3 |> set_ip
  | IFN -> 
    let next_next_instruction = instructions.(get_ip () + 3) in
    let next_instruction = instructions.(get_ip () + 2) in
    let instruction = instructions.(get_ip () + 1) in
    if registers.(instruction) <> instructions.(next_instruction) then
      let _ = set_ip instructions.(next_next_instruction) in
      is_jmp := true
    else
      get_ip () + 3 |> set_ip
  | GLD -> 
    get_sp () + 1 |> set_sp;
    get_ip () + 1 |> set_ip;
    set_stack (get_sp ()) registers.(instructions.(get_ip ()))
  | GPT -> 
    registers.(instructions.(get_ip () + 1)) <- get_stack (get_sp ());
    get_ip () + 1 |> set_ip
  | NOP -> 
    Printf.printf("Do Nothing\n")

let add_instruction index instruction =
  instructions.(index) <- instruction

let set_instructions filename =
  let chan = Scanf.Scanning.from_file filename in
  let i = ref 0 in
  while not (Scanf.Scanning.end_of_input chan) do
    Scanf.bscanf chan " %d " (add_instruction !i);
    i := !i + 1
  done;
  Scanf.Scanning.close_in chan;
  instruction_count := !i

let main () =
  if Array.length Sys.argv = 1 then
    failwith "error: no input files\n";
  let file_name = Sys.argv.(1) in
  set_instructions file_name;
  set_sp (-1);
  while !running && ((get_ip ()) < !instruction_count) do
    is_jmp := false;
    Printf.printf "Instruction: %d, ip: %d\n" (fetch ()) (get_ip ());
    fetch () |> instruction_index |> eval;
    if not !is_jmp then
      get_ip () + 1 |> set_ip
  done

let _ = main ()