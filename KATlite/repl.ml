open Util
open Command
open Proof
open State

let undo_stack : state list ref = ref []

(* command loop *)
let rec repl (state : state) : unit =
  print_string "? ";
  let state =
    try
      let input = read_line() in
      let new_state = do_command input state in
      undo_stack := state :: !undo_stack;
      new_state
    with Failure s -> print_endline s; state
    | Parsing.Parse_error -> print_endline "Parse Error"; state
    | Undo ->
        match !undo_stack with
        | [] -> print_endline "Nothing to undo"; state
        | state :: rest -> undo_stack := rest; state in
  print_endline "================================================";
  begin try
    print_endline (State.to_string state)
  with Failure s -> print_endline s end;
  repl state

let _ =
  print_endline "KAT lite version 1.0";
  try repl init_state
  with Quit -> print_endline "bye"
