
open Ast
open Proof

(***********************************************
 * command parsing utilities
 ***********************************************)

let parse_formula (s : string) : formula =
  Katparser.formula_main Katlexer.token (Lexing.from_string s)

let parse_equation (s : string) : equation =
  Katparser.equation_main Katlexer.token (Lexing.from_string s)

let parse_term (s : string) : term =
  Katparser.term_main Katlexer.token (Lexing.from_string s)
  
let noargs (args : string list) : unit =
  match args with
    [] -> ()
  | _ -> failwith "Too many arguments"

let onearg (args : string list) : string =
  match args with
  | [] -> failwith "Not enough arguments"
  | [x] -> x
  | _ -> failwith "Too many arguments"
  
(***********************************************
 * state manipulation
 ***********************************************)

let get_current_theorem (state : state) : theorem =
  match state with 
    | [] -> failwith "Empty library"
    | {thm_name=""} :: _ -> failwith "No current theorem"
    | x :: _ -> x

let get_current_task (state : state) : task =
  let {tasks=tasks} = get_current_theorem state in
  match tasks with
  | t :: _ -> t
  | _ -> failwith "No tasks"

let next_task_name : unit -> id =
  let name = ref 0 in
  fun () -> let n = !name in
    incr name;
    "T" ^ (string_of_int n)
  
let next_premise_name : unit -> id =
  let name = ref 0 in
  fun () -> let n = !name in
    incr name;
    "A" ^ (string_of_int n)
    
let next_theorem_name : unit -> id =
  let name = ref 0 in
  fun () -> let n = !name in
    incr name;
    "L" ^ (string_of_int n)
    
let make_premise (e : equation) : premise =
  {premise_name=next_premise_name(); premise=e}

let make_task (p : premise list) (e : equation) : task =
  {task_name=next_task_name(); premises=p; goal=e}

let initial_task (ce : formula) : task =
  let premises = List.map make_premise (Ast.premises ce) in
  let conclusion = Ast.conclusion ce in
  make_task premises conclusion

let premise_to_string (p : premise) : string =
    "    " ^ p.premise_name ^ ": " ^ (Ast.eqn_to_string p.premise)
    
let task_to_string (t : task) : string =
  let {task_name=name; premises=premises; goal=goal} = t in
  let s =
    (name ^ ":\n  Premises:") ::
    (match premises with
      | [] -> ["    no premises"]
      | _ -> List.map premise_to_string premises)
    @ ["  Goal: " ^ Ast.eqn_to_string goal]
  in String.concat "\n" s
  
let to_string (state : state) : string =
  let {thm_name=name; tasks=tasks; proof=proof; theorem=theorem} = get_current_theorem state in
  let s =
    ["Theorem " ^ name ^ ": " ^ (Ast.formula_to_string theorem);
     "Proof: " ^ Proof.to_string proof] in
  let t =
    match tasks with
      [] -> ["No tasks"]
    | task :: _ ->
        [string_of_int (List.length tasks) ^ " task(s)";
         "Current task " ^ (task_to_string task)] in
  String.concat "\n" (s @ t)
  
(***********************************************
 * core environment
 ***********************************************)

let library : state =
  List.map (fun (n, s) -> {thm_name=n; theorem=(parse_formula s); proof=(Proof.Axiom n); tasks=[]})
  [
      ("ref=", "x = x");
      ("sym", "x = y -> y = x");
      ("trans=", "x = y -> y = z -> x = z");
      ("cong+R", "x = y -> x + z = y + z");
      ("cong.L", "y = z -> x;y = x;z");
      ("cong.R", "x = y -> x;z = y;z");
      ("cong*", "x = y -> x* = y*");
      ("<intro", "x + y = y -> x < y");
      ("<elim", "x < y -> x + y = y");
      ("commut+", "x + y = y + x");
      ("id+R", "x + 0 = x");
      ("idemp+", "x + x = x");
      ("id.L", "1;x = x");
      ("id.R", "x;1 = x");
      ("abs.L", "0;x = 0");
      ("abs.R", "x;0 = 0");
      ("dist.L", "x;(y + z) = x;y + x;z");
      ("dist.R", "(x + y);z = x;z + y;z");
      ("fold<L", "1 + x;x* < x*");
      ("*L", "y + x z < z -> x*;y < z");
      ("*R", "x + z y < z -> x;y* < z");
      ("cong~", "A = B -> ~A = ~B");
      ("deMorgan+", "~(A + B) = ~A ~B");
      ("~~", "~~A = A");
      ("tnd", "A + ~A = 1")
  ]
  
let no_theorem = {thm_name=""; tasks=[]; proof=Proof.NoProof; theorem=[]}
  
let init_state = no_theorem :: library
