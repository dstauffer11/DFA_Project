
open Ast
open Util
open Match
open State
open Proof

(***********************************************
* command handlers
***********************************************)

(***********************************************
* library
***********************************************)

let status (tasks : task list) (proof : proof) : string =
  match proof with
  | Axiom _ -> "axiom"
  | _ -> match tasks with
      | [] -> "proved"
      | _ -> Printf.sprintf "%d task(s)" (List.length tasks)

let view_library (args : string list) (state : state) : state =
  noargs args;
  let f { thm_name = n; theorem = s; tasks = tasks; proof = proof } =
    if n = "" then () else
      Printf.printf "%9s: %s [%s]\n" n (utf8_pad (formula_to_string s)) (status tasks proof) in
  List.iter f state;
  state

(***********************************************
* get
***********************************************)

let move_to_front (f : 'a -> bool) (s : 'a list) : 'a list =
  let x = List.find f s in
  let t = List.filter (fun y -> y <> x) s in
  x :: t

let get_theorem (name : string) (state : state) : state =
  let new_lib = move_to_front (fun { thm_name = n } -> name = String.uppercase n) state in
  List.filter (fun y -> y <> no_theorem) new_lib

let get_task (name : string) (state : state) : state =
  let t = get_current_theorem state in
  let new_tasks = move_to_front (fun { task_name = n } -> String.uppercase name = String.uppercase n) t.tasks in
  { thm_name = t.thm_name; theorem = t.theorem; proof = t.proof; tasks = new_tasks } :: List.tl state

let get (args : string list) (state : state) : state =
  let x = String.uppercase(onearg args) in
  try get_theorem x state
  with Not_found ->
      try get_task x state
      with Not_found -> failwith "No such theorem or task"

(***********************************************
* normalize
***********************************************)

(*let normalize (args : string list) (state : state) : state =                                   *)
(*  let t = get_current_theorem state in                                                         *)
(*  try                                                                                          *)
(*    let p = Proof.normalize t.proof in                                                         *)
(*    { thm_name = t.thm_name; tasks = t.tasks; proof = p; theorem = t.theorem } :: List.tl state*)
(*  with Failure x -> print_endline x; state                                                     *)

(***********************************************
* cite
***********************************************)

(* check if we need to ask for a value for an unmatched variable *)
let rec unmatched (ce : formula) : id list =
  let pvars = vars_in_formula (Ast.premises ce) in
  let cvars = vars_in_equation (Ast.conclusion ce) in
  List.filter (fun x -> not (List.mem x cvars)) pvars

(* resolve ambiguities by user input in case of multiple matches *)
let resolve_subst (ss : substitution list) (ce : formula) : substitution =
  assert (List.length ss >= 2);
  let rec resolve (ss : substitution list) =
    let _ =
      let z = range 0 (List.length ss) in
      Printf.printf "Ambiguous match for conclusion %s of cited formula\n" (eqn_to_string (conclusion ce));
      print_endline "Please specify desired bindings by number:";
      let f n s = Printf.sprintf "%d: %s" n (subst_to_string s) in
      List.iter print_endline (List.map2 f z ss) in
    try
      let index = (print_string "? "; read_int()) in
      List.nth ss index
    with _ -> print_endline "Invalid input, please try again"; resolve ss in
  resolve ss

let get_unmatched (u : id list) (thm : formula) : substitution =
  let rec get_one (x : id) : term =
    Printf.printf "Please provide a binding for %s in %s\n" x (formula_to_string thm);
    let input = read_line() in
    if input = "" then failwith "Canceled" else
      try
        flatten (parse_term input)
      with _ -> print_endline "Could not parse, please try again"; get_one x in
  let s = Subst.make() in
  let _ = List.iter (fun x -> Subst.add s x (get_one x)) u in
  s

let cite (args : string list) (state : state) : state =
  let x = String.uppercase (onearg args) in
  let { task_name = taskname; premises = premises; goal = goal } = get_current_task state in
  let { thm_name = name; theorem = ce } as cited =
    try List.find (fun { thm_name = n } -> x = String.uppercase n) state
    with Not_found -> failwith "No such theorem" in
  
  (* ensure acyclicity in the library *)
  let { thm_name = n } = get_current_theorem state in
  if HashSet.mem (Proof.dependencies cited state) n
  then failwith "Citation would create a circular dependency" else
    
    (* unify goal with the conclusion of the cited theorem *)
    let s = get_unmatched (unmatched ce) ce in
    let substs = Match.match_equation (conclusion ce) goal in
    let t =
      match substs with
      | [] -> failwith (name ^ " does not apply")
      | [t] -> t
      | _ -> resolve_subst substs ce in
    let s = Subst.consis s t in
    
    (* apply the theorem under the substitution *)
    let spec = subst_in_formula s ce in
    let task = get_current_task state in
    assert (conclusion spec = task.goal);  (* sanity check *)
    let new_tasks = List.map (make_task task.premises) (Ast.premises spec) in
    let t = get_current_theorem state in
    let p = Proof.cite name s new_tasks task.premises in
    let new_proof = Proof.subst p task.task_name t.proof in
    let tasks = new_tasks @ List.tl t.tasks in
    { thm_name = t.thm_name; tasks = tasks; proof = new_proof; theorem = t.theorem } :: List.tl state

(***********************************************
* use
***********************************************)

let use (args : string list) (state : state) : state =
  let x = String.uppercase(onearg args) in
  let { task_name = taskname; premises = premises; goal = goal } = get_current_task state in
  let { thm_name = thmname; tasks = tasks; proof = proof; theorem = theorem } = get_current_theorem state in
  let { premise_name = n; premise = e } =
    try List.find (fun { premise_name = n } -> x = n) premises
    with Not_found -> failwith "No such premise" in
  if e <> goal then failwith "Premise does not match" else
    let proof = Proof.subst (Var n) taskname proof in
    { thm_name = thmname; tasks = List.tl tasks; proof = proof; theorem = theorem } :: List.tl state

(***********************************************
* tasks
***********************************************)

let tasks (args : string list) (state : state) : state =
  noargs args;
  let t = get_current_theorem state in
  List.iter print_endline (List.map task_to_string t.tasks); state

(***********************************************
* undo
***********************************************)

let undo (args : string list) (_ : state) : state =
  noargs args;
  raise Undo

(***********************************************
* toggle character encoding
***********************************************)

let enc (args : string list) (state : state) : state =
  noargs args;
  Ast.utf8 := not !Ast.utf8;
  state

(***********************************************
* publish - start a new theorem
***********************************************)

let rec publish (args : string list) (state : state) : state =
  let theorem = String.concat " " args in
  if theorem = "" then failwith "No theorem specified" else
    let ce = flatten_formula (parse_formula theorem) in
    let task = initial_task ce in
    let t = { thm_name = next_theorem_name(); tasks =[task]; proof = (to_proof task ce); theorem = ce } in
    t :: state

(***********************************************
* reset - erase proof
***********************************************)

let reset (args : string list) (state : state) : state =
  noargs args;
  let t = get_current_theorem state in
  if t.thm_name = "" then state else
    let t = get_current_theorem state in
    let task = initial_task t.theorem in
    let t = { thm_name = t.thm_name; tasks =[task]; proof = (to_proof task t.theorem); theorem = t.theorem } in
    t :: List.tl state

(***********************************************
* load / save state
***********************************************)

let save (args : string list) (state : state) : state =
  let file = onearg args in
  let _ =
    try
      let check = open_in_bin file in
      close_in check;
      print_string "File exists; overwrite? ";
      let input = read_line() in
      if input = "y" || input = "yes" then () else failwith "Nothing written"
    with Sys_error _ -> () in
  let out = open_out_bin file in
  output_value out state;
  close_out out;
  state

let load (args : string list) (state : state) : state =
  let file = onearg args in
  let inp =
    try open_in_bin file
    with Sys_error s -> failwith s in
  let new_state : state =
    try input_value inp
    with _ -> failwith "Corrupt library file" in
  close_in inp;
  (* keep track of new theorems that replace old theorems *)
  let h : (id, proof) Hashtbl.t = Hashtbl.create 11 in
  let f (old_thm : theorem) : theorem =
    try
      let new_thm = find_in_lib old_thm.thm_name new_state in
      Hashtbl.add h (String.uppercase old_thm.thm_name) (Theorem new_thm.thm_name); new_thm
    with Failure _ -> old_thm in
  (* replace old theorems with new *)
  let state = List.map f state in
  Hashtbl.iter (fun x th -> Printf.printf "Replacing %s\n" (Proof.to_string th)) h;
  (* change citations in old list to cite new names *)
  let g = Proof.subst_for_all_cited h in
  let f (thm : theorem) : theorem =
    {thm_name=thm.thm_name; proof=(g thm.proof); tasks=thm.tasks; theorem=thm.theorem} in
  let state = List.map f state in
  (* remove replaced ones from new list *)
  let f {thm_name=n} = not (Hashtbl.mem h (String.uppercase n)) in
  let new_state = List.filter f new_state in
  no_theorem :: new_state @ state

(***********************************************
* rename
***********************************************)

let rename (args : string list) (state : state) : state =
  let new_name = onearg args in
  let uname = String.uppercase new_name in
  if List.exists (fun { thm_name = n } -> String.uppercase n = uname) state
  then failwith "Name already exists" else
  let { thm_name=old_name } = get_current_theorem state in
  let state = List.map (Proof.rename old_name new_name) state in
  let ct = get_current_theorem state in
  { thm_name=new_name; theorem=ct.theorem; proof=ct.proof; tasks=ct.tasks } :: List.tl state

(***********************************************
* depends
***********************************************)

let depends (args : string list) (state : state) : state =
  noargs args;
  let thm = get_current_theorem state in
  let dep = Proof.dependencies thm state in
  HashSet.iter print_endline dep;
  state

(***********************************************
* help
***********************************************)

let rec help (_ : string list) (state : state) : state =
  print_endline "Commands are:";
  let f (x, n, _, s) =
    let arg = if n = 0 then "" else " x" in
    Printf.printf "%9s  %s%s\n" (x ^ arg) s arg in
  List.iter f commands;
  state

and commands =
  [("library", 0, view_library, "view library");
  ("publish", 1, publish, "publish theorem");
  ("use", 1, use, "use an assumption");
  ("cite", 1, cite, "cite an axiom");
  ("get", 1, get, "get theorem or task");
  ("tasks", 0, tasks, "show list of tasks");
  ("undo", 0, undo, "undo last command");
  ("enc", 0, enc, "toggle ISO-8859-1/UTF-8 character encoding");
  ("reset", 0, reset, "begin proof from scratch");
  ("save", 1, save, "save state in file");
  ("load", 1, load, "load state from file");
  ("rename", 1, rename, "rename current theorem");
  ("depends", 0, depends, "list dependencies of current theorem");
  ("quit", 0, (fun _ _ -> raise Quit), "quit");
  ("help", 0, help, "show list of commands")
  ]

let do_command (input : string) (state : state) : state =
  let s = Str.split (Str.regexp "[ \t]+") input in
  match s with
  | [] -> state
  | cmd :: args ->
      let (_, _, f, _) =
        try List.find (fun (x, _, _, _) -> cmd = x) commands
        with Not_found -> failwith ("Unknown command " ^ cmd)
      in f args state
