
open Ast
open Util

(***********************************************
 * syntax
 ***********************************************)

type proof =
  | LambdaP of (id list) * proof
  | LambdaS of (id list) * proof
  | ApplyS of proof * substitution
  | ApplyP of proof * proof list
  | Var of id
  | Task of id
  | Axiom of id
  | Theorem of id
  | NoProof

type premise = {premise_name : string; premise : equation}
type task = {task_name: string; premises : premise list; goal : equation}
type theorem = {thm_name : string; theorem : formula; proof : proof; tasks : task list}
type state = theorem list

(***********************************************
 * output
 ***********************************************)

  let rec to_string (p : proof) : string =
    let protect x = match x with
      | Task id -> id
      | (ApplyS _ | ApplyP _) -> "(" ^ to_string x ^ ")"
      | _ -> to_string x in
    match p with
      | (Var id | Axiom id | Task id | Theorem id) -> id
      | (LambdaS (s, p) | LambdaP (s, p)) ->
        (match s with
          | [] -> protect p
          | _  -> (if !utf8 then "λ" else "\\") ^ (String.concat "," s) ^ "." ^ protect p)
      | ApplyS (p, s) -> to_string p ^ Match.subst_to_string s
      | ApplyP (p, q) ->
        (match q with
          | [] -> to_string p
          | [x] -> to_string p ^ " " ^  protect x
          | _ -> to_string p ^ " " ^ (String.concat " " (List.map protect q)))
      | NoProof -> ""

(***********************************************
 * utilities
 ***********************************************)

(*  (* make deep copy *)                                                                               *)
(*  let rec copy (p:proof) : proof =                                                                   *)
(*    match p with                                                                                     *)
(*      (Var _ | CONST _ | Axiom _) -> p                                                               *)
(*    | Task (id,(ce,t,f)) -> Task (id,(ce,List.map copy t,f))                                         *)
(*    | LambdaS (s,q) -> LambdaS (s,copy q)                                                            *)
(*    | LambdaP (s,q) -> LambdaP (s,copy q)                                                            *)
(*    | ApplyS (q,s) -> ApplyS (copy q,s)                                                              *)
(*(*    | APPLYF (q,s) -> APPLYF (copy q,s) *)                                                         *)
(*    | ApplyP (q,r) -> ApplyP (copy q,List.map copy r)                                                *)
(*    | SUBST (f,e,d,q,r) -> SUBST (f,e,d,copy q,copy r)                                               *)
(*                                                                                                     *)
(*  (* get a list of tasks *)                                                                          *)
(*  let getTasks (proof:proof) : task list =                                                           *)
(*    let rec gt (p:proof) : task list =                                                               *)
(*      match p with                                                                                   *)
(*        Task (id,(ce,args,focus)) -> (id,(ce,args,focus)) :: List.concat(List.map gt args)           *)
(*      | (Var _ | CONST _ | Axiom _) -> []                                                            *)
(*      | ApplyP (q,args) -> gt q @ List.concat(List.map gt args)                                      *)
(*      | (ApplyS (q,_) | LambdaS (_,q) | LambdaP (_,q)) -> gt q                                       *)
(*      | SUBST (_,_,_,q,r) -> gt q @ gt r                                                             *)
(*    in                                                                                               *)
(*      removeDuplicates(gt proof)                                                                     *)
(*                                                                                                     *)
(*                                                                                                     *)
(*  (* check for occurrence of a constant in a proof *)                                                *)
(*  let rec occurs (name:id) (p:proof) : bool =                                                        *)
(*    match p with                                                                                     *)
(*      CONST c -> c = name                                                                            *)
(*    | Task (_,(_,args,_)) -> List.exists (occurs name) args                                          *)
(*    | (Var _ | Axiom _) -> false                                                                     *)
(*    | ApplyP (q,args) -> occurs name q || List.exists (occurs name) args                             *)
(*    | (ApplyS (q,_) | LambdaS (_,q) | LambdaP (_,q)) -> occurs name q                                *)
(*    | SUBST (_,_,_,q,r) -> occurs name q || occurs name r                                            *)
(*                                                                                                     *)
(*  (* count occurrences of a constant in a proof *)                                                   *)
(*  let rec occurrences (name:id) (p:proof) : int =                                                    *)
(*    match p with                                                                                     *)
(*      CONST c -> if c = name then 1 else 0                                                           *)
(*    | Task (t,(_,args,_)) -> List.fold_right (fun x y -> x + y) (List.map (occurrences name) args) 0 *)
(*    | (Var _ | Axiom _) -> 0                                                                         *)
(*    | ApplyP (q,args) -> List.fold_right (fun x y -> x + y) (List.map (occurrences name) (q::args)) 0*)
(*    | (ApplyS (q,_) | LambdaS (_,q) | LambdaP (_,q)) -> occurrences name q                           *)
(*    | SUBST (_,_,_,q,r) -> (occurrences name q) + (occurrences name r)                               *)
(*                                                                                                     *)
    
  (* recursive substitution for a task variable *)
  let subst (subst_proof : proof) (taskname : id) (in_proof : proof) : proof =
    let rec replace (p : proof) =
      match p with
      | LambdaP (ids, proof) -> LambdaP (ids, replace proof)
      | LambdaS (ids, proof) -> LambdaS (ids, replace proof)
      | ApplyS (proof, subst) ->
          if Subst.size subst = 0 then replace proof
          else ApplyS (replace proof, subst)
      | ApplyP (proof, []) -> replace proof
      | ApplyP (proof, proofList) -> ApplyP (replace proof, List.map replace proofList)
      | Task n -> if n = taskname then subst_proof else p
      | _ -> p in
    replace in_proof

  (* substitute a proof for a cited theorem in a proof *)
  let subst_for_cited (subst_proof : proof) (theorem_name : id) (in_proof : proof) : proof =
    let rec replace (p : proof) =
      match p with
      | LambdaP (ids, proof) -> LambdaP (ids, replace proof)
      | LambdaS (ids, proof) -> LambdaS (ids, replace proof)
      | ApplyS (proof, subst) ->
          if Subst.size subst = 0 then replace proof
          else ApplyS (replace proof, subst)
      | ApplyP (proof, []) -> replace proof
      | ApplyP (proof, proofList) -> ApplyP (replace proof, List.map replace proofList)
      | Theorem n -> if n = theorem_name then subst_proof else p
      | _ -> p in
    replace in_proof

  (* substitute proofs for cited theorems in a proof *)
  (* input is a hashtable with cited thm name -> proof to substitute *)
  (* keys are in uppercase *)
  (* no consistency checking is done *)
  let subst_for_all_cited (h : (id, proof) Hashtbl.t) (in_proof : proof) : proof =
    let rec replace (p : proof) =
      match p with
      | LambdaP (ids, proof) -> LambdaP (ids, replace proof)
      | LambdaS (ids, proof) -> LambdaS (ids, replace proof)
      | ApplyS (proof, subst) ->
          if Subst.size subst = 0 then replace proof
          else ApplyS (replace proof, subst)
      | ApplyP (proof, []) -> replace proof
      | ApplyP (proof, proofList) -> ApplyP (replace proof, List.map replace proofList)
      | Theorem n -> (try Hashtbl.find h (String.uppercase n) with Not_found -> p)
      | _ -> p in
    replace in_proof

let find_in_lib (name : id) (state : state) : theorem =
  let uname = String.uppercase name in
  try List.find (fun {thm_name=n} -> uname = String.uppercase n) state
  with Not_found -> failwith ("Corrupt library - Missing theorem " ^ name)

(*  (* same, but lazy computation of substituted proof *)                                              *)
(*  let replaceTheoremLazy (name:id) (q:unit -> proof) (p:proof) : proof =                             *)
(*    let rec replace (p:proof) =                                                                      *)
(*      match p with                                                                                   *)
(*        CONST c -> if c = name then q() else p                                                       *)
(*      | Task (t,(ce,args,focus)) -> Task (t,(ce,List.map replace args,focus))                        *)
(*      | (Var _ | Axiom _) -> p                                                                       *)
(*      | ApplyP (u,[]) -> replace u                                                                   *)
(*      | ApplyP (u,args) -> ApplyP (replace u, List.map replace args)                                 *)
(*      | ApplyS (p,[]) -> replace p                                                                   *)
(*      | ApplyS (p,s) -> ApplyS (replace p,s)                                                         *)
(*(*      | APPLYF (p,[]) -> replace p                                                                 *)
(*      | APPLYF (p,s) -> APPLYF (replace p,s) *)                                                      *)
(*      | LambdaS (s,p) -> LambdaS (s,replace p)                                                       *)
(*      | LambdaP (s,p) -> LambdaP (s,replace p)                                                       *)
(*      | SUBST (c,e,d,p,q) -> SUBST (c,e,d,replace p,replace q)                                       *)
(*  in                                                                                                 *)
(*    replace p                                                                                        *)
(*                                                                                                     *)
(*                                                                                                     *)
(*(***********************************************                                                     *)
(* * alpha-reduction                                                                                   *)
(* **********************************************)                                                     *)
(*                                                                                                     *)
(*  (* substitution of a proof q for a cited theorem in a proof p *)                                   *)
(*  let replaceVar (name:id) (q:id) (p:proof) : proof =                                                *)
(*    let replaceString x y z  = if x = z then y else z in                                             *)
(*    let rec replace (p:proof) =                                                                      *)
(*      match p with                                                                                   *)
(*        Var c -> if c = name then Var(q) else p                                                      *)
(*      | Task (t,(ce,args,focus)) -> Task (t,(ce,List.map replace args,focus))                        *)
(*      | (CONST _ | Axiom _) -> p                                                                     *)
(*      | ApplyP (u,[]) -> replace u                                                                   *)
(*      | ApplyP (u,args) -> ApplyP (replace u, List.map replace args)                                 *)
(*      | ApplyS (p,[]) -> replace p                                                                   *)
(*      | ApplyS (p,s) -> ApplyS (replace p,s)                                                         *)
(*(*      | APPLYF (p,[]) -> replace p                                                                 *)
(*      | APPLYF (p,s) -> APPLYF (replace p,s) *)                                                      *)
(*      | LambdaS (s,p) -> LambdaS (s,replace p)                                                       *)
(*      | LambdaP (s,p) -> LambdaP (List.map (replaceString name q) s,replace p)                       *)
(*      | SUBST (c,e,d,p,q) -> SUBST (c,e,d,replace p,replace q)                                       *)
(*    in                                                                                               *)
(*      replace p                                                                                      *)
(*                                                                                                     *)
(*let rec getPs (p:proof) : id list =                                                                  *)
(*      match p with                                                                                   *)
(*        LambdaP (ids,_) -> ids                                                                       *)
(*      | LambdaS (_,prf) -> getPs(prf)                                                                *)
(*      | _ -> []                                                                                      *)
(*                                                                                                     *)
(*let alphaRedux(p:proof):proof =                                                                      *)
(*  let ps = getPs(p)                                                                                  *)
(*  and makereplace (proof,num) id = ((replaceVar id ("P"^string_of_int(num)) proof),num+1)            *)
(*in                                                                                                   *)
(*  fst(List.fold_left makereplace (p,0) ps)                                                           *)



(***********************************************
 * normalize
 ***********************************************)

(*  (* check consistency -- variables in a lambda-binding are the                                 *)
(*   * same as those in the substitution to which it is applied *)                                *)
(*  let lambdaSConsis (v : id list) (s : substitution) : bool =                                   *)
(*    List.length v = List.length s && List.for_all Option.isSome (List.map (fun x -> find x s) v)*)
(*                                                                                                *)
(*  let lambdaPConsis (v:id list) (p:proof list) : bool =                                         *)
(*    List.length v = List.length p                                                               *)
(*                                                                                                *)
(*  (* apply a substitution to a proof term *)                                                    *)
(*  let rec applySubstToProof (s:substitution) (p:proof) : proof =                                *)
(*    let rec apply (p:proof) =                                                                   *)
(*      match p with                                                                              *)
(*        (Var _ | CONST _ | Axiom _) -> p                                                        *)
(*      | Task (t,(ce,args,_)) ->                                                                 *)
(*          let ce = makeConstantCondEqn (Unify.applyToCondEqn s (makeVariableCondEqn ce))        *)
(*          in Task (t,(ce,List.map apply args,[]))                                               *)
(*      | ApplyP (u,[]) -> apply u                                                                *)
(*      | ApplyP (u,args) -> ApplyP (apply u, List.map apply args)                                *)
(*      | ApplyS (q,[]) -> apply q                                                                *)
(*      | ApplyS (q,t) -> ApplyS (apply q, Unify.compose t s)                                     *)
(*      | LambdaS (t,q) ->                                                                        *)
(*          let s = deleteAll t s (* don't subst in bound vars *)                                 *)
(*          in                                                                                    *)
(*            (match s with [] -> p                                                               *)
(*            | _ -> LambdaS (t,applySubstToProof s q))                                           *)
(*                                                                                                *)
(*      | LambdaP (t,q) -> LambdaP (t,apply q)                                                    *)
(*      | SUBST (c,e,d,q,r) -> SUBST (c,e,d,apply q,apply r)                                      *)
(*    in                                                                                          *)
(*      apply p                                                                                   *)
(*                                                                                                *)
(*                                                                                                *)
(*  (* substitute proofs for proof variables *)                                                   *)
(*  let subst_proofs (proofs:proof list) (vars:id list) (p:proof) : proof =                        *)
(*    let s = List.combine vars proofs in                                                         *)
(*    let rec apply (s:(id * proof) list) (p:proof) =                                             *)
(*      match p with                                                                              *)
(*        Var x -> (match lookup x s with Some y -> y | _ -> p)                                   *)
(*      | (CONST _ | Axiom _) -> p                                                                *)
(*      | Task (t,(ce,args,focus)) -> Task (t,(ce,List.map (apply s) args,focus))                 *)
(*      | ApplyP (u,[]) -> apply s u                                                              *)
(*      | ApplyP (u,args) -> ApplyP (apply s u, List.map (apply s) args)                          *)
(*      | ApplyS (q,[]) -> apply s q                                                              *)
(*      | ApplyS (q,t) -> ApplyS (apply s q,t)                                                    *)
(*      | LambdaS (t,q) -> LambdaS (t,apply s q)                                                  *)
(*      | LambdaP (t,q) ->                                                                        *)
(*          let s = deleteAll t s in  (* don't subst in bound vars *)                             *)
(*          (match s with                                                                         *)
(*            | [] -> p                                                                           *)
(*            | _ -> LambdaP (t,apply s q))                                                       *)
(*                                                                                                *)
(*      | SUBST (c,e,d,q,r) -> SUBST (c,e,d,apply s q,apply s r)                                  *)
(*    in                                                                                          *)
(*      apply s p                                                                                 *)
  

  (* reduce proof term to normal form *)
  let rec normalize (p : proof) : proof =
    failwith "Proof normalization not implemented"
    
(*    let rec existsRedux (p:proof) : bool =                                     *)
(*      match p with                                                             *)
(*        (Var _ | CONST _ | Axiom _) -> false                                   *)
(*      | Task (_,(_,args,_)) -> List.exists existsRedux args                    *)
(*      | (ApplyP (LambdaP _,_) | ApplyS (LambdaS _,_)) -> true                  *)
(*      | ApplyP (u,args) -> existsRedux u || List.exists existsRedux args       *)
(*      | (ApplyS (q,_) | LambdaS (_,q) | LambdaP (_,q)) -> existsRedux q        *)
(*      | SUBST (_,_,q,r) -> existsRedux q || existsRedux r                      *)
(*    in                                                                         *)
(*                                                                               *)
(*    let rec reduceOne (p:proof) : proof =                                      *)
(*      try                                                                      *)
(*      match p with                                                             *)
(*        (Var _ | CONST _ | Axiom _) -> p                                       *)
(*      | Task (t,(ce,args,focus)) -> Task (t,(ce,List.map reduceOne args,focus))*)
(*      | ApplyP (LambdaP (t,q),args) ->                                         *)
(*          if (lambdaPConsis t args) then subst_proofs args t q                  *)
(*          else failwith "system error: improper lambda binding"                *)
(*      | ApplyP (u,[]) -> reduceOne u                                           *)
(*      | ApplyP (u,args) -> ApplyP (reduceOne u, List.map reduceOne args)       *)
(*      | ApplyS (LambdaS (t,q),s) ->                                            *)
(*          if (lambdaSConsis t s) then applySubstToProof s q                    *)
(*          else failwith "system error: improper lambda binding"                *)
(*      | ApplyS (q,[]) -> reduceOne q                                           *)
(*      | ApplyS (q,t) -> ApplyS (reduceOne q,t)                                 *)
(*      | LambdaS (t,q) -> LambdaS (t,reduceOne q)                               *)
(*      | LambdaP (t,q) -> LambdaP (t,reduceOne q)                               *)
(*      | SUBST (c,e,q,r) -> SUBST (c,e,reduceOne q,reduceOne r)                 *)
(*      with Failure x -> (print_endline x; p)                                   *)
(*    in                                                                         *)
(*      if existsRedux p then normalize (reduceOne p)                            *)
(*      else p                                                                   *)
  

(***********************************************
 * create a new proof from a given task
 ***********************************************)

  (* create a new proof term for a given task *)
  let to_proof ({task_name=name; premises=premises} : task) (ce : formula) : proof =
    let s = vars_in_formula ce in
    LambdaS (s, LambdaP (List.map (fun p -> p.premise_name) premises, Task name))
    
(***********************************************
 * cite
 ***********************************************)

  let cite (name : id) (s : substitution) (new_tasks : task list) (premises : premise list) : proof =
    let task_names = List.map (fun t -> Task t.task_name) new_tasks in
    ApplyP (ApplyS (Theorem name, s), task_names)

(*                                                                                                                 *)
(*                                                                                                                 *)
(*                                                                                                                 *)
(*(***********************************************                                                                 *)
(* * contains                                                                                                      *)
(* **********************************************)                                                                 *)
(*let rec contains((proof,offLimits) : proof * string list): bool =                                                *)
(*  match proof with                                                                                               *)
(*    (LambdaS(_,p1) | LambdaP(_,p1)) -> contains(p1,offLimits)                                                    *)
(*  | ApplyS(p1,_) -> contains(p1,offLimits)                                                                       *)
(*  | ApplyP(p1,pl) -> contains(p1,offLimits) || (List.fold_left (fun l p -> contains(p,offLimits) || l) false pl) *)
(*  | Var(id) -> List.mem id offLimits                                                                             *)
(*  | CONST(id) -> List.mem id offLimits                                                                           *)
(*  | Axiom(_) -> false                                                                                            *)
(*  | SUBST(_,_,_,p1,p2) -> contains(p1,offLimits) || contains(p2,offLimits)                                       *)
(*  | Task(_,(_,pl,_)) -> List.exists (fun p -> contains(p,offLimits)) pl                                          *)
(*                                                                                                                 *)
    
(* find all cited theorems in a proof *)
let cited (proof : proof) : id HashSet.t =
  let h = HashSet.make() in
  let rec cited proof : unit =
	  match proof with

	  | LambdaP (x, p) -> cited p
	  | LambdaS (x, p) -> cited p
	  | ApplyS (p, s) -> cited p
	  | ApplyP (p, plist) -> cited p; List.iter cited plist
	  | Theorem thm -> HashSet.add h thm
	  | _ -> () in
  cited proof; h
  
(* Find dependencies of a named theorem.  Checks for circularities *)
let dependencies (theorem : theorem) (state : state) : id HashSet.t =
  let on_stack = HashSet.make() in
  let visited = HashSet.make() in
  let rec dependencies (theorem : theorem) : unit =
    if HashSet.mem on_stack theorem.thm_name
    then failwith "Corrupt library - circular dependency detected!"
    else if HashSet.mem visited theorem.thm_name then ()
    else let h = cited theorem.proof in
    HashSet.add on_stack theorem.thm_name;
    HashSet.iter (fun n -> dependencies (find_in_lib n state)) h;
    HashSet.remove on_stack theorem.thm_name;
    HashSet.add visited theorem.thm_name in
  dependencies theorem; visited
  
(***********************************************
 * rename
 ***********************************************)


  let rename (old_name : id) (new_name : id) (thm : theorem) : theorem = 
    let new_proof = subst_for_cited (Theorem new_name) old_name thm.proof in
    { thm_name=thm.thm_name; theorem=thm.theorem; tasks=thm.tasks; proof=new_proof }
    
(*(***********************************************                                                                 *)
(* * forget a theorem                                                                                              *)
(* ***********************************************)                                                                *)
(*                                                                                                                 *)
(*  (* substitute proof terms for constant name of theorem being forgotten *)                                      *)
(*  (* need new unique task ids in each copy *)                                                                    *)
(*  let forget ((name, subst_proof, in_proof) : id * proof * proof) : proof =                                        *)
(*    let rec renameTasks (p:proof) : proof =                                                                      *)
(*      match p with                                                                                               *)
(*        Task (_,(ce,args,focus)) ->                                                                              *)
(*          Task (newTaskId(),(ce,List.map renameTasks args,focus))                                                *)
(*      | (Var _ | CONST _ | Axiom _) -> p                                                                         *)
(*      | ApplyP (q,[]) -> renameTasks q                                                                           *)
(*      | ApplyP (q,args) -> ApplyP (renameTasks q, List.map renameTasks args)                                     *)
(*      | ApplyS (q,[]) -> renameTasks q                                                                           *)
(*      | ApplyS (q,s) -> ApplyS (renameTasks q,s)                                                                 *)
(*(*      | APPLYF (q,[]) -> renameTasks q                                                                         *)
(*      | APPLYF (q,s) -> APPLYF (renameTasks q,s) *)                                                              *)
(*      | LambdaS (s,q) -> LambdaS (s,renameTasks q)                                                               *)
(*      | LambdaP (s,q) -> LambdaP (s,renameTasks q)                                                               *)
(*      | SUBST (c,e,d,q,r) -> SUBST (c,e,d,renameTasks q,renameTasks r)                                           *)
(*  in                                                                                                             *)
(*    replaceTheoremLazy name (fun _ -> renameTasks subst_proof) in_proof                                            *)
  