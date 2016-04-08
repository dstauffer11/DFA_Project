(* matching and substitution *)

open Ast
open Util

let no_match n = failwith ("Premise and conclusion do not match [" ^ string_of_int n ^ "]")

let subst_to_string (s : substitution) : string =
  let f var value list = (var ^ "=" ^ (term_to_string value)) :: list in
  "[" ^ String.concat ", " (Subst.fold f s []) ^ "]"
  
  (* print a substitution *)
  let print_subst (s : substitution) =
     print_endline "[ ";
     Subst.iter (fun x t -> print_endline (x ^ "=" ^ term_to_string t ^ " ")) s;
     print_endline "]"

let rec remove_duplicates (s : substitution list) : substitution list =
  match s with
    | [] -> []
    | x :: t -> if List.exists (Subst.equal x) t then t else s

(* combine 2 lists of substitutions *)
let rec combine_substs (ss : substitution list) (tt : substitution list) : substitution list =
  let a = List.map (fun x ->
    List.map (fun y ->
      try [Subst.consis x y]
      with Failure _ -> []) tt) ss in
  remove_duplicates (List.concat (List.concat a))
  
(* apply a substitution operator to a term *)
(* substitutions are applied simultaneously, not sequentially *)
let subst_in_term (s : substitution) (t : term) : term =
  let rec subst (allow_actions : bool) (t : term) : term =
    match t with
    | Tst y ->
        (try
          let t = Subst.lookup s y in
          if allow_actions || is_test t then t
          else failwith "May not substitute an action expression under a negation"
        with Not_found -> t)
    | Act y -> 
        if allow_actions
        then try
          Subst.lookup s y
        with Not_found -> t
        else failwith "Should not have action variables under a negation"
    | Plus y -> Plus (List.map (subst allow_actions) y)
    | Times y -> Times (List.map (subst allow_actions) y)
    | Not y -> Not (subst false y)
    | Star y -> Star (subst allow_actions y)
    | _ -> t in
  flatten (subst true t)

(* apply a substitution s in an equation e *)
let subst_in_equation (s : substitution) (e : equation) : equation =
  match e with
    | Eq (u, v) -> Eq (subst_in_term s u, subst_in_term s v)
    | Le (u, v) -> Le (subst_in_term s u, subst_in_term s v)

(* apply a substitution s in a formula ce *)
let subst_in_formula (s : substitution) (ce : formula) : formula =
  List.map (subst_in_equation s) ce
  
(* create an empty substitution *)
let empty_subst () : substitution = Subst.make ()
  
(* create substitution with one element *)
let create_subst (x : id) (s : term) : substitution =
  let h = empty_subst() in
  let _ = Subst.add h x s in
  h
  
(* substitute term s for all occurrences of variable x in term t *)
let subst_one_in_term (s : term) (x : id) (t : term) : term =
  subst_in_term (create_subst x s) t

(* substitute term s for all free occurrences of x in an equation e *)
let subst_one_in_equation (s : term) (x : id) (e : equation) : equation =
  subst_in_equation (create_subst x s) e

(* substitute term s for all free occurrences of x in a formula ce *)
let subst_one_in_formula (s : term) (x : id) (ce : formula) : formula =
  subst_in_formula (create_subst x s) ce
  
(* associate elements of a list of length m into n
 * nonmempty sublists, n <= m, in all possible ways *)
  let rec associate (s : 'a list) (n : int) : 'a list list list =
    if n = 0 then [] else
    if List.length s < n then [] else
    if n = 1 then [[s]] else
    match s with
    | [] -> []
    | x :: t ->
        let u = List.map (fun u -> [x] :: u) (associate t (n - 1)) in
        let v = List.map (fun u -> (x :: List.hd u) :: List.tl u) (associate t n) in
        u @ v

  (* match a term with variables s to a ground term t
   * returns a list of possible substitutions -- no most general
   * unifier because of associativity of Plus and Times *)
  let rec match_term (s : term) (t : term) : substitution list =
    match s, t with
    | (Plus sc, Plus tc) ->
        let s1 = associate tc (List.length sc) in
        let s2 = List.map (fun y -> List.map (fun x -> flatten (Plus x)) y) s1 in
        let s3 = List.map (fun z -> match_terms sc z) s2 in
          remove_duplicates (List.concat s3)
    | (Times sc, Times tc) ->
        let s1 = associate tc (List.length sc) in
        let s2 = List.map (fun y -> List.map (fun x -> flatten (Times x)) y) s1 in
        let s3 = List.map (fun z -> match_terms sc z) s2 in
          remove_duplicates (List.concat s3)
    | (Not sc, Not tc) -> match_term sc tc
    | (Star sc, Star tc) -> match_term sc tc
    | (Tst x, _) -> if is_test t then [create_subst x t] else []
    | (Act x, _) -> [create_subst x t]
    | ((Zero, Zero) | (One, One)) -> [empty_subst()]
    | _ -> []

  (* match a list of pairs *)
  and match_terms (ss : term list) (tt : term list) : substitution list =
    assert (List.length ss = List.length tt);
    let a = List.map2 match_term ss tt in
    List.fold_left combine_substs ([empty_subst()]) a

  (* match an equation *)
  (* match both sides simultaneously *)
  let match_equation (e1 : equation) (e2 : equation) : substitution list =
    match e1, e2 with
      | ((Eq (s1, t1), Eq (s2, t2)) | (Le (s1, t1), Le (s2, t2))) ->
          match_terms [s1; t1] [s2; t2]
      | _ -> []


