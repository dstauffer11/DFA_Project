
open Util

(***********************************************
 * syntax
 ***********************************************)

  type id = string

  (* character encoding *)
  let utf8 = ref false

  type term =
    Tst of id
  | Act of id
  | Plus of term list
  | Times of term list
  | Not of term
  | Star of term
  | Zero
  | One
  
  type substitution = (id, term) Subst.t
  type equation = Eq of term * term | Le of term * term
  type formula = equation list
  
  type attribute = String_attribute of string * string | Int_attribute of string * int
  type element = string * attribute list * string

(***********************************************
 * output
 ***********************************************)

  (* higher precedence binds tighter *)
  let out_precedence (t : term) : int =
    match t with
      Plus _ -> 0
    | Times _ -> 1
    | Not _ -> 2
    | Star _ -> 3
    | _ -> 4 (* variables and constants *)

  let assoc_to_string (op : string) (id : string) (s : string list) : string =
    match s with
      [] -> id
    | _ -> String.concat op s

  let rec term_to_string (t : term) : string =
    (* parenthesize as dictated by surrounding precedence *)
    let protect (x : term) : string =
      let s = term_to_string x in
      if out_precedence t <= out_precedence x then s else "(" ^ s ^ ")" in
    match t with
      Tst x -> x
    | Act x -> x
    | Plus x -> assoc_to_string " + " "0" (List.map protect x)
    | Times x -> assoc_to_string ";" "1" (List.map protect x)
    | Not x -> (if !utf8 then "¬" else "~") ^ (protect x)
    | Star x -> (protect x) ^ "*"
    | Zero -> "0"
    | One -> "1"

  let op_to_string = function (Eq _) -> "="
                            | (Le _) -> if !utf8 then "≤" else "<"

  let eqn_to_string (e : equation) =
    match e with (Eq (s,t) | Le (s,t)) ->
    term_to_string s ^ " " ^ op_to_string e ^ " " ^ term_to_string t

  let formula_to_string (ce : formula) =
    String.concat (if !utf8 then " ⇒ " else " -> ") (List.map eqn_to_string ce)

(***********************************************
 * utilities
 ***********************************************)

  let rec is_test (t : term) : bool =
    match t with
      Tst _ -> true
    | Act _ -> false
    | (Plus x | Times x) -> List.for_all is_test x
    | Not x -> is_test x || failwith "May not have action under a negation"
    | Star x -> is_test x
    | (Zero | One) -> true

  let rec vars_in_term (t : term) : id list =
    match t with
      (Tst x | Act x) -> [x]
    | (Plus x | Times x) -> List.concat (List.map vars_in_term x)
    | (Not x | Star x) -> vars_in_term x
    | _ -> []

  let vars_in_equation (e : equation) : id list =
    match e with
      | Eq (s, t) -> vars_in_term s @ vars_in_term t
      | Le (s, t) -> vars_in_term s @ vars_in_term t

  let vars_in_formula (ce : formula) : id list =
    Util.removeDuplicates (List.concat (List.map vars_in_equation ce))

  let conclusion (ce : formula) : equation =
    match List.rev ce with
      | x :: t -> x
      | [] -> failwith "Empty formula"

  let premises (ce : formula) : equation list =
    match List.rev ce with
      | x :: t -> List.rev t
      | [] -> failwith "Empty formula"

(*  let substVariables (s : substitution) : id list =               *)
(*    Subst.fold (fun id term list -> id :: list) s []              *)

  (* flatten terms *)
  let rec flatten (t : term) : term =
    match t with
      Plus x ->
        let y = List.map flatten x in
        let z = List.concat (List.map (fun u -> match u with Plus v -> v | _ -> [u]) y) in
        (match z with [] -> Zero | [x] -> x | _ -> Plus z)
    | Times x ->
        let y = List.map flatten x in
        let z = List.concat (List.map (fun u -> match u with Times v -> v | _ -> [u]) y) in
        (match z with [] -> One | [x] -> x | _ -> Times z)
    | Not x -> Not (flatten x)
    | Star x -> Star (flatten x)
    | _ -> t

  let rec flatten_eqn (e : equation) : equation =
    match e with
    | Eq (s, t) -> Eq (flatten s, flatten t)
    | Le (s, t) -> Le (flatten s, flatten t)

  let rec flatten_formula (ce : formula) : formula =
    List.map flatten_eqn ce
		
    (* calculate discrepancy between # of utf8 characters and length in bytes *)
    let utf8_discrepancy (s : string) : int =
	    let rec count (c : int) : int =
	        if c land 64 != 0 then 1 + count (c lsl 1) else 0 in
	    let a = ref 0 in
	    let f (ch : char) : unit =
	        let c = Char.code ch in
	        if c land 128 != 0 then a := !a + count c in
	    String.iter f s; !a
    
    (* pad a utf8 string with blanks *)
    let utf8_pad (s : string) : string =
      let pad = 36 - (String.length s) + (utf8_discrepancy s) in
      if pad <= 0 then s ^ " "
      else s ^ (String.make pad ' ')
    
(***********************************************
 * simplify
 ***********************************************)

  (* convert empty sums and products to 0 and 1, resp,
   * and sums and products of one element to that element,
   * combine adjacent sums and products *)
  let rec simplifyLite (t : term) : term =
    match t with
      Plus [] -> Zero
    | Plus [x] -> simplifyLite x
    | Plus y ->
        let y' = List.map simplifyLite y in
        let f = function (Plus z) -> fun zz -> z @ zz
                       | x -> fun zz -> x :: zz in
        Plus (List.fold_right f y' [])
    | Times [] -> One
    | Times [t] -> simplifyLite t
    | Times y ->
        let y' = List.map simplifyLite y in
        let f = function (Times z) -> fun zz -> z @ zz
                       | x -> fun zz -> x :: zz in
        Times (List.fold_right f y' [])
    | Not x -> Not (simplifyLite x)
    | Star x -> Star (simplifyLite x)
    | _ -> t

  let rec simplify (t : term) : term =
    match t with
      Plus x ->
        let x1 = List.map simplify x in
        let x2 = List.filter (function Zero -> false | _ -> true) x1 in
        let x3 = Util.removeDuplicates x2 in
        simplifyLite (Plus x3)
    | Times x ->
        let x1 = List.map simplify x in
        let x2 = List.filter (function One -> false | _ -> true) x1 in
        let x3 = if List.mem Zero x2 then [] else x2 in
        simplifyLite (Times x3)
    | Not (Not x) -> simplify x
    | Not x -> Not (simplify x)
    | Star (Star x) -> simplify (Star x)
    | Star x ->
        let x' = simplify x
        in if is_test x' then One else Star x'
    | _ -> t

