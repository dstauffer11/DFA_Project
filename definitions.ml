
open Util

(* regular expression definition *)
type regex = 
	| Star of regex
	| Or of regex * regex
	| Concat of regex * regex
	| Epsilon
  | Emptyset
	| Character of char

(* DFA and NFA modules *)
module type FA = sig
	type 'a t
	val build : 'a list -> char list -> ('a * char * 'a) list -> 'a -> 'a list -> ('a -> 'a -> bool) -> 'a t
	val deconstruct : 'a t -> 'a list * char list * ('a * char * 'a) list * 'a * 'a list * ('a -> 'a -> bool)
end

module FA : FA = struct
  type 'a t = {
    mutable states : 'a list;
    mutable alphabet : char list;
    mutable delta : ('a * char * 'a) list;
    mutable start_state : 'a;
    mutable accept_states : 'a list;
    mutable compare_states : 'a -> 'a -> bool }
    
  let build = fun states alphabet delta start_state accept_states comparator -> {
          states = states;
          alphabet = alphabet;
          delta = delta;
          start_state = start_state;
          accept_states = accept_states;
          compare_states = comparator }
          
  (* let create _ = build [] [] () [] (fun _ _ -> false) *)
  
  let deconstruct = fun fa -> (
          fa.states,
          fa.alphabet,
          fa.delta,
          fa.start_state,
          fa.accept_states,
          fa.compare_states)
end

module NFA = (FA : FA)
module DFA = (FA : FA)


(* Get the epsilon-closure of a set of states using the given delta        *)
(* relation.                                                               *)
let rec closure (states : 'a list) (delta : ('a * char * 'a) list) (comparator : 'a -> 'a -> bool) : ('a list) =
  let states' = List.fold_left (fun acc state -> List.fold_left (fun acc2 delt -> let (p, s, q) = delt in
                    if (comparator p state) && s = ' ' then q :: acc2 else acc2) acc delta) states states in
  let states' = remove_dups comparator states' in
  if List.length states = List.length states' then states' else closure states' delta comparator

(* uses the given delta relation to transition from a set of states to the *)
(* set of reachable states with the given input                            *)
let goto (states : 'a list) (delta : ('a * char * 'a) list) (input : char) (comparator : 'a -> 'a -> bool) : ('a list) =
  let new_states = List.fold_left (fun acc state ->
            List.fold_left (fun acc2 delt -> let (p, s, q) = delt in
                    if (comparator p state) && s = input then q :: acc2 else acc2) acc delta) [] states in
  let dups_list = (closure new_states delta comparator) in
  remove_dups comparator dups_list




let rec basic_simplify_helper (regex : regex) : (bool * regex) = 
  match regex with
  | Star Emptyset -> (true, Epsilon)
  | Star Epsilon -> (true, Epsilon)
  | Star reg -> let (simplified,reg') = basic_simplify_helper reg in if simplified then (true,Star reg') else (false,Star reg)
  | Or (Emptyset,reg) 
  | Or (reg,Emptyset) -> (true, snd (basic_simplify_helper reg))
  | Concat (Emptyset,_)
  | Concat (_,Emptyset) -> (true,Emptyset)
  | Concat (Epsilon,reg)
  | Concat (reg,Epsilon) -> (true,reg)
  | Or (reg1,reg2) -> let (s1,reg1') = basic_simplify_helper reg1 in let (s2,reg2') = basic_simplify_helper reg2 in
    if s1 || s2 then (true,Or (reg1',reg2')) else (false,regex)
  | Concat (reg1,reg2) -> let (s1,reg1') = basic_simplify_helper reg1 in let (s2,reg2') = basic_simplify_helper reg2 in
    if s1 || s2 then (true,Concat (reg1',reg2')) else (false,regex)
  | Character _
  | Epsilon
  | Emptyset -> (false,regex)

let rec basic_simplify (regex : regex) : regex =
  let (simplifed,regex') = basic_simplify_helper regex in
  if simplifed then basic_simplify regex' else regex'


  
