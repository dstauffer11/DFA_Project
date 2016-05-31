(* Module to create a minimial DFA from a DFA *)

open Util
open Definitions
open NFAtoDFA


exception NoDeltaRelationFound


(* find the set of reachable states in dfa from the current set of states found_states *)
let rec find_reachable_states (dfa : 'a DFA.t) (found_states : 'a list) : 'a list = 
	let (_,alphabet,delta,_,_,comparator) = DFA.deconstruct dfa in
	let new_states = flatten_once (List.fold_left (fun acc a -> (Definitions.goto found_states delta a comparator)::acc) [] alphabet) in 
		let total_states = Util.union comparator new_states found_states in
			if List.length total_states == List.length found_states then total_states
				else find_reachable_states dfa total_states

(* create a new dfa from the given dfa using only the reachable_states *)
let reachable_dfa (dfa : 'a DFA.t) (reachable_states : 'a list) : 'a DFA.t = 
	let (states, alphabet, delta, start_state, accepts, comparator) = DFA.deconstruct dfa in
	let new_accepts = List.fold_left (fun acc el -> if (Util.contains comparator reachable_states el) then el::acc else acc) [] accepts in
	let new_delta = List.fold_left (fun acc (s1,c,s2) -> if contains comparator reachable_states s1 then (s1,c,s2)::acc else acc) [] delta in
	let new_start_state = start_state in
	DFA.build reachable_states alphabet new_delta new_start_state new_accepts comparator

(* find the set of states which transition to the reachable_states set with delta and input character a *)
let generate_transition_to_states (reachable_states : 'a list) (delta : ('a * char * 'a) list) (a : 'a list) (input : char) (comparator : 'a -> 'a -> bool) : 'a list = 
	List.fold_left (fun acc state ->
		if Util.intersect comparator (Definitions.goto [state] delta input comparator) a <> [] then state::acc else acc) [] reachable_states

(* Manipulate the set w given x and y as described in https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft.27s_algorithm *)
let manipulate_w (x : 'a list) (y : 'a list) (w : 'a list list) (comparator : 'a -> 'a -> bool) : 'a list list = 
	if Util.set_in comparator w y then let w' = Util.remove_set comparator w y in (Util.cept comparator y x)::(Util.intersect comparator y x)::w'
	else (if List.length (Util.intersect comparator y x) <= List.length (Util.cept comparator y x) then (Util.intersect comparator y x)::w 
		else (Util.cept comparator y x)::w)

(* find the set containing element el in a set of sets *)
let set_of (set_list : 'a list list) (el : 'a) (comparator : 'a -> 'a -> bool) : 'a list = 
	List.fold_left (fun set_of_el_acc set -> if Util.contains comparator set el then set else set_of_el_acc) [] set_list

(* find the state a set of states transition to under input a with delta
* precondition: all elements in lst under input a with delta go to the same state *)
let delta_goto (delta : ('a * char * 'a) list) (lst : 'a list) (a : char) (comparator : 'a -> 'a -> bool) : 'a =
	match lst with
	| [] -> raise NoDeltaRelationFound
	| h::t -> let result = List.fold_left (fun acc (p,s,q) -> if comparator p h && compare a s == 0 then Some q else acc) None delta in 
		(match result with 
		| None -> raise NoDeltaRelationFound 
		| Some x -> x)

(* find the state that delta goes to under input a from state *)
let rec delta_list_goto (delta : (('a list) * char * ('a list)) list) (state : 'a list) (a : char) (comparator : 'a -> 'a -> bool) : 'a list =
	match delta with
	| [] -> []
	| (p,s,q)::t -> if lists_equal comparator p state && compare a s == 0 then q else delta_list_goto t state a comparator

(* gien a partition of the states, create a new list delta from the old delta that treates all states in an element of the partition as equivalent *)
let create_delta (dfa : 'a DFA.t) (p : 'a list list) : ('a list * char * 'a list) list = 
	let (_, alphabet, delta, _, _, comparator) = DFA.deconstruct dfa in
	List.fold_left (fun delta' partition -> List.fold_left (fun delta' a -> 
		(partition,a,(set_of p (delta_goto delta partition a comparator) comparator))::delta') delta' alphabet) [] p

(* build a dfa from the given dfa given a partition p of the dfa's states into equivalence classes *)
let build_dfa_of_dfa (dfa : 'a DFA.t) (p : 'a list list) : ('a list) DFA.t = 
	let (states, alphabet, delta, start_state, accept_states, comparator) = DFA.deconstruct dfa in
	let new_start_state = List.fold_left (fun start_state_temp partition -> if Util.contains comparator partition start_state then partition else start_state_temp) [] p in
	let new_delta = create_delta dfa p in
	let new_accept_states = List.fold_left (fun acc partition -> if List.length (Util.intersect comparator accept_states partition) > 0 then partition::acc else acc) [] p 
		in DFA.build p alphabet new_delta new_start_state new_accept_states (lists_equal comparator)

(* partition the states into equivalence classes 
* preconditions: p: the current partition
* w: the sets of "distinguishers"
* reachable_states: a list of all states in dfa which are reachable from the start state *)
let rec partition_states_helper (dfa : 'a DFA.t) (p : 'a list list) (w : 'a list list) (reachable_states : 'a list) : ('a list) DFA.t = 
	let (states, alphabet, delta, start_state, accept_states, comparator) = DFA.deconstruct dfa in
	match w with
	| [] -> build_dfa_of_dfa dfa p
	| a::t -> let (p_new,w_new) = (List.fold_left (fun (p,w) input -> let x = generate_transition_to_states reachable_states delta a input comparator in
					(List.fold_left (fun (p_acc,w_acc) y -> if (List.length (Util.cept comparator y x) > 0 && List.length (Util.intersect comparator y x) > 0)
						then let p_acc' = (Util.cept comparator y x)::(Util.intersect comparator y x)::p_acc and w_acc' = manipulate_w x y t comparator 
							in (p_acc',w_acc')
						else (y::p_acc,w_acc))
					([],w) p))
				(p,t) alphabet)
			in partition_states_helper dfa p_new w_new reachable_states

(* start the partitioning process by creating the p and w set where:
* p = {F,Q\F} and
* w = {F}
* where F = accept states, Q = all reachable states of the dfa *)
let partition_states (dfa : 'a DFA.t) (reachable_states : 'a list) : ('a list) DFA.t = 
	let (_, _, _, _, accept_states, comparator) = DFA.deconstruct dfa in
	match (accept_states,Util.cept comparator reachable_states accept_states) with 
	| (h1::t1,h2::t2) -> let p = [accept_states;Util.cept comparator reachable_states accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| (h1::t1,[]) -> let p = [accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],h2::t2) -> let p = [Util.cept comparator reachable_states accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],[]) -> let p = [] in let w = [] in
			partition_states_helper dfa p w reachable_states

(* minimize a given dfa *)
let minimize (dfa : 'a DFA.t) : ('a list) DFA.t = 
	let (states, alphabet, delt, start_state, accept_states, comparator) = DFA.deconstruct dfa in
	let reachable_states = find_reachable_states dfa [start_state] in
	let new_dfa = reachable_dfa dfa reachable_states in
	partition_states new_dfa reachable_states

(* creates an assignment of states between two dfas. If a conflict in assignments reached (an accept state and a non-accept paired), returns true and the current assignment
* otherwise returns false and the total assignment of equivalent states between the two dfas *)
let rec dfa_equivalence_helper (delta1 : ('a list * char * 'a list) list) (delta2 : ('a list * char * 'a list) list) (state_assignment : ('a list * 'a list) list) 
(q1 : 'a list) (q2 : 'a list) (comparator : 'a -> 'a -> bool) (alphabet : char list) : bool * (('a list * 'a list) list) =
	if (List.fold_left (fun acc (p1,p2) -> 
		if lists_equal comparator q1 p1 && not (lists_equal comparator q2 p2) then true else acc) false state_assignment) then (false,state_assignment)
	else let q1_found = List.fold_left (fun acc (p1,p2) -> if (lists_equal comparator q1 p1) then true else acc) false state_assignment in
		(if q1_found then (true,state_assignment) else let state_assignment = (q1,q2)::state_assignment in 
			List.fold_left (fun (same,current_state_assignment) a -> let (same_sub,sa') = dfa_equivalence_helper delta1 delta2 current_state_assignment 
				(delta_list_goto delta1 q1 a comparator) (delta_list_goto delta2 q2 a comparator) comparator alphabet in 
				if same_sub then (same,sa') else (false, sa')) 
			(true,state_assignment) alphabet)

(* test whether two minimal dfas are equivalent
* requires the two dfa to share the same state comparison function *)
let minimal_dfa_equivalence (dfa1 : 'a list DFA.t) (dfa2 : 'a list DFA.t) (comparator : 'a -> 'a -> bool) : bool = 
  let (states1, alphabet1, delta1, s1, accept_states1, _) = DFA.deconstruct dfa1 and (states2, alphabet2, delta2, s2, accept_states2, _) = DFA.deconstruct dfa2 in
  if not (lists_equal comparator_of_compare alphabet1 alphabet2) then false 
  else 
	(let (assignment_works,assignment) = dfa_equivalence_helper delta1 delta2 [] s1 s2 comparator alphabet1 in
	(List.fold_left (fun acc (q1,q2) -> let c1 = (Util.contains (lists_equal comparator) accept_states1 q1) 
										and c2 = (Util.contains (lists_equal comparator) accept_states2 q2) in 
		if (c1 && c2) || (not c1 && not c2) then acc else false) true assignment) && assignment_works)

(* requires the two dfa to share the same state comparison function *)
let equivalence_test_dfa (dfa1 : 'a DFA.t) (dfa2 : 'a DFA.t) : bool = 
	let (_,_,_,_,_,comparator) = DFA.deconstruct dfa1 in
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2 comparator

(* requires the two dfa to share the same state comparison function *)
let equivalence_test_nfa (nfa1 : 'a NFA.t) (nfa2 : 'a NFA.t) : bool = 
	let (_,_,_,_,_,comparator) = NFA.deconstruct nfa1 in
	let dfa1 = NFAtoDFA.convert_NFA_to_DFA nfa1 and dfa2 = NFAtoDFA.convert_NFA_to_DFA nfa2 in
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2 (lists_equal comparator)

