(* Module to create a minimial DFA from a DFA *)

module MinimizeDFA = struct

open Definitions
open Shared
open NFAtoDFA


exception NoDeltaRelationFound

let rec find_reachable_states (dfa : 'a DFA.dfa) (found_states : 'a list) : 'a list = 
	let (_,delta,_,_,comparator) = DFA.deconstruct_dfa dfa in
	let new_states = Shared.flatten_once (List.fold_left (fun acc a -> (Shared.goto found_states delta a comparator)::acc) [] Shared.alphabet) in 
		let total_states = Shared.union new_states found_states comparator in
			if List.length total_states == List.length found_states then total_states
				else find_reachable_states dfa total_states


let reachable_dfa (dfa : 'a DFA.dfa) (reachable_states : 'a list) : 'a DFA.dfa = 
	let (states, delta, start_state, accepts, comparator) = DFA.deconstruct_dfa dfa in
	let new_accepts = List.fold_left (fun acc el -> if (Shared.contains el reachable_states comparator) then el::acc else acc) [] accepts in
	let new_delta = List.fold_left (fun acc (s1,c,s2) -> if Shared.contains s1 reachable_states comparator then (s1,c,s2)::acc else acc) [] delta in
	let new_start_state = start_state in
	DFA.build_dfa reachable_states new_delta new_start_state new_accepts comparator





let generate_transition_to_states (reachable_states : 'a list) (delta : ('a * char * 'a) list) (a : 'a list) (comparator : 'a -> 'a -> bool) : 'a list = 
	List.fold_left (fun acc state -> List.fold_left (fun acc2 s -> 
		if Shared.intersect (Shared.goto [state] delta s comparator) a comparator <> [] then state::acc2 else acc2) acc Shared.alphabet) [] reachable_states



let manipulate_w (x : 'a list) (y : 'a list) (p : 'a list list) (w : 'a list list) (comparator : 'a -> 'a -> bool) : 'a list list = 
	if Shared.set_in w y comparator then let w' = Shared.remove_set w y comparator in (Shared.cept y x comparator)::(Shared.intersect y x comparator)::w'
	else (if List.length (Shared.intersect y x comparator) <= List.length (Shared.cept y x comparator) then (Shared.intersect y x comparator)::w 
		else (Shared.cept y x comparator)::w)


let set_of (set : 'a list list) (el : 'a) (comparator : 'a -> 'a -> bool) : 'a list = 
	List.fold_left (fun set_of_el set_el -> if Shared.contains el set_el comparator then set_el else set_of_el) [] set

let delta_goto (delta : ('a * char * 'a) list) (lst : 'a list) (a : char) (comparator : 'a -> 'a -> bool) : 'a =
	match lst with
	| [] -> raise NoDeltaRelationFound
	| h::t -> let result = List.fold_left (fun acc (p,s,q) -> if comparator p h then Some q else acc) None delta in 
		(match result with 
		| None -> raise NoDeltaRelationFound 
		| Some x -> x)

let rec delta_list_goto (delta : (('a list) * char * ('a list)) list) (state : 'a list) (a : char) (comparator : 'a -> 'a -> bool) : 'a list =
	match delta with
	| [] -> []
	| (p,s,q)::t -> if Shared.lists_equal p state comparator && compare a s == 0 then q else delta_list_goto t state a comparator



let create_delta (dfa : 'a DFA.dfa) (p : 'a list list) : ('a list * char * 'a list) list = 
	let (_, delta, _, _, comparator) = DFA.deconstruct_dfa dfa in
	List.fold_left (fun delta' partition -> List.fold_left (fun delta' a -> 
		(partition,a,(set_of p (delta_goto delta partition a comparator) comparator))::delta') delta' Shared.alphabet) [] p


let build_dfa_of_dfa (dfa : 'a DFA.dfa) (p : 'a list list) : ('a list) DFA.dfa = 
	let (states, delta, start_state, accept_states, comparator) = DFA.deconstruct_dfa dfa in
	let new_start_state = List.fold_left (fun start_state_temp partition -> if Shared.contains start_state partition comparator then partition else start_state_temp) [] p in
	let new_delta = create_delta dfa p in
	let new_accept_states = List.fold_left (fun acc partition -> if Shared.set_in [accept_states] partition comparator then partition::acc else acc) [] p in
	DFA.build_dfa p new_delta new_start_state new_accept_states (fun x y -> if Shared.lists_equal x y comparator then true else false)

let rec partition_states_helper (dfa : 'a DFA.dfa) (p : 'a list list) (w : 'a list list) (reachable_states : 'a list) : ('a list) DFA.dfa = 
	let (states, delta, start_state, accept_states, comparator) = DFA.deconstruct_dfa dfa in
	match w with
	| [] -> build_dfa_of_dfa dfa p
	| a::t -> let x = (generate_transition_to_states reachable_states delta a comparator) in let (p_new,w_new) = 
		List.fold_left (fun (p_acc,w_acc) y -> 
			if (List.length (Shared.cept y x comparator) > 0 && List.length (Shared.intersect y x comparator) > 0)
				then let p_acc' = (Shared.cept y x comparator)::(Shared.intersect y x comparator)::p_acc and w_acc' = manipulate_w x y p t comparator in (p_acc',w_acc') 
			else (y::p_acc,w_acc)) 
		([],t) p in partition_states_helper dfa p_new w_new reachable_states



let partition_states (dfa : 'a DFA.dfa) (reachable_states : 'a list) : ('a list) DFA.dfa = 
	let (_, _, _, accept_states, comparator) = DFA.deconstruct_dfa dfa in
	match (accept_states,Shared.cept reachable_states accept_states comparator) with 
	| (h1::t1,h2::t2) -> let p = [accept_states;Shared.cept reachable_states accept_states comparator] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| (h1::t1,[]) -> let p = [accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],h2::t2) -> let p = [Shared.cept reachable_states accept_states comparator] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],[]) -> let p = [] in let w = [] in
			partition_states_helper dfa p w reachable_states


let minimize (dfa : 'a DFA.dfa) : ('a list) DFA.dfa = 
	let (states, delt, start_state, accept_states, comparator) = DFA.deconstruct_dfa dfa in
	let reachable_states = find_reachable_states dfa [start_state] in
	let new_dfa = reachable_dfa dfa reachable_states in
	partition_states new_dfa reachable_states


let rec dfa_equivalence_helper (delta1 : ('a list * char * 'a list) list) (delta2 : ('a list * char * 'a list) list) (state_assignment : ('a list * 'a list) list) 
(q1 : 'a list) (q2 : 'a list) (comparator : 'a -> 'a -> bool) : bool * (('a list * 'a list) list) =
	if (List.fold_left (fun acc (p1,p2) -> 
		if Shared.lists_equal q1 p1 comparator && not (Shared.lists_equal q2 p2 comparator) then true else acc) false state_assignment) then (false,state_assignment)
	else let q1_found = List.fold_left (fun acc (p1,p2) -> if (Shared.lists_equal q1 p1 comparator) then true else acc) false state_assignment in
		(if q1_found then (true,state_assignment) else let state_assignment = (q1,q2)::state_assignment in 
			List.fold_left (fun (same,current_state_assignment) a -> let (same_sub,sa') = dfa_equivalence_helper delta1 delta2 current_state_assignment 
				(delta_list_goto delta1 q1 a comparator) (delta_list_goto delta2 q2 a comparator) comparator in 
				if same_sub then (same,sa') else (false, sa')) 
			(true,state_assignment) Shared.alphabet)
		



(* requires the two dfa to share the same state comparison function *)
let minimal_dfa_equivalence (dfa1 : 'a list DFA.dfa) (dfa2 : 'a list DFA.dfa) (comparator : 'a -> 'a -> bool) : bool = 
  let (states1, delta1, s1, accept_states1, _) = DFA.deconstruct_dfa dfa1 and (states2, delta2, s2, accept_states2, _) = DFA.deconstruct_dfa dfa2 in
	let (assignment_works,assignment) = dfa_equivalence_helper delta1 delta2 [] s1 s2 comparator in
	(List.fold_left (fun acc (q1,q2) -> let c1 = (Shared.contains q1 accept_states1 (fun x y -> Shared.lists_equal x y comparator)) 
										and c2 = (Shared.contains q2 accept_states2 (fun x y -> Shared.lists_equal x y comparator)) in 
		if (c1 && c2) || (not c1 && not c2) then acc else false) true assignment) && assignment_works



(* requires the two dfa to share the same state comparison function *)
let equivalence_test_dfa (dfa1 : 'a DFA.dfa) (dfa2 : 'a DFA.dfa) : bool = 
	let (_,_,_,_,comparator) = DFA.deconstruct_dfa dfa1 in
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2 comparator

(* requires the two dfa to share the same state comparison function *)
let equalence_test_nfa (nfa1 : 'a NFA.nfa) (nfa2 : 'a NFA.nfa) : bool = 
	let (_,_,_,_,comparator) = NFA.deconstruct_nfa nfa1 in
	let dfa1 = NFAtoDFA.convert_NFA_to_DFA nfa1 and dfa2 = NFAtoDFA.convert_NFA_to_DFA nfa2 in
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2 (fun x y -> Shared.lists_equal x y comparator)


end