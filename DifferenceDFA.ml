(* Module to create find the difference between two DFAs or declare them identical *)

module DifferenceDFA = struct
	open MinimizeDFA
	open Definitions
	open Shared
	open Shared

	exception DFAs_Equivalent
	exception NoResultingState


	(* create a new dfa the same as the given dfa but with accept states and non-accpet states swapped *)
	let invert_dfa (dfa : 'a DFA.dfa) : 'a DFA.dfa = let (states, delta, s, accepts, comparator) = DFA.deconstruct_dfa dfa in
		DFA.build_dfa states delta s (cept states accepts comparator) comparator

	(* find the first instance in a delta relation that applies to the given state and input char, gives the resulting state of this relation *)
	let delta_use (delta : ('a * char * 'a) list) (state : 'a) (input : char) (comparator : 'a -> 'a -> bool) : 'a = 
		let resulting_state = List.fold_left (fun acc (p,s,q) -> if comparator p state && compare s input == 0 then Some q else acc) None delta in
		match resulting_state with 
		| Some x -> x
		| None -> raise NoResultingState

	(* mark a state as visited in the state_tracker pair list *)
	let visit_state (states_tracker : ('a * bool) list) (state : 'a) (comparator : 'a -> 'a -> bool) : ('a * bool) list = 
		List.fold_left (fun acc (state',visited) -> if comparator state state' then (state',true)::acc else (state',visited)::acc) [] states_tracker

	let state_visited (states_tracker : ('a * bool) list) (state : 'a) (comparator : 'a -> 'a -> bool) : bool = 
		List.fold_left (fun acc (state',visited) -> if comparator state state' then visited else acc) false states_tracker

	(* helper method for difference_helper *)
	let rec difference_helper (delta1 : ('a * char * 'a) list) (delta2 : ('a * char * 'a) list) (q1 : 'a) (q2 : 'a) 
	(accepts1 : 'a list) (accepts2 : 'a list) (states1_tracker : ('a * bool) list) (states2_tracker : ('a * bool) list) (char_list : char list)
	(comparator1 : 'a -> 'a -> bool) (comparator2 : 'a -> 'a -> bool) : bool * (char list) * (('a * bool) list) * (('a * bool) list) = 
		let c1 = contains q1 accepts1 comparator1 and c2 = contains q2 accepts2 comparator2 in
		if (c1 && (not c2)) || ((not c1) && c2) then (true,char_list,states1_tracker,states2_tracker)
		else List.fold_left (fun (found,char_list,s1t,s2t) input -> if found then (true,char_list,s1t,s2t) else 
				(let q1' = (delta_use delta1 q1 input comparator1) and q2' = (delta_use delta2 q2 input comparator2) in
					(if (state_visited s1t q1' comparator1) && (state_visited s2t q2' comparator2) then (false,char_list,s1t,s2t)
					else let (found',char_list',s1t',s2t') = difference_helper delta1 delta2 q1' q2' accepts1 accepts2 
						(visit_state s1t q1' comparator1) (visit_state s2t q2' comparator2) (input::char_list) comparator1 comparator2 in
				if found' then (true, char_list', s1t', s2t') else (false,char_list,s1t',s2t'))))
			(false,char_list,states1_tracker,states2_tracker) alphabet

	(* requires that the two dfa share a comparison function *)
	let find_difference_in_dfas (dfa1 : 'a DFA.dfa) (dfa2 : 'a DFA.dfa) : char list =
		if MinimizeDFA.equivalence_test_dfa dfa1 dfa2 then raise DFAs_Equivalent
		else let (states1, delta1, s1, accepts1, comparator1) = DFA.deconstruct_dfa dfa1 and
				 (states2, delta2, s2, accepts2, comparator2) = DFA.deconstruct_dfa dfa2 in
			let states1_tracker = List.map (fun s -> (s,false)) states1 and states2_tracker = List.map (fun s -> (s,false)) states2 in
			let (found,char_list,states1_tracker,states2_tracker) = 
				difference_helper delta1 delta2 s1 s2 accepts1 accepts2 states1_tracker states2_tracker [] comparator1 comparator2 in
			if found then List.rev char_list else raise DFAs_Equivalent


end