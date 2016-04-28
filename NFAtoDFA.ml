(* Module to create a convert a NFA to a DFA *)

module NFAtoDFA = struct

	open Definitions
	open Shared
	open Shared


	(* check if two delta relations are equivalent *)
	let delta_relations_equal ((pstates1,char1,qstates1) : ('a list) * char * ('a list)) ((pstates2,char2,qstates2) : ('a list) * char * ('a list)) 
		(comparator : 'a -> 'a -> bool) : bool = 
		if (lists_equal pstates1 pstates2 comparator) && (compare char1 char2 == 0) && (lists_equal qstates1 qstates2 comparator) then true else false

	(* add a new state to a set of states unless it is a dubplicate of an element in the state set *)
	let add_state (old_states : 'a list list) (new_state : 'a list) (comparator : 'a -> 'a -> bool) : 'a list list = let repeat = List.fold_left (fun acc old_state ->
		if (lists_equal new_state old_state comparator) then true else acc) false old_states in if repeat then old_states else new_state::old_states

	(* add a delta relation to a delta relation list *)
	let add_delta_relation (old_delta : (('a list) * char * ('a list)) list) (new_delta : (('a list) * char * ('a list))) (comparator : 'a -> 'a -> bool)
	: (('a list) * char * ('a list)) list =
		if (List.fold_left (fun acc old_relation -> if (delta_relations_equal new_delta old_relation comparator) then true else acc) false old_delta) 
			then old_delta else new_delta::old_delta

	(* convert an nfa to a dfa given the nfa, a list of previously accessed states, and the delta relation currently built from these previously found states *)
	let rec convert_to_DFA (nfa : 'a NFA.nfa) (states : 'a list list) (delta : (('a list) * char * ('a list)) list)
	: ('a list list) * ((('a list) * char * ('a list)) list) = 
		let (_, nfa_delta, nfa_start, nfa_accepts, comparator) = NFA.deconstruct_nfa nfa in let (new_states,new_delta) = 
			List.fold_left (fun acc input_char -> 
				List.fold_left (fun ((states : 'a list list),delta) state_list -> let new_state_list = goto state_list nfa_delta input_char comparator in 
					let new_delta_relation = (state_list,input_char,new_state_list) in let new_states = add_state states new_state_list comparator in
						let new_delta = add_delta_relation delta new_delta_relation comparator in (new_states,new_delta)) acc states) 
							(states,delta) alphabet in
		if (List.length states == List.length new_states) && (List.length delta == List.length new_delta)
			then (new_states,new_delta) else convert_to_DFA nfa new_states new_delta


	(* convert a nfa to a dfa
	* precondition: the nfa's states list does not contain duplicates *)
	let convert_NFA_to_DFA (nfa : 'a NFA.nfa) : ('a list) DFA.dfa = 
		let (_, nfa_delta, nfa_start, nfa_accepts, comparator) = NFA.deconstruct_nfa nfa in
		let start_state = (closure [nfa_start] nfa_delta comparator) in
		let (states_list,delta_list) = convert_to_DFA nfa [start_state] [] in
		match states_list with
		| h::t -> let (accept_states : 'a list list) = 
			List.fold_left (fun dfa_accept_states dfa_state -> 
				List.fold_left (fun dfa_accept_states state -> 
					List.fold_left (fun dfa_accept_states accept_state -> 
						if comparator state accept_state then add_state dfa_accept_states dfa_state comparator
						else dfa_accept_states) 
					dfa_accept_states nfa_accepts) 
				dfa_accept_states dfa_state) 
			[] states_list 
			in DFA.build_dfa states_list delta_list start_state accept_states (fun x y -> if lists_equal x y comparator then true else false)
		| [] -> DFA.build_dfa [] [] [] [] (fun x y -> if lists_equal x y comparator then true else false)


end