module NFAtoDFA = struct

	open Definitions
	open Shared
	open Shared



	let delta_relations_equal (pstates1,char1,qstates1) (pstates2,char2,qstates2) : bool = 
		if (lists_equal pstates1 pstates2) && (compare char1 char2 == 0) && (lists_equal qstates1 qstates2) then true else false


	let add_state (old_states : 'a list list) (new_state : 'a list) : 'a list list = let repeat = List.fold_left (fun acc old_state ->
		if (lists_equal new_state old_state) then true else acc) false old_states in if repeat then old_states else new_state::old_states

	let add_delta_relation (old_delta : (('a list) * char * ('a list)) list) (new_delta : (('a list) * char * ('a list))) : (('a list) * char * ('a list)) list =
		if (List.fold_left (fun acc old_relation -> if (delta_relations_equal new_delta old_relation) then true else acc) false old_delta) 
			then old_delta else new_delta::old_delta


	let rec convert_to_DFA (nfa : 'a nfa) (states : 'a list list) (delta : (('a list) * char * ('a list)) list) : ('a list list) * ((('a list) * char * ('a list)) list) = 
		let Nfa (_,Delta nfa_delta,StartState nfa_start,AcceptStates nfa_accepts) = nfa in let (new_states,new_delta) = 
			List.fold_left (fun acc input_char -> 
				List.fold_left (fun ((states : 'a list list),delta) state_list -> let new_state_list = goto state_list nfa_delta input_char in 
					let new_delta_relation = (state_list,input_char,new_state_list) in let new_states = add_state states new_state_list in
						let new_delta = add_delta_relation delta new_delta_relation in (new_states,new_delta)) acc states) 
							(states,delta) alphabet in
		if (List.length states == List.length new_states) && (List.length delta == List.length new_delta)
			then (new_states,new_delta) else convert_to_DFA nfa new_states new_delta


	(* states_list does not contain duplicates *)
	let convert_NFA_to_DFA (nfa : 'a nfa) : ('a list) dfa = 
		let Nfa (_,Delta nfa_delta,StartState nfa_start,AcceptStates nfa_accepts) = nfa in
		let start_state = (closure [nfa_start] nfa_delta) in
		let (states_list,delta_list) = convert_to_DFA nfa [start_state] [] in
		match states_list with
		| h::t -> let (accept_states : 'a list list) = 
			List.fold_left (fun dfa_accept_states dfa_state -> 
				List.fold_left (fun dfa_accept_states state -> 
					List.fold_left (fun dfa_accept_states accept_state -> if (compare state accept_state) == 0 
						then add_state dfa_accept_states dfa_state (*dfa_state::dfa_accept_states*)
						else dfa_accept_states) dfa_accept_states nfa_accepts) dfa_accept_states dfa_state) [] states_list 
					in Dfa (States states_list, Delta delta_list,StartState start_state, AcceptStates accept_states)
		| [] -> Dfa (States [], Delta [], StartState [], AcceptStates [])


end