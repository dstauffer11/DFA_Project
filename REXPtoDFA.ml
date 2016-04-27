(* convert regular expressions to NFAs and DFAs *)

module REXPtoDFA = struct
	open Definitions
	open Shared
	open Shared
	open MinimizeDFA
	open NFAtoDFA
	open NFAtoDFA
	open DifferenceDFA
	open DifferenceDFA


	exception NoAcceptStateInNFA



	let star_NFA (states : string list) (delta : (string * char * string) list) (start_state : string) (accept_states : string list) : string NFA.nfa =
		match accept_states with
		| [] -> raise NoAcceptStateInNFA
		| h::t -> let fresh_start = fresh_var states in
			NFA.build_nfa (fresh_start::states) ((fresh_start, ' ', h)::(fresh_start,' ',start_state)::(h,' ',fresh_start)::delta) fresh_start [h] 
				Shared.comparator_of_compare

	let concat_NFA (states1) (delta1) (start1) (accepts1) (states2) (delta2) (start2) (accepts2) : string NFA.nfa = 
		match (accepts1,accepts2) with
		| (h1::t1,h2::t2) -> let states_pair = fresh_states states1 states2 in let (_,states2') = List.split states_pair in 
			let delta2' = delta_fresh_vars delta2 states_pair in
				NFA.build_nfa (merge states1 states2') ((h1, ' ', (find_var states_pair start2))::(merge delta1 delta2')) start1 [find_var states_pair h2] 
					Shared.comparator_of_compare
		| (_,_) -> raise NoAcceptStateInNFA

	let or_NFA (states1) (delta1) (start1) (accepts1) (states2) (delta2) (start2) (accepts2) : string NFA.nfa = 
		match (accepts1,accepts2) with
		| (h1::t1,h2::t2) -> let states_pair = fresh_states states1 states2 in let (_,states2') = List.split states_pair in
			let delta2' = delta_fresh_vars delta2 states_pair in let states' = (merge states1 states2') in 
			let fresh_start = fresh_var states' in let fresh_accept = fresh_var (fresh_start::states') in
				NFA.build_nfa (fresh_start::fresh_accept::states') 
				((fresh_start,' ',start1)::(fresh_start,' ', (find_var states_pair start2))::(h1,' ',fresh_accept)::
					((find_var states_pair h2), ' ',fresh_accept)::(merge delta1 delta2')) 
				fresh_start [fresh_accept] Shared.comparator_of_compare
		| (_,_) -> raise NoAcceptStateInNFA

	(* conert regular epxression to NFA *)
	let rec regex_to_nfa (regex : regex) : string NFA.nfa =
		match regex with
		| Star exp -> let (states, delta, start_state, accept_states, comparator) = NFA.deconstruct_nfa (regex_to_nfa exp) in
			star_NFA states delta start_state accept_states
		| Concat (exp1, exp2) -> let (states1, delta1, start_state1, accept_states1,_) = NFA.deconstruct_nfa (regex_to_nfa exp1) and
			(states2, delta2, start_state2, accept_states2,_) = NFA.deconstruct_nfa (regex_to_nfa exp2) in
			concat_NFA states1 delta1 start_state1 accept_states1 states2 delta2 start_state2 accept_states2
		| Or (exp1, exp2) -> let (states1, delta1, start_state1, accept_states1,_) = NFA.deconstruct_nfa (regex_to_nfa exp1) and
			(states2, delta2, start_state2, accept_states2,_) = NFA.deconstruct_nfa (regex_to_nfa exp2) in
			or_NFA states1 delta1 start_state1 accept_states1 states2 delta2 start_state2 accept_states2
		| Character a -> let s = fresh_var [] in let p = fresh_var [s] in NFA.build_nfa [s;p] [(s,a,p)] s [p] Shared.comparator_of_compare
		| Epsilon -> let s = fresh_var [] in NFA.build_nfa [s] [] s [s] Shared.comparator_of_compare

	(* convert regular expression to DFA *)
	let regex_to_dfa (regex : regex) : (string list) DFA.dfa = convert_NFA_to_DFA (regex_to_nfa regex)



end