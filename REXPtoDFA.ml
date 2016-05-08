(* convert regular expressions to NFAs and DFAs *)

module REXPtoDFA = struct
	open Definitions
	open Shared
	open Shared
	open MinimizeDFA
	open NFAtoDFA
	open DifferenceDFA
	open DifferenceDFA


	exception NoAcceptStateInNFA
	exception EmptySetRegex


	(* create an NFA from the current nfa for x (given by states, delta, start_state, and accept state) and build a new nfa that represents x* *)
	let star_NFA (nfa : string NFA.nfa) : string NFA.nfa =
		let (states,alphabet,delta,start_state,accept_states,comparator) = NFA.deconstruct_nfa nfa in
		match accept_states with
		| [] -> raise NoAcceptStateInNFA
		| h::t -> let fresh_start = fresh_var states in
			NFA.build_nfa (fresh_start::states) alphabet ((fresh_start, ' ', h)::(fresh_start,' ',start_state)::(h,' ',fresh_start)::delta) fresh_start [h] 
				Shared.comparator_of_compare

	(* create an NFA from two nfas for regex x and y (given by states, delta, start_state, and accept state) and build a new nfa that represents xy *)
	let concat_NFA (nfa1 : string NFA.nfa) (nfa2 : string NFA.nfa) : string NFA.nfa = 
		let (states1, alphabet1, delta1, s1, accept_states1,_) = NFA.deconstruct_nfa nfa1 and 
			(states2, alphabet2, delta2, s2, accept_states2,_) = NFA.deconstruct_nfa nfa2 in
		match (accept_states1,accept_states2) with
		| (h1::t1,h2::t2) -> let states_pair = fresh_states states1 states2 in let (_,states2') = List.split states_pair in 
			let delta2' = delta_fresh_vars delta2 states_pair in
				NFA.build_nfa (merge states1 states2') (merge (cept alphabet1 (intersect alphabet1 alphabet2 comparator_of_compare) comparator_of_compare) alphabet2) 
				((h1, ' ', (find_var states_pair s2))::(merge delta1 delta2')) s1 [find_var states_pair h2] Shared.comparator_of_compare
		| (_,_) -> raise NoAcceptStateInNFA

	(* create an NFA from two nfas for regex x and y (given by states, delta, start_state, and accept state) and build a new nfa that represents x+y *)
	let or_NFA (nfa1 : string NFA.nfa) (nfa2 : string NFA.nfa) : string NFA.nfa = 
		let (states1, alphabet1, delta1, s1, accept_states1,_) = NFA.deconstruct_nfa nfa1 and 
			(states2, alphabet2, delta2, s2, accept_states2,_) = NFA.deconstruct_nfa nfa2 in
		match (accept_states1,accept_states2) with
		| (h1::t1,h2::t2) -> let states_pair = fresh_states states1 states2 in let (_,states2') = List.split states_pair in
			let delta2' = delta_fresh_vars delta2 states_pair in let states' = (merge states1 states2') in 
			let fresh_start = fresh_var states' in let fresh_accept = fresh_var (fresh_start::states') in
				NFA.build_nfa (fresh_start::fresh_accept::states') 
					(merge (cept alphabet1 (intersect alphabet1 alphabet2 comparator_of_compare) comparator_of_compare) alphabet2)
					((fresh_start,' ',s1)::(fresh_start,' ', (find_var states_pair s2))::(h1,' ',fresh_accept)::
						((find_var states_pair h2), ' ',fresh_accept)::(merge delta1 delta2')) 
					fresh_start [fresh_accept] Shared.comparator_of_compare
		| (_,_) -> raise NoAcceptStateInNFA

	(* convert regular epxression to NFA *)
	let rec regex_to_nfa (regex : regex) : string NFA.nfa =
		match regex with
		| Star exp -> let nfa = regex_to_nfa exp in star_NFA nfa
		| Concat (exp1, exp2) ->  let nfa1 = regex_to_nfa exp1 and nfa2 = regex_to_nfa exp2 in concat_NFA nfa1 nfa2
		| Or (exp1, exp2) -> let nfa1 = regex_to_nfa exp1 and nfa2 = regex_to_nfa exp2 in or_NFA nfa1 nfa2
		| Character a -> let s = fresh_var [] in let p = fresh_var [s] in NFA.build_nfa [s;p] [a] [(s,a,p)] s [p] Shared.comparator_of_compare
		| Epsilon -> let s = fresh_var [] in NFA.build_nfa [s] [] [] s [s] Shared.comparator_of_compare
		| Emptyset -> let s = fresh_var [] in NFA.build_nfa [s] [] [] s [] Shared.comparator_of_compare

	(* convert regular expression to DFA *)
	let regex_to_dfa (regex : regex) : (string list) DFA.dfa = NFAtoDFA.convert_NFA_to_DFA (regex_to_nfa regex)



end