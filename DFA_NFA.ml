module type DFA = sig
	type 'a dfa_temp = {mutable states : ('a list); mutable delta : (('a * char * 'a) list); mutable start_state : 'a; mutable accept_states : ('a list)}
	type 'a dfa = Dfa of ('a dfa_temp ref)
	val create : unit -> unit dfa
	val build_dfa : unit dfa -> 'a list -> ('a * char * 'a) list -> 'a -> 'a list -> 'a dfa
end

module DFA = struct
	type 'a dfa_temp = {mutable states : ('a list); mutable delta : (('a * char * 'a) list); mutable start_state : 'a; mutable accept_states : ('a list);
						mutable compare_states : 'a -> 'a -> bool}
	type 'a dfa = Dfa of ('a dfa_temp)
	let create = fun () -> Dfa {states = []; delta = []; start_state = (); accept_states  = []; compare_states = fun q1 q2 -> false}
	let build_dfa = fun (dfa : unit dfa) states delta start_state accept_states comparator -> let Dfa dfa_temp = dfa in dfa_temp.states <- states; 
		dfa_temp.delta <- delta; dfa_temp.start_state <- start_state; dfa_temp.accept_states <- accept_states; dfa_temp.compare_states <- comparator; Dfa dfa_temp
end

module type NFA = sig
	type 'a nfa_temp = {mutable states : ('a list); mutable delta : (('a * char * 'a) list); mutable start_state : 'a; mutable accept_states : ('a list)}
		type 'a nfa = Nfa of ('a nfa_temp ref)
	val create : unit -> unit nfa
	val build_dfa : unit nfa -> 'a list -> ('a * char * 'a) list -> 'a -> 'a list -> 'a nfa
end

module NFA = struct
	type 'a nfa_temp = {mutable states : ('a list); mutable delta : (('a * char * 'a) list); mutable start_state : 'a; mutable accept_states : ('a list);
						mutable compare_states : 'a -> 'a -> bool}
	type 'a nfa = Nfa of ('a nfa_temp)
	let create = fun () -> Nfa {states = []; delta = []; start_state = (); accept_states  = []; compare_states = fun q1 q2 -> false}
	let build_nfa = fun (nfa : unit nfa) states delta start_state accept_states comparator -> let Nfa nfa_temp = nfa in nfa_temp.states <- states; 
		nfa_temp.delta <- delta; nfa_temp.start_state <- start_state; nfa_temp.accept_states <- accept_states; nfa_temp.compare_states <- comparator; Nfa nfa_temp
end