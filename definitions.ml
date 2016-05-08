(*type 'a states = States of 'a list
type 'a delta = Delta of ('a * char * 'a) list
type 'a start_state = StartState of 'a
type 'a accept_states = AcceptStates of 'a list
type 'a dfa = Dfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)
type 'a nfa = Nfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)*)

(* regular expression definition *)
type regex = 
	| Star of regex
	| Or of regex * regex
	| Concat of regex * regex
	| Epsilon
	| Character of char
	| Emptyset

(* DFA and NFA modules *)
module type DFA = sig
	type 'a dfa
	val create : unit -> unit dfa
	val build_dfa : 'a list -> char list -> ('a * char * 'a) list -> 'a -> 'a list -> ('a -> 'a -> bool) -> 'a dfa
	val deconstruct_dfa : 'a dfa -> (('a list)*(char list)*(('a * char * 'a) list)*('a)*('a list)*('a -> 'a -> bool))
end

module DFA : DFA = struct
	type 'a dfa = {mutable states : ('a list); mutable alphabet : (char list); mutable delta : (('a * char * 'a) list); mutable start_state : 'a; 
			mutable accept_states : ('a list); mutable compare_states : 'a -> 'a -> bool}
	let create = fun () -> {states = []; alphabet = []; delta = []; start_state = (); accept_states  = []; compare_states = fun q1 q2 -> false}
	let build_dfa = fun states alphabet delta start_state accept_states comparator -> {states = states; alphabet = alphabet; delta = delta; start_state = start_state; 
		accept_states = accept_states; compare_states = comparator}
	let deconstruct_dfa = fun dfa -> (dfa.states, dfa.alphabet, dfa.delta, dfa.start_state, dfa.accept_states, dfa.compare_states)
end

module type NFA = sig
	type 'a nfa
	val create : unit -> unit nfa
	val build_nfa : 'a list -> char list -> ('a * char * 'a) list -> 'a -> 'a list -> ('a -> 'a -> bool) -> 'a nfa
	val deconstruct_nfa : 'a nfa -> (('a list)*(char list)*(('a * char * 'a) list)*('a)*('a list)*('a -> 'a -> bool))
end

module NFA : NFA = struct
	type 'a nfa = {mutable states : ('a list); mutable alphabet : char list; mutable delta : (('a * char * 'a) list); mutable start_state : 'a; 
			mutable accept_states : ('a list); mutable compare_states : 'a -> 'a -> bool}
	let create = fun () -> {states = []; alphabet = []; delta = []; start_state = (); accept_states  = []; compare_states = fun q1 q2 -> false}
	let build_nfa = fun states alphabet delta start_state accept_states comparator -> {states = states; alphabet = alphabet; delta = delta; start_state = start_state; 
		accept_states = accept_states; compare_states = comparator}
	let deconstruct_nfa = fun nfa -> (nfa.states, nfa.alphabet, nfa.delta, nfa.start_state, nfa.accept_states, nfa.compare_states)
end