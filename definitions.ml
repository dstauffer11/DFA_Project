(*type 'a states = States of 'a list
type 'a delta = Delta of ('a * char * 'a) list
type 'a start_state = StartState of 'a
type 'a accept_states = AcceptStates of 'a list
type 'a dfa = Dfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)
type 'a nfa = Nfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)*)

type regex = 
	|Star of regex
	| Or of regex * regex
	| Concat of regex * regex
	| Epsilon
	| Character of char



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
	let build_dfa = fun states delta start_state accept_states comparator -> Dfa {states = states; delta = delta; start_state = start_state; 
		accept_states = accept_states; compare_states = comparator}
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
	let build_nfa = fun states delta start_state accept_states comparator -> Nfa {states = states; delta = delta; start_state = start_state; 
		accept_states = accept_states; compare_states = comparator}
end