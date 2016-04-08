module type DFA = sig
	type dfa
	val states : 'a list
	val alphabet : string list
	val delta : 'a -> string -> 'a
	val start_state : 'a
	val accepted_states : 'a list
end