type 'a states = States of 'a list
type 'a delta = Delta of ('a * char * 'a) list
type 'a start_state = StartState of 'a
type 'a accept_states = AcceptStates of 'a list
type 'a dfa = Dfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)
type 'a nfa = Nfa of ('a states) * ('a delta) * ('a start_state) * ('a accept_states)

type regex = 
	|Star of regex
	| Or of regex * regex
	| Concat of regex * regex
	| Epsilon
	| Character of char