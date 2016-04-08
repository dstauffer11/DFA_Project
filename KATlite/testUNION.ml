type ('a,'b) nfa = 'a * (('a * char * 'a) list) * 'a list
	| ('a * 'b) * (('a * char * 'a) list) b' * ('a list) 'b	




let union_nfa (nfa1 : ('a1, 'b1) nfa) (nfa2 : ('a2, 'b2) nfa) : (('a1,'a2),('b1,'b2)) nfa = 
	let (start1,delta1,accept1) = nfa1 and (start2,delta2,accept1) = nfa2 in
		((start1,start2), (delta1,delta2), (accept1, accept2))


