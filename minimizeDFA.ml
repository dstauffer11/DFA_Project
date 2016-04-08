module MinimizeDFA = struct

open NFAtoDFA
open Definitions
open Shared




let rec find_reachable_states (dfa : 'a dfa) (found_states : 'a list) : 'a list = 
	let Dfa (_,Delta delta,_,_) = dfa in
	let new_states = Shared.flatten_once (List.fold_left (fun acc a -> (Shared.goto found_states delta a)::acc) [] Shared.alphabet) in 
		let total_states = Shared.union new_states found_states in
			if List.length total_states == List.length found_states then total_states
				else find_reachable_states dfa total_states


let reachable_dfa (dfa : 'a dfa) (reachable_states : 'a list) : 'a dfa = 
	let Dfa (States states, Delta delta, StartState start_state, AcceptStates accepts) = dfa in
	let new_accepts = List.fold_left (fun acc el -> if (Shared.contains el reachable_states compare) then el::acc else acc) [] accepts in
	let new_delta = List.fold_left (fun acc (s1,c,s2) -> if Shared.contains s1 reachable_states compare then (s1,c,s2)::acc else acc) [] delta in
	let new_start_state = start_state in
	Shared.build_dfa reachable_states new_delta new_start_state new_accepts





let generate_transition_to_states (reachable_states : 'a list) (delta : ('a * char * 'a) list) (a : 'a list) : 'a list = 
	List.fold_left (fun acc state -> List.fold_left (fun acc s -> 
		if Shared.intersect (Shared.goto [state] delta s) a <> [] then state::acc else acc) acc Shared.alphabet) [] reachable_states



let manipulate_w (x : 'a list) (y : 'a list) (p : 'a list list) (w : 'a list list) : 'a list list = 
	if Shared.set_in w y then let w' = Shared.remove_set w y in (Shared.cept y x)::(Shared.intersect y x)::w'
	else (if List.length (Shared.intersect y x) <= List.length (Shared.cept y x) then (Shared.intersect y x)::w else (Shared.cept y x)::w)


let set_of (set : 'a list list) (el : 'a) : 'a list = List.fold_left (fun set_of_el set_el -> if Shared.contains el set_el compare then set_el else set_of_el) [] set

let delta_goto (delta : ('a * char * 'a) list) (lst : 'a list) (a : char) : 'a =
	match lst with
	| [] -> ""
	| h::t -> List.fold_left (fun acc (p,s,q) -> if compare p h == 0 then q else acc) "" delta

let rec delta_list_goto (delta : (('a list) * char * ('a list)) list) (state : 'a list) (a : char) : 'a list =
	match delta with
	| [] -> []
	| (p,s,q)::t -> if Shared.lists_equal p state && compare a s == 0 then q else delta_list_goto t state a



let create_delta (dfa : 'a dfa) (p : 'a list list) : ('a list * char * 'a list) list = 
	let Dfa (States states, Delta delt, StartState s, AcceptStates accept) = dfa in
	List.fold_left (fun delta partition -> List.fold_left (fun delta a -> (partition,a,(set_of p (delta_goto delt partition a)))::delta) delta Shared.alphabet) [] p


let build_dfa_of_dfa (dfa : 'a dfa) (p : 'a list list) : ('a list) dfa = 
	let Dfa (States states, Delta delt, StartState s, AcceptStates accept) = dfa in
	let start_state = List.fold_left (fun start_state_temp partition -> if Shared.contains s partition compare then partition else start_state_temp) [] p in
	let delta = create_delta dfa p in
	let accept_states = List.fold_left (fun acc partition -> if Shared.set_in [accept] partition then partition::acc else acc) [] p in
	Dfa (States p, Delta delta, StartState start_state, AcceptStates accept_states)

let rec partition_states_helper (dfa : 'a dfa) (p : 'a list list) (w : 'a list list) (reachable_states : 'a list) : ('a list) dfa = 
	let Dfa (States states, Delta delta, StartState s, AcceptStates accept_states) = dfa in
	match w with
	| [] -> build_dfa_of_dfa dfa p
	| a::t -> let x = (generate_transition_to_states reachable_states delta a) in let (p_new,w_new) = List.fold_left (fun (p_acc,w_acc) y -> 
			if (List.length (Shared.cept y x) > 0 && List.length (Shared.intersect y x) > 0)
			then let p_acc' = (Shared.cept y x)::(Shared.intersect y x)::p_acc and w_acc' = manipulate_w x y p t in (p_acc',w_acc') else (y::p_acc,w_acc)) ([],t) p in 
		partition_states_helper dfa p_new w_new reachable_states



let partition_states (dfa : 'a dfa) (reachable_states : 'a list) : ('a list) dfa = 
	let Dfa (States states, Delta delta, StartState s, AcceptStates accept_states) = dfa in
	match (accept_states,Shared.cept reachable_states accept_states) with 
	| (h1::t1,h2::t2) -> let p = [accept_states;Shared.cept reachable_states accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| (h1::t1,[]) -> let p = [accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],h2::t2) -> let p = [Shared.cept reachable_states accept_states] in let w = [accept_states] in
			partition_states_helper dfa p w reachable_states
	| ([],[]) -> let p = [] in let w = [] in
			partition_states_helper dfa p w reachable_states


let minimize (dfa : 'a dfa) : ('a list) dfa = 
	let Dfa (States states, Delta delta, StartState s, AcceptStates accept_states) = dfa in
	let reachable_states = find_reachable_states dfa [s] in
	let new_dfa = reachable_dfa dfa reachable_states in
	partition_states new_dfa reachable_states


let rec dfa_equivalence_helper (delta1 : ('a list * char * 'a list) list) (delta2 : ('a list * char * 'a list) list) (state_assignment : ('a list * 'a list) list) 
(q1 : 'a list) (q2 : 'a list) : bool * (('a list * 'a list) list) =
	if (List.fold_left (fun acc (p1,p2) -> 
		if Shared.lists_equal q1 p1 && not (Shared.lists_equal q2 p2) then true else acc) false state_assignment) then (false,state_assignment)
	else let q1_found = List.fold_left (fun acc (p1,p2) -> if (Shared.lists_equal q1 p1) then true else acc) false state_assignment in
		(if q1_found then (true,state_assignment) else let state_assignment = (q1,q2)::state_assignment in 
			List.fold_left (fun (same,current_state_assignment) a -> let (same_sub,sa') = dfa_equivalence_helper delta1 delta2 current_state_assignment 
				(delta_list_goto delta1 q1 a) (delta_list_goto delta2 q2 a) in 
				if same_sub then (same,sa') else (false, sa')) 
			(true,state_assignment) Shared.alphabet)
		




let minimal_dfa_equivalence (dfa1 : 'a list dfa) (dfa2 : 'a list dfa) : bool = 
  let Dfa (States states1, Delta delta1, StartState s1, AcceptStates accepts1) = dfa1 and Dfa (States states2, Delta delta2, StartState s2, AcceptStates accepts2) = dfa2 in
	let (assignment_works,assignment) = dfa_equivalence_helper delta1 delta2 [] s1 s2 in
	(List.fold_left (fun acc (q1,q2) -> let c1 = (Shared.contains q1 accepts1 Shared.lists_equal_int) and c2 = (Shared.contains q2 accepts2 Shared.lists_equal_int) in 
		if (c1 && c2) || (not c1 && not c2) then acc else false) true assignment) && assignment_works




let equivalence_test_dfa (dfa1 : 'a dfa) (dfa2 : 'a dfa) : bool = 
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2


(*let equalence_test_nfa (nfa1) (nfa2) : bool = 
	let dfa1 = NFAtoDFA.convert_NFA_to_DFA nfa1 and dfa2 = NFAtoDFA.convert_NFA_to_DFA nfa2 in
	let min_dfa1 = minimize dfa1 and min_dfa2 = minimize dfa2 in
	minimal_dfa_equivalence min_dfa1 min_dfa2	*)


end