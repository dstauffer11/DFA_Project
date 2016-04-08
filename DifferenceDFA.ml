module DifferenceDFA = struct
	open MinimizeDFA
	open Definitions
	open Shared
	open Shared

	exception DFAs_Equivalent
	exception NoDifferenceFound
	exception NoResultingState



	let invert_dfa (dfa : 'a dfa) : 'a dfa = let Dfa (States states, Delta delta, StartState s, AcceptStates accepts) = dfa in
		build_dfa states delta s (cept states accepts)

	let delta_use (delta : ('a * char * 'a) list) (state : 'a) (a : char) : 'a = 
		let resulting_state = List.fold_left (fun acc (p,s,q) -> if compare p state == 0 then Some q else acc) None delta in
		match resulting_state with 
		| Some x -> x
		| None -> raise NoResultingState

	let visit_state (states_tracker : ('a * bool) list) (state : 'a) : ('a * bool) list = 
		List.fold_left (fun acc (state',visited) -> if compare state state' == 0 then (state',visited)::acc else (state',true)::acc) [] states_tracker


	let rec difference_helper (delta1 : ('a * char * 'a) list) (delta2 : ('a * char * 'a) list) (q1 : 'a) (q2 : 'a) 
	(accepts1 : 'a list) (accepts2 : 'a list) (states1_tracker : ('a * bool) list) (states2_tracker : ('a * bool) list) (char_list : char list)
		: bool * (char list) * (('a * bool) list) * (('a * bool) list) = let c1 = contains q1 accepts1 compare and c2 = contains q2 accepts2 compare in
			if (c1 && not c2) || (not c1 && c2) then (false,char_list,states1_tracker,states2_tracker) 
			else List.fold_left (fun (found,char_list,s1t,s2t) a -> let (found',char_list',s1t',s2t') = 
					(let q1' = (delta_use delta1 q1 a) and q2' = (delta_use delta2 q2 a) in
				difference_helper delta1 delta2 q1' q2' accepts1 accepts2 (visit_state s1t q1') (visit_state s2t q2') (a::char_list)) in
			if found then (true, char_list, s1t', s2t') else (false, char_list', s1t',s2t'))
			(true,char_list,states1_tracker,states2_tracker) alphabet


	let find_difference_in_dfas (dfa1 : 'a dfa) (dfa2 : 'a dfa) : char list =
		if MinimizeDFA.equivalence_test_dfa dfa1 dfa2 then raise DFAs_Equivalent
		else let dfa2 = invert_dfa dfa2 in
			let Dfa (States states1, Delta delta1, StartState s1, AcceptStates accepts1) = dfa1 and
			Dfa (States states2, Delta delta2, StartState s2, AcceptStates accepts2) = dfa2 in
			let states1_tracker = List.map (fun s -> (s,false)) states1 and states2_tracker = List.map (fun s -> (s,false)) states2 in
			let (found,char_list,states1_tracker,states2_tracker) = difference_helper delta1 delta2 s1 s2 accepts1 accepts2 states1_tracker states2_tracker [] in
			if not found then char_list else raise NoDifferenceFound


end