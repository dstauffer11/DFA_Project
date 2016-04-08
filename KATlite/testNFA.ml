let test_nfa = (["s"],[("s","a","r");("s","b","p");("s","a","s");("p","b","r");("r","a","s");("r","b","p")],["s";"p"]) in
let test_string = ["a";"b";"b";"a"] in
let contains lst el = List.fold_left (fun acc b -> if (compare b el) == 0 then true else acc) false lst in
let list_contains lst1 lst2 = List.fold_left (fun acc el -> if (contains lst1 el) then true else acc) false lst2 in
let rec simulateNFA nfa input : (string list) list * bool = 
	let list_finder lst q s : string list = List.fold_left (fun acc str -> let (a,b,c) = str in if ((compare a  q) == 0 && (compare b s) == 0) then c::acc else acc) [] lst in
	let (states,delta,accepts) = nfa in
	match input with
	| h::t -> let new_states = List.fold_left (fun acc state -> let new_states = list_finder delta state h in new_states @ acc) [] states 
					in let (future_states,accepted) = simulateNFA (new_states, delta, accepts) t in (states::future_states,accepted)
	| [] -> ([states], list_contains accepts states)
in simulateNFA test_nfa test_string


(* add epsilon transitions to the running *)