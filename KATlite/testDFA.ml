let test_dfa = ("s",[("s","a","r");("s","b","p");("p","a","s");("p","b","r");("r","a","s");("r","b","p")],["s";"p"]) in
let test_string = ["a";"b";"a"] in
let contains lst el = List.fold_left (fun acc b -> if (compare b el) == 0 then true else acc) false lst in


let rec simulateDFA dfa input : string list * bool = 
	let list_finder lst q s : string = List.fold_left (fun acc str -> let (a,b,c) = str in if ((compare a  q)==0 && (compare b s)==0) then c else acc) "" lst in
	let (state,delta,accepts) = dfa in
	match input with
	| h::t -> let new_state = list_finder delta state h in let (states,accepted) = simulateDFA (new_state, delta, accepts) t in (state::states,accepted)
	| [] -> ([state], contains accepts state)
in simulateDFA test_dfa test_string
