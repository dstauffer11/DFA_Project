module Shared = struct
	open Definitions

	let alphabet = ['a';'b']

	let build_dfa (states : 'a list) (delta : ('a * char * 'a) list) (start_state : 'a) (accept_states : 'a list) : 'a dfa = 
		Dfa (States states, Delta delta, StartState start_state, AcceptStates accept_states)


	let contains (el : 'a) (lst : 'a list) (comparator : 'a -> 'a -> int) : bool = 
		List.fold_left (fun acc el2 -> if comparator el el2 == 0 then true else acc) false lst

	let rec remove_dups (lst : string list) = match lst with 
		| [] -> []
		| h::t -> h::(remove_dups (List.filter (fun x -> compare x h <> 0) t))


	let lists_equal (lst1 : 'a list) (lst2 : 'a list) : bool = (List.fold_left (fun acc el1 -> 
			if List.exists (fun el2 -> if compare el1 el2 == 0 then true else false) lst2 then acc else false) true lst1) &&
		(List.fold_left (fun acc el2 -> 
			if List.exists (fun el1 -> if compare el1 el2 == 0 then true else false) lst1 then acc else false) true lst2)

	let lists_equal_int (lst1 : 'a list) (lst2 : 'a list) : int = if lists_equal lst1 lst2 then 0 else -1


	let cept (set : 'a list) (remove : 'a list) : 'a list = List.fold_left (fun acc el -> if (contains el remove compare) then acc else el::acc) [] set


	let union (lst1 : 'a list) (lst2 : 'a list) : 'a list = 
		List.fold_left (fun acc el1 -> if List.fold_left (fun found el2 -> if compare el1 el2 == 0 then true else found) false lst2 then acc else el1::acc) lst2 lst1

	let closure (states : 'a list) (delta : ('a * char * 'a) list) : ('a list) = List.fold_left (fun acc state -> List.fold_left (fun acc2 delt -> let (p,s,q) = delt in 
			if (compare p state) == 0 && (compare s ' ') == 0 then q::acc2 else acc2) acc delta) states states
	let goto (states : 'a list) (delta : ('a * char * 'a) list) (input : char) : ('a list) = let new_states = List.fold_left (fun acc state -> 
		List.fold_left (fun acc2 delt -> let (p,s,q) = delt in 
			if (compare p state) == 0 && (compare s input) == 0 then q::acc2 else acc2) acc delta) [] states in let dups_list = (closure new_states delta) in 
				remove_dups dups_list

	let flatten_once (lst : 'a list list) : 'a list = List.fold_left (fun acc el -> el@acc) [] lst

	let intersect (lst1 : 'a list) (lst2 : 'a list) : 'a list =
		List.fold_left (fun acc el1 -> if List.fold_left (fun found el2 -> if compare el1 el2 == 0 then true else found) false lst2 then el1::acc else acc) [] lst1

	let remove_set (set : 'a list list) (el : 'a list) : 'a list list = List.fold_left (fun set' el' -> if lists_equal el' el then set' else el'::set') [] set


	let set_in (set : 'a list list) (el : 'a list) : bool = List.fold_left (fun same lst -> if lists_equal lst el then true else same) false set


	let test_dfa : string dfa = Dfa (States ["s";"p"], Delta [("s",'a',"s");("s",'b',"p");("p",'a',"s");("p",'b',"s")], StartState "s", AcceptStates ["s";"p"])
	let test2_dfa : string dfa = Dfa (States ["p";"q";"r"], Delta [("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")], StartState "p",
			AcceptStates ["q";"r"])

	let test_nfa : string nfa = Nfa (States ["s";"p";"r"],
							Delta [("s",'a',"r");("s",'b',"p");("s",'a',"s");("p",'b',"r");("r",'a',"s");("r",'b',"p")],
							StartState "s",
							AcceptStates ["s";"p"])
end