(* generate new strings to use as node labels for DFAs/NFAs *)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref 
  
  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t
        
  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end


(* funcitons shared across the different modules *)
module Shared = struct
	open Definitions



	(*  change the output of compare from an int to bool *)
	let comparator_of_compare (x : 'a) (y : 'a) : bool = if compare x y == 0 then true else false

	(* check if el is in the list lst *)
	let contains (el : 'a) (lst : 'a list) (comparator : 'a -> 'a -> bool) : bool = 
		List.fold_left (fun acc el2 -> if comparator el el2 then true else acc) false lst

	(* remove the duplicate elements in a list *)
	let rec remove_dups (lst : 'a list) (comparator : 'a -> 'a -> bool) = match lst with 
		| [] -> []
		| h::t -> h::(remove_dups (List.filter (fun x -> not (comparator x h)) t) comparator)

	(* check if two lists contain teh same set of items *)
	let lists_equal (lst1 : 'a list) (lst2 : 'a list) (comparator : 'a -> 'a -> bool) : bool = (List.fold_left (fun acc el1 -> 
			if List.exists (fun el2 -> if comparator el1 el2 then true else false) lst2 then acc else false) true lst1) &&
		(List.fold_left (fun acc el2 -> 
			if List.exists (fun el1 -> if comparator el1 el2 then true else false) lst1 then acc else false) true lst2)

	(* check if two lists contain the same item and return an integer 0 if equal, -1 if not equal *)
	let lists_equal_int (lst1 : 'a list) (lst2 : 'a list) (comparator : 'a -> 'a -> bool) : int = if lists_equal lst1 lst2 comparator then 0 else -1

	(* return set without any elements shared by set and remove *)
	let cept (set : 'a list) (remove : 'a list) (comparator : 'a -> 'a -> bool): 'a list = 
		List.fold_left (fun acc el -> if (contains el remove comparator) then acc else el::acc) [] set

	(* union of two lists *)
	let union (lst1 : 'a list) (lst2 : 'a list) (comparator : 'a -> 'a -> bool) : 'a list = 
		List.fold_left (fun acc el1 -> if List.fold_left (fun found el2 -> if comparator el1 el2 then true else found) false lst2 then acc else el1::acc) lst2 lst1

	(* get the closure of a set of states using the given delta relation. I.e. takes any epsilon transitions available from teh given set of states *)
	let rec closure (states : 'a list) (delta : ('a * char * 'a) list) (comparator : 'a -> 'a -> bool) : ('a list) = 
		let states' = List.fold_left (fun acc state -> List.fold_left (fun acc2 delt -> let (p,s,q) = delt in 
			if (comparator p state) && (compare s ' ') == 0 then q::acc2 else acc2) acc delta) states states in let states'' = remove_dups states' comparator in
				if (List.length states) == (List.length states'') then states'' else closure states'' delta comparator
	
	(* uses the given delta relation to transition from a set of states to the set of reachable states with the given input *)
	let goto (states : 'a list) (delta : ('a * char * 'a) list) (input : char) (comparator : 'a -> 'a -> bool) : ('a list) = 
		let new_states = List.fold_left (fun acc state -> 
			List.fold_left (fun acc2 delt -> let (p,s,q) = delt in 
				if (comparator p state) && (compare s input == 0) then q::acc2 else acc2) acc delta) [] states in let dups_list = (closure new_states delta comparator) in 
					remove_dups dups_list comparator

	(* flatten a list once *)
	let flatten_once (lst : 'a list list) : 'a list = List.fold_left (fun acc el -> el@acc) [] lst

	(* take the intersection of two sets *)
	let intersect (lst1 : 'a list) (lst2 : 'a list) (comparator : 'a -> 'a -> bool) : 'a list =
		List.fold_left (fun acc el1 -> if List.fold_left (fun found el2 -> if comparator el1 el2 then true else found) false lst2 then el1::acc else acc) [] lst1

	(* remove a set from a set of sets *)
	let remove_set (set : 'a list list) (el : 'a list) (comparator : 'a -> 'a -> bool) : 'a list list = 
		List.fold_left (fun set' el' -> if lists_equal el' el comparator then set' else el'::set') [] set

	(* check if a set is in a set of sets *)
	let set_in (set : 'a list list) (el : 'a list) (comparator : 'a -> 'a -> bool) : bool = 
		List.fold_left (fun same lst -> if lists_equal lst el comparator then true else same) false set

	(* join to lists, a tail-recursive version of @ *)
	let merge (lst1 : 'a list) (lst2 : 'a list) : 'a list = 
		List.fold_left (fun acc el -> el::acc) lst1 lst2

	(* get a variable that is not used in the current list of variables lst *)
	let fresh_var (lst : string list) : string = let ls = LexStream.make () in 
		let rec f () = let new_string = LexStream.next ls in if contains new_string lst (fun x y -> if compare x y == 0 then true else false) then f() else new_string in f()

	(* create a new names for a set of variables and return the old set paired with the new names *)
	let fresh_states (lst1 : string list) (lst2 : string list) : (string * string) list =
		fst (List.fold_left (fun (acc,vars) el -> let f_v = fresh_var vars in ((el,f_v)::acc,f_v::vars)) ([],lst1) lst2)

	(* recreate delta with the new variable names *)
	let delta_fresh_vars (delta : (string * char * string) list) (assoc_list : (string * string) list) : (string * char * string) list  = 
		(List.fold_left (fun acc (p,a,q) -> (List.assoc p assoc_list,a, List.assoc q assoc_list)::acc) [] delta)

	(* find the new name of a variable *)
	let find_var (lst_pair : (string * string) list) (var : string) : string = 
		List.fold_left (fun acc (el,el') -> if compare el var == 0 then el' else acc) "" lst_pair

	let parse_dfa (location : string) : 'a DFA.dfa = DFA.create ()

	let parse_nfa (location : string) : 'a NFA.nfa = NFA.create ()

	let parse_regex (location : string) : regex = Epsilon
end