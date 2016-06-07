(*   compare x y == 0                                   *)
let comparator_of_compare = (=)

(* check if el is in the list lst *)
let contains (comparator : 'a -> 'a -> bool) (lst : 'a list) (el : 'a) : bool =
  List.exists (comparator el) lst

(* add an element to a list if it is not already there *)
let add_if_new (comparator : 'a -> 'a -> bool) (el : 'a) (lst : 'a list) : 'a list =
  if (contains comparator lst el) then lst else el :: lst

(* remove the duplicate elements in a list *)
let rec remove_dups (comparator : 'a -> 'a -> bool) (lst : 'a list) =
  match lst with
  | [] -> []
  | h :: t -> let t = remove_dups comparator t in add_if_new comparator h t

(* check if two lists contain teh same set of items *)
let lists_equal (comparator : 'a -> 'a -> bool) (lst1 : 'a list) (lst2 : 'a list) : bool =
  (List.for_all (contains comparator lst2) lst1) &&
  (List.for_all (contains comparator lst1) lst2)

(* check if two lists contain the same item and return an integer 0 if equal, -1 if not equal *)
let lists_equal_int (comparator : 'a -> 'a -> bool) (lst1 : 'a list) (lst2 : 'a list) : int = if lists_equal comparator lst1 lst2 then 0 else -1

(* return set without any elements shared by set and remove *)
let cept (comparator : 'a -> 'a -> bool) (set : 'a list) (remove : 'a list) : 'a list = 
	List.fold_left (fun acc el -> if (contains comparator remove el) then acc else el::acc) [] set

(* union of two lists *)
let union (comparator : 'a -> 'a -> bool) (lst1 : 'a list) (lst2 : 'a list) : 'a list =
  List.append (cept comparator lst1 lst2) lst2

(* get the closure of a set of states using the given delta relation. I.e. takes any epsilon transitions available from teh given set of states *)
let rec closure (comparator : 'a -> 'a -> bool) (states : 'a list) (delta : ('a * char * 'a) list) : ('a list) = 
	let states' = List.fold_left (fun acc state -> 
			List.fold_left (fun acc2 delt -> 
				let (p,s,q) = delt in if (comparator p state) && (compare s ' ') == 0 then q::acc2 else acc2) 
			acc delta) 
		states states 
	in let states'' = remove_dups comparator states' in 
	if (List.length states) == (List.length states'') then states'' else closure comparator states'' delta 

(* uses the given delta relation to transition from a set of states to the set of reachable states with the given input *)
let goto (comparator : 'a -> 'a -> bool) (states : 'a list) (delta : ('a * char * 'a) list) (input : char) : ('a list) = 
	let new_states = List.fold_left (fun acc state -> 
		List.fold_left (fun acc2 delt -> 
			let (p,s,q) = delt in if (comparator p state) && (compare s input == 0) then q::acc2 else acc2) 
		acc delta) 
	[] states in 
	let dups_list = (closure comparator new_states delta) in 
	remove_dups comparator dups_list

(* flatten a list once *)
let flatten_once (lst : 'a list list) : 'a list =
  List.flatten (List.rev lst)

(* take the intersection of two sets *)
let intersect (comparator : 'a -> 'a -> bool) (lst1 : 'a list) (lst2 : 'a list) : 'a list =
	List.rev (List.filter (fun el1 -> contains comparator lst2 el1) lst1)

(* remove a set from a set of sets *)
let remove_set (comparator : 'a -> 'a -> bool) (set : 'a list list) (el : 'a list) : 'a list list =
  List.rev (List.filter (fun el' -> not (lists_equal comparator el el')) set)

(* check if a set is in a set of sets *)
let set_in (comparator : 'a -> 'a -> bool) (set : 'a list list) (el : 'a list) : bool = 
	contains (lists_equal comparator) set el

(* join to lists, a tail-recursive version of @ *)
let merge (lst1 : 'a list) (lst2 : 'a list) : 'a list = 
	List.rev_append lst1 lst2


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

(* get a variable that is not used in the current list of variables lst *)
let fresh_var (lst : string list) : string = let ls = LexStream.make () in
  let rec f () = let new_string = LexStream.next ls in
  if contains comparator_of_compare lst new_string then f () else new_string
  in f ()

(* create a new names for a set of variables and return the old set      *)
(* paired with the new names                                             *)
let fresh_states (lst1 : string list) (lst2 : string list) : (string * string) list =
  fst (List.fold_left (fun (acc, vars) el -> let f_v = fresh_var vars in ((el, f_v) :: acc, f_v :: vars)) ([], lst1) lst2)

(* recreate delta with the new variable names *)
let delta_fresh_vars (delta : (string * char * string) list) (assoc_list : (string * string) list) : (string * char * string) list =
  (List.fold_left (fun acc (p, a, q) -> (List.assoc p assoc_list, a, List.assoc q assoc_list):: acc) [] delta)

(* find the new name of a variable *)
let find_var (lst_pair : (string * string) list) (var : string) : string =
  List.assoc var lst_pair

let read_file (filename : string) : string = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done; ""
  with End_of_file ->
    close_in chan;
    String.concat "" (List.rev !lines)

(* let parse_dfa (s : string) : 'a DFA.t =
  Dfaparser.dfa_main Dfalexer.token (Lexing.from_string s) *)



