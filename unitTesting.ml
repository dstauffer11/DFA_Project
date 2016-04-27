open Definitions
open Definitions
open Shared
open Shared
open NFAtoDFA
open MinimizeDFA
open DifferenceDFA
open OUnit

(* Shared module testing *)
let test = assert_equal alphabet ['a';'b'];
			assert_equal (contains 'a' alphabet comparator_of_compare) true;
			assert_equal (contains 'c' alphabet comparator_of_compare) false


let test_lists_equal = assert_equal (lists_equal ['a';'b'] ['b';'a'] comparator_of_compare) true;
	assert_equal (lists_equal ['a';'b'] ['b';'a';'b'] comparator_of_compare) true;
	assert_equal (lists_equal ['a';'b';'c'] ['b';'a'] comparator_of_compare) false;
	assert_equal (lists_equal ['a';'b'] ['b';'a';'c'] comparator_of_compare) false

let test_remove_dups = assert_equal (remove_dups ["a";"b";"b";"c"] comparator_of_compare) ["a";"b";"c"];
	assert_equal (remove_dups [] comparator_of_compare) [];
	assert_equal (remove_dups ["a";"a";"a";"a"] comparator_of_compare) ["a"]

let test_union = assert_equal (union ["1";"2";"3"] ["4";"5";"6"] comparator_of_compare) ["3";"2";"1";"4";"5";"6"];
	assert_equal (union ["1";"2";"3"] ["1";"2";"3"] comparator_of_compare) ["1";"2";"3"];
	assert_equal (union ["1";"2";"3"] ["1"] comparator_of_compare) ["3";"2";"1"];
	assert_equal (union ["1"] ["4";"5";"6"] comparator_of_compare) ["1";"4";"5";"6"];
	assert_equal (union [] [] comparator_of_compare) []




let delta = [("p",'a',"q");("q",' ',"r")]
let delta2 = [("p",'a',"q");("q",'b',"r")]
let test_goto_closure = assert_equal (goto ["p"] delta 'a' comparator_of_compare) ["r";"q"];
	assert_equal (goto ["p"] delta 'b' comparator_of_compare) [];
	assert_equal (goto ["p"] delta2 'a' comparator_of_compare) ["q"];
	assert_equal (goto ["q"] delta2 'b' comparator_of_compare) ["r"]

let test_flatten_once = assert_equal (flatten_once [["a"];["b"];["c"]]) ["c";"b";"a"]


(* Test the NFAtoDFA module *)


(* Test MinimizeDFA module *)
let test_dfa2 : string DFA.dfa = DFA.build_dfa ["p";"q";"r"] delta2 "p" ["p"] comparator_of_compare
let test_dfa : string DFA.dfa = DFA.build_dfa ["p"] delta "p" [] comparator_of_compare
let reachable_states2 : string list = MinimizeDFA.find_reachable_states test_dfa2 ["p"]
let delta_equality = (fun delta1 delta2 comparator -> 
	(List.fold_left (fun acc1 (p1,s1,q1) -> if (List.fold_left (fun acc2 (p2,s2,q2) -> 
		if (comparator p1 p2) && (comparator_of_compare s1 s2) && (comparator q1 q2) then true else acc2) false delta2) then acc1 else false) true delta1) 
	&& (List.fold_left (fun acc1 (p2,s2,q2) -> if (List.fold_left (fun acc2 (p1,s1,q1) -> 
		if (comparator p1 p2) && (comparator_of_compare s1 s2) && (comparator q1 q2) then true else acc2) false delta1) then acc1 else false) true delta2))
let dfa_equality : 'a DFA.dfa -> 'a DFA.dfa -> bool = (fun dfa1 dfa2 -> let (states1,delta1,s1,accept_states1,c1) = DFA.deconstruct_dfa dfa1 
	and (states2,delta2,s2,accept_states2,c2) = DFA.deconstruct_dfa dfa2 in 
	if (lists_equal states1 states2 c1) && (delta_equality delta1 delta2 c1) && (c1 s1 s2) 
		&& (lists_equal accept_states1 accept_states2 c1) then true else false)
let test_find_reachable_states = assert_equal reachable_states2 ["r";"q";"p"];
	assert_equal (MinimizeDFA.find_reachable_states test_dfa ["p"]) ["q";"r";"p"]

let test_dfa2' : string DFA.dfa = DFA.build_dfa ["r";"q";"p"] (List.rev delta2) "p" ["p"] comparator_of_compare
let test_dfa_equality = assert_equal (dfa_equality test_dfa test_dfa) true;
	assert_equal (dfa_equality test_dfa2 test_dfa2) true;
	assert_equal (dfa_equality test_dfa test_dfa2) false

let test_reachable_dfa = assert_equal ~cmp:dfa_equality (MinimizeDFA.reachable_dfa test_dfa2 reachable_states2) test_dfa2'

let test_cept = assert_equal (cept ["1";"2";"3"] [] comparator_of_compare) ["3";"2";"1"];
	assert_equal (cept ["1";"2";"3"] ["1";"2";"3"] comparator_of_compare) [];
	assert_equal (cept ["1";"2";"3"] ["3";"2"] comparator_of_compare) ["1"];
	assert_equal (cept ["4"] ["1";"2";"3"] comparator_of_compare) ["4"]


(* more Shared module testing *)
let test_intersect = assert_equal (intersect ["1";"2";"3"] [] comparator_of_compare) [];
	assert_equal (intersect ["1";"2";"3"] ["1";"2";"3"] comparator_of_compare) ["3";"2";"1"];
	assert_equal (intersect ["1";"2";"3"] ["3";"2"] comparator_of_compare) ["3";"2"];
	assert_equal (intersect ["4"] ["1";"2";"3"] comparator_of_compare) []

let test_generate_transition_to_states = assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["q"] comparator_of_compare) ["p"];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["q";"r"] comparator_of_compare) ["p";"q"];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["p"] comparator_of_compare) [];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["p";"q";"r"] comparator_of_compare) ["p";"q"]


let lstlst1 = [["a";"b";"c"];["a"];["b";"c"]]
let lst1 = ["a";"b";"c"]
let lst2 = ["b";"c"]
let test_remove_set = assert_equal (remove_set lstlst1 lst1 comparator_of_compare) [["b";"c"];["a"]];
	assert_equal (remove_set lstlst1 [] comparator_of_compare) (List.rev lstlst1);
	assert_equal (remove_set [] lst2 comparator_of_compare) [];
	assert_equal (remove_set lstlst1 lst2 comparator_of_compare) [["a"];["a";"b";"c"]];
	assert_equal (remove_set lstlst1 ("d"::lst2) comparator_of_compare) (List.rev lstlst1)

let test_set_in = assert_equal (set_in lstlst1 lst1 comparator_of_compare) true;
	assert_equal (set_in lstlst1 ("d"::lst2) comparator_of_compare) false;
	assert_equal (set_in lstlst1 [] comparator_of_compare) false;
	assert_equal (set_in lstlst1 lst2 comparator_of_compare) true


(* Test the MinimizeDFA module further *)
let x = ["1";"2"]
let y = ["2";"3"]
let w = [["1";"4"];["2";"3"]]
let p = [["1";"4"];["2";"3"]]
let w' = [["1";"3"];["2";"4"]]
let test_manipulate_w = assert_equal (MinimizeDFA.manipulate_w x y p w comparator_of_compare) [["3"];["2"];["1";"4"]];
	assert_equal (MinimizeDFA.manipulate_w x y p w' comparator_of_compare) [["2"];["1";"3"];["2";"4"]]

let p = [["p"];["q";"r"]]
let w = [["p"];["q";"r"]]
let test_create_delta_dfa : string DFA.dfa = DFA.build_dfa ["p";"q";"r"] [("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["q";"r"] 
	comparator_of_compare
let test_create_delta_dfa2 : string DFA.dfa = DFA.build_dfa ["p";"q";"r"] [("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["r"]
	comparator_of_compare
let test_create_delta = assert_equal (MinimizeDFA.create_delta test_create_delta_dfa p) [(["q"; "r"], 'b', ["q"; "r"]); (["q"; "r"], 'a', ["q"; "r"]);
 (["p"], 'b', ["q"; "r"]); (["p"], 'a', ["q"; "r"])]
let test_build_dfa = assert_equal ~cmp:dfa_equality (MinimizeDFA.build_dfa_of_dfa test_create_delta_dfa p) (DFA.build_dfa [["p"];["q";"r"]]
	(MinimizeDFA.create_delta test_create_delta_dfa p) ["p"] [["q";"r"]] comparator_of_compare)

let test_partition_states = assert_equal (MinimizeDFA.partition_states test_create_delta_dfa ["p";"q";"r"]) 





(* Test DifferenceDFA module *)
let test_invert_dfa = assert_equal ~cmp:dfa_equality (DifferenceDFA.invert_dfa test_create_delta_dfa) (DFA.build_dfa ["p";"q";"r"]
	[("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["p"] comparator_of_compare)


let test_delta_list = [("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")]
let test_delta_use = assert_equal (DifferenceDFA.delta_use test_delta_list "p" 'a' comparator_of_compare) "q"

let states_tracker_list = [("p",false);("q",true);("r",false)]
let test_visit_state = assert_equal (DifferenceDFA.visit_state states_tracker_list "q" comparator_of_compare) (List.rev states_tracker_list);
	assert_equal (DifferenceDFA.visit_state states_tracker_list "r" comparator_of_compare) [("r",true);("q",true);("p",false)]

let test_find_difference_in_dfas = assert_equal (DifferenceDFA.find_difference_in_dfas test_create_delta_dfa test_dfa2) [];
	assert_equal (DifferenceDFA.find_difference_in_dfas test_dfa2 test_create_delta_dfa) [];
	assert_raises DifferenceDFA.DFAs_Equivalent (fun () -> DifferenceDFA.find_difference_in_dfas test_create_delta_dfa test_create_delta_dfa)
	(*THIS has a bug!!!: assert_equal (DifferenceDFA.find_difference_in_dfas test_create_delta_dfa test_create_delta_dfa2) ['a']*)




(* Test the REXPtoDFA Module *)







