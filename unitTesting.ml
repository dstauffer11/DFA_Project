
open Util
open Definitions
open NFAtoDFA
open MinimizeDFA
open DifferenceDFA
open REXPtoDFA
open DFAtoREXP
open OUnit

(* Shared module testing *)
let alphabet = ['a';'b']
let test = assert_equal (contains comparator_of_compare alphabet 'a') true;
	assert_equal (contains comparator_of_compare alphabet 'c') false


let test_lists_equal = assert_equal (lists_equal comparator_of_compare ['a';'b'] ['b';'a']) true;
	assert_equal (lists_equal comparator_of_compare ['a';'b'] ['b';'a';'b']) true;
	assert_equal (lists_equal comparator_of_compare ['a';'b';'c'] ['b';'a']) false;
	assert_equal (lists_equal comparator_of_compare ['a';'b'] ['b';'a';'c']) false

let test_remove_dups =
  	assert_equal (remove_dups comparator_of_compare ["a";"b";"b";"c"]) ["a";"b";"c"];
	assert_equal (remove_dups comparator_of_compare []) [];
	assert_equal (remove_dups comparator_of_compare ["a";"a";"a";"a"]) ["a"]

let test_union =
  	assert_equal ~msg:"test_union 1" (union comparator_of_compare ["1";"2";"3"] ["4";"5";"6"]) ["3";"2";"1";"4";"5";"6"];
	assert_equal ~msg:"test_union 2" (union comparator_of_compare ["1";"2";"3"] ["1";"2";"3"]) ["1";"2";"3"];
 	assert_equal ~msg:"test_union 3" (union comparator_of_compare ["1";"2";"3"] ["1"]) ["3";"2";"1"];
	assert_equal ~msg:"test_union 4" (union comparator_of_compare ["1"] ["4";"5";"6"]) ["1";"4";"5";"6"];
	assert_equal ~msg:"test_union 5" (union comparator_of_compare [] []) []




let delta = [("p",'a',"q");("q",' ',"r")]
let delta2 = [("p",'a',"q");("q",'b',"r")]
let test_goto_closure = assert_equal (goto ["p"] delta 'a' comparator_of_compare) ["r";"q"];
	assert_equal (goto ["p"] delta 'b' comparator_of_compare) [];
	assert_equal (goto ["p"] delta2 'a' comparator_of_compare) ["q"];
	assert_equal (goto ["q"] delta2 'b' comparator_of_compare) ["r"]

let test_flatten_once = assert_equal (flatten_once [["a"];["b"];["c"]]) ["c";"b";"a"]

(* Functions to test the structural equality of two dfas *)
let delta_equality = (fun delta1 delta2 comparator -> 
	(List.fold_left (fun acc1 (p1,s1,q1) -> if (List.fold_left (fun acc2 (p2,s2,q2) -> 
		if (comparator p1 p2) && (comparator_of_compare s1 s2) && (comparator q1 q2) then true else acc2) false delta2) then acc1 else false) true delta1) 
	&& (List.fold_left (fun acc1 (p2,s2,q2) -> if (List.fold_left (fun acc2 (p1,s1,q1) -> 
		if (comparator p1 p2) && (comparator_of_compare s1 s2) && (comparator q1 q2) then true else acc2) false delta1) then acc1 else false) true delta2))
let dfa_equality : 'a DFA.t -> 'a DFA.t -> bool = (fun dfa1 dfa2 -> let (states1,alphabet1,delta1,s1,accept_states1,c1) = DFA.deconstruct dfa1 
	and (states2,alphabet2,delta2,s2,accept_states2,c2) = DFA.deconstruct dfa2 in 
	if (lists_equal c1 states1 states2) && (lists_equal comparator_of_compare alphabet1 alphabet2) && (delta_equality delta1 delta2 c1) && (c1 s1 s2) 
		&& (lists_equal c1 accept_states1 accept_states2) then true else false)
let test_dfa : string DFA.t = DFA.build ["p"] alphabet delta "p" [] comparator_of_compare
let test_dfa2 : string DFA.t = DFA.build ["p";"q";"r"] alphabet delta2 "p" ["p"] comparator_of_compare
let test_dfa2' : string DFA.t = DFA.build ["r";"q";"p"] alphabet (List.rev delta2) "p" ["p"] comparator_of_compare
let test_dfa_equality = assert_equal (dfa_equality test_dfa test_dfa) true;
	assert_equal (dfa_equality test_dfa2 test_dfa2) true;
	assert_equal (dfa_equality test_dfa test_dfa2) false

let () = Printf.printf "Shared ok\n"



(* Test the NFAtoDFA module *)
let test_nfa : int NFA.t = NFA.build [1;2] alphabet [(1,'a',1);(1,'b',2);(2,'b',1)] 1 [1] comparator_of_compare
let test_nfa2 : int NFA.t = NFA.build [1;2;3] alphabet [(1,' ',3);(1,'a',3);(3,'b',1);(3,'b',3)] 1 [3] comparator_of_compare
let test_nfa_to_dfa : int list DFA.t = DFA.build [[1];[2];[]] alphabet [([1],'a',[1]);([1],'b',[2]);([2],'a',[]);([2],'b',[1]);([],'a',[]);([],'b',[])] 
	[1] [[1]] (lists_equal comparator_of_compare)
let test_nfa2_to_dfa : int list DFA.t = DFA.build [[1;3];[3];[]] alphabet [([1;3],'a',[3]);([1;3],'b',[1;3]);([3],'a',[]);([3],'b',[1;3]);([],'a',[]);([],'b',[])] 
	[1;3] [[1;3];[3]] (lists_equal comparator_of_compare)
let test_convert_NFA_to_DFA = assert_equal ~cmp:dfa_equality (NFAtoDFA.convert_NFA_to_DFA test_nfa) test_nfa_to_dfa;
	assert_equal ~cmp:dfa_equality (NFAtoDFA.convert_NFA_to_DFA test_nfa2) test_nfa2_to_dfa;
	assert_equal (dfa_equality (NFAtoDFA.convert_NFA_to_DFA test_nfa2) test_nfa_to_dfa) false

(* More testing of the Shared module functions *)
let test_intersect = assert_equal (intersect comparator_of_compare ["1";"2";"3"] []) [];
	assert_equal (intersect comparator_of_compare ["1";"2";"3"] ["1";"2";"3"]) ["3";"2";"1"];
	assert_equal (intersect comparator_of_compare ["1";"2";"3"] ["3";"2"]) ["3";"2"];
	assert_equal (intersect comparator_of_compare ["4"] ["1";"2";"3"]) []


let lstlst1 = [["a";"b";"c"];["a"];["b";"c"]]
let lst1 = ["a";"b";"c"]
let lst2 = ["b";"c"]
let test_remove_set = assert_equal (remove_set comparator_of_compare lstlst1 lst1) [["b";"c"];["a"]];
	assert_equal (remove_set comparator_of_compare lstlst1 []) (List.rev lstlst1);
	assert_equal (remove_set comparator_of_compare [] lst2) [];
	assert_equal (remove_set comparator_of_compare lstlst1 lst2) [["a"];["a";"b";"c"]];
	assert_equal (remove_set comparator_of_compare lstlst1 ("d"::lst2)) (List.rev lstlst1)

let test_set_in = assert_equal (set_in comparator_of_compare lstlst1 lst1) true;
	assert_equal (set_in comparator_of_compare lstlst1 ("d"::lst2)) false;
	assert_equal (set_in comparator_of_compare lstlst1 []) false;
	assert_equal (set_in comparator_of_compare lstlst1 lst2) true

let test_cept = assert_equal (cept comparator_of_compare ["1";"2";"3"] []) ["3";"2";"1"];
	assert_equal (cept comparator_of_compare ["1";"2";"3"] ["1";"2";"3"]) [];
	assert_equal (cept comparator_of_compare ["1";"2";"3"] ["3";"2"]) ["1"];
	assert_equal (cept comparator_of_compare ["4"] ["1";"2";"3"]) ["4"]


let () = Printf.printf "NFAtoDFA ok\n"






(* Test the MinimizeDFA module *)
let delta_long1 = [(1,'a',2);(1,'b',6);(2,'a',3);(2,'b',2);(3,'a',3);(3,'b',4);(4,'a',5);(4,'b',4);(5,'a',5);(5,'b',6);(6,'a',6);(6,'b',6)]
let delta_long1_list = [([1],'a',[2]);([1],'b',[4;5;6]);([2],'a',[3]);([2],'b',[2]);([3],'a',[3]);([3],'b',[4;5;6]);([4;5;6],'a',[4;5;6]);([4;5;6],'b',[4;5;6])]
let test_long_dfa1 : int DFA.t = DFA.build [1;2;3;4;5;6] alphabet delta_long1 1 [1;2;4;5;6] comparator_of_compare
let test_long_dfa1_minimized : int list DFA.t = 
	DFA.build [[1];[2];[3];[4;5;6]] alphabet delta_long1_list [1] [[1];[2];[4;5;6]] (lists_equal comparator_of_compare)
let delta_long2 = [(1,'a',2);(1,'b',6);(2,'a',3);(2,'b',2);(3,'a',3);(3,'b',4);(4,'a',5);(4,'b',4);(5,'a',5);(5,'b',6);(6,'a',6);(6,'b',6)]
let delta_long2_list = List.map (fun (p,s,q)->([p],s,[q])) delta_long2
let test_long_dfa2 : int DFA.t = DFA.build [1;2;3;4;5;6] alphabet delta_long2 1 [1;2;4;6] comparator_of_compare
let test_long_dfa2_minimized : int list DFA.t = 
	DFA.build [[1];[2];[3];[4];[5];[6]] alphabet delta_long2_list [1] [[1];[2];[4];[6]] (lists_equal comparator_of_compare)
let test_find_reachable_states =
	assert_equal ~cmp:(lists_equal comparator_of_compare) (MinimizeDFA.find_reachable_states test_long_dfa1 [1]) [1;2;3;4;5;6];
	assert_equal ~cmp:(lists_equal comparator_of_compare) (MinimizeDFA.find_reachable_states test_long_dfa2 [1]) [1;2;3;4;5;6]

let test_reachable_dfa = assert_equal ~cmp:dfa_equality (MinimizeDFA.reachable_dfa test_long_dfa1 [1;2;3;4;5;6]) test_long_dfa1;
	assert_equal ~cmp:dfa_equality (MinimizeDFA.reachable_dfa test_long_dfa2 [1;2;3;4;5;6]) test_long_dfa2

let test_generate_transition_to_states =
	assert_equal (MinimizeDFA.generate_transition_to_states [1;2;3;4;5;6] delta_long1 [3] 'b' comparator_of_compare) [];
	assert_equal (MinimizeDFA.generate_transition_to_states [1;2;3;4;5;6] delta_long2 [3] 'a' comparator_of_compare) [3;2];
	assert_equal (MinimizeDFA.generate_transition_to_states [1;2;3;4;5;6] delta_long1 [3;2] 'a' comparator_of_compare) [3;2;1];
	assert_equal (MinimizeDFA.generate_transition_to_states [1;2;3;4;5;6] delta_long2 [3;2] 'b' comparator_of_compare) [2];
	assert_equal (MinimizeDFA.generate_transition_to_states [1;2;3;4;5;6] delta_long2 [] 'b' comparator_of_compare) []

let x = ["1";"2"]
let y = ["2";"3"]
let w = [["1";"4"];["2";"3"]]
let p = [["1";"4"];["2";"3"]]
let w' = [["1";"3"];["2";"4"]]
let test_manipulate_w = assert_equal (MinimizeDFA.manipulate_w x y w comparator_of_compare) [["3"];["2"];["1";"4"]];
	assert_equal (MinimizeDFA.manipulate_w x y w' comparator_of_compare) [["2"];["1";"3"];["2";"4"]]

let test_set_of = assert_equal (MinimizeDFA.set_of [["a";"b";"d"];["c"];["e"]] "e" comparator_of_compare) ["e"];
	assert_equal (MinimizeDFA.set_of [["a";"b";"d"];["c"];["e"]] "f" comparator_of_compare) [];
	assert_equal (MinimizeDFA.set_of [["a";"b";"d"];["c"];["e"]] "b" comparator_of_compare) ["a";"b";"d"]

let test_delta_goto = assert_equal (MinimizeDFA.delta_goto delta_long1 [3] 'a' comparator_of_compare) 3;
	assert_equal (MinimizeDFA.delta_goto delta_long2 [1] 'b' comparator_of_compare) 6;
	assert_equal (MinimizeDFA.delta_goto delta_long2 [1;5;6] 'b' comparator_of_compare) 6;
	assert_equal (MinimizeDFA.delta_goto delta_long1 [1] 'a' comparator_of_compare) 2

let test_delta_list_goto = assert_equal (MinimizeDFA.delta_list_goto [([1;2],'a',[1;2]);([2;1],'b',[2;1])] [1;2] 'a' comparator_of_compare) [1;2];
	assert_equal (MinimizeDFA.delta_list_goto [([1;2],'a',[1;2]);([2;1],'b',[2;1])] [1;2] 'b' comparator_of_compare) [2;1]

let test_create_delta = assert_equal ~cmp:(lists_equal comparator_of_compare) 
		(MinimizeDFA.create_delta test_long_dfa1 [[1];[2];[3];[4;5;6]]) delta_long1_list;
	assert_equal ~cmp:(lists_equal comparator_of_compare)
		(MinimizeDFA.create_delta test_long_dfa2 [[1];[2];[3];[4];[5];[6]]) delta_long2_list

let test_build_dfa_of_dfa = assert_equal ~cmp:dfa_equality (MinimizeDFA.build_dfa_of_dfa test_long_dfa1 [[1];[2];[3];[4;5;6]]) test_long_dfa1_minimized;
	assert_equal ~cmp:dfa_equality (MinimizeDFA.build_dfa_of_dfa test_long_dfa2 [[1];[2];[3];[4];[5];[6]]) test_long_dfa2_minimized

let p = [["p"];["q";"r"]]
let w = [["p"];["q";"r"]]
let test_create_delta_dfa : string DFA.t = DFA.build ["p";"q";"r"] alphabet 
	[("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["q";"r"] comparator_of_compare
let test_create_delta_dfa2 : string DFA.t = DFA.build ["p";"q";"r"] alphabet
	[("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["r"] comparator_of_compare
let test_create_delta = assert_equal (MinimizeDFA.create_delta test_create_delta_dfa p) [(["q"; "r"], 'b', ["q"; "r"]); (["q"; "r"], 'a', ["q"; "r"]);
 (["p"], 'b', ["q"; "r"]); (["p"], 'a', ["q"; "r"])]
let test_build_dfa = assert_equal ~cmp:dfa_equality (MinimizeDFA.build_dfa_of_dfa test_create_delta_dfa p) (DFA.build [["p"];["q";"r"]] alphabet
	(MinimizeDFA.create_delta test_create_delta_dfa p) ["p"] [["q";"r"]] comparator_of_compare)

let test_partition_states_helper = assert_equal ~cmp:dfa_equality 
	(MinimizeDFA.partition_states_helper test_long_dfa1 [[1];[2];[3];[4;5;6]] [] [1;2;3;4;5;6]) test_long_dfa1_minimized;
	assert_equal ~cmp:dfa_equality (MinimizeDFA.partition_states_helper test_long_dfa2 [[1];[2];[3];[4];[5];[6]] [] [1;2;3;4;5;6]) test_long_dfa2_minimized

let test_partition_state = assert_equal ~cmp:dfa_equality (MinimizeDFA.partition_states test_long_dfa1  [1;2;3;4;5;6]) test_long_dfa1_minimized;
	assert_equal ~cmp:dfa_equality (MinimizeDFA.partition_states test_long_dfa2 [1;2;3;4;5;6]) test_long_dfa2_minimized

let test_minimize = assert_equal ~cmp:dfa_equality (MinimizeDFA.minimize test_long_dfa1) test_long_dfa1_minimized;
	assert_equal ~cmp:dfa_equality (MinimizeDFA.minimize test_long_dfa2) test_long_dfa2_minimized

let test_equivalence_test_dfa = assert_equal (MinimizeDFA.equivalence_test_dfa test_long_dfa1 test_long_dfa2) false;
	assert_equal (MinimizeDFA.equivalence_test_dfa test_long_dfa2 test_long_dfa1) false;
	assert_equal (MinimizeDFA.equivalence_test_dfa test_long_dfa1 test_long_dfa1) true;
	assert_equal (MinimizeDFA.equivalence_test_dfa test_long_dfa2 test_long_dfa2) true

let test_long_nfa1 = let (a,b,c,d,e,f) = (DFA.deconstruct test_long_dfa1) in NFA.build a b c d e f
let test_long_nfa2 = let (a,b,c,d,e,f) = (DFA.deconstruct test_long_dfa2) in NFA.build a b c d e f
let fake_test_equivalence_test_nfa = assert_equal (MinimizeDFA.equivalence_test_nfa test_long_nfa1 test_long_nfa2) false


let () = Printf.printf "MinimizeDFA ok\n"








(* Test DifferenceDFA module *)
let test_invert_dfa = assert_equal ~cmp:dfa_equality (DifferenceDFA.invert_dfa test_create_delta_dfa) (DFA.build ["p";"q";"r"] alphabet
	[("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")] "p" ["p"] comparator_of_compare)

let test_delta_list = [("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")]
let test_delta_use = assert_equal (DifferenceDFA.delta_use test_delta_list "p" 'a' comparator_of_compare) "q"

let states_tracker_list = [("p",false);("q",true);("r",false)]
let test_visit_state = assert_equal (DifferenceDFA.visit_state states_tracker_list "q" comparator_of_compare) (List.rev states_tracker_list);
	assert_equal (DifferenceDFA.visit_state states_tracker_list "r" comparator_of_compare) [("r",true);("q",true);("p",false)]

let test_find_difference_in_dfas =
	assert_raises DifferenceDFA.DFAs_Equivalent (fun () -> DifferenceDFA.find_difference_in_dfas test_create_delta_dfa test_create_delta_dfa);
	assert_equal (DifferenceDFA.find_difference_in_dfas test_create_delta_dfa test_create_delta_dfa2) ['a'];
	assert_equal (DifferenceDFA.find_difference_in_dfas test_long_dfa1 test_long_dfa2) ['a';'a';'b';'a'];
	assert_equal (DifferenceDFA.find_difference_in_dfas test_long_dfa2 test_long_dfa1) ['a';'a';'b';'a']


let complicated_delta = [(1,'a',2);(1,'b',3);(2,'a',2);(2,'b',2);(3,'a',4);(3,'b',3);(4,'a',5);(4,'b',6);(5,'a',6);(5,'b',6);(6,'a',1);(6,'b',6)]
let complicated_dfa1 : int DFA.t = DFA.build [1;2;3;4;5;6] alphabet complicated_delta 1 [1;4;5] comparator_of_compare
let complicated_dfa2 : int DFA.t = DFA.build [1;2;3;4;5;6] alphabet complicated_delta 1 [1;4;5;6] comparator_of_compare

let test2_find_difference_in_dfas = assert_equal (DifferenceDFA.find_difference_in_dfas complicated_dfa1 complicated_dfa2) ['b';'a';'a';'a']

let () = Printf.printf "DifferenceDFA ok\n"



(* Test the REXPtoDFA Module *)
let test_regex1 : regex = Star (Concat (Character 'a',Character 'b'))
let test_regex2 : regex = Or (Epsilon,Star (Character 'b'))
let test_regex1_to_nfa :  string NFA.t = 
	NFA.build ["a";"b";"c";"d";"e"] alphabet [("e",' ',"d");("e",' ',"a");("a",'a',"b");("b",' ',"c");("c",'b',"d");("d",' ',"e")] "e" ["d"] comparator_of_compare
let test_regex1_to_dfa : string list DFA.t = NFAtoDFA.convert_NFA_to_DFA (test_regex1_to_nfa)
let test_regex2_to_dfa : string list DFA.t =
	(DFA.build [["b"; "f"; "c"; "d"]; ["b"; "a"; "f"; "c"; "d"; "e"]] ['b'] [(["b"; "f"; "c"; "d"], 'b', ["b"; "f"; "c"; "d"]);
  		(["b"; "a"; "f"; "c"; "d"; "e"], 'b', ["b"; "f"; "c"; "d"])]
 		["b"; "a"; "f"; "c"; "d"; "e"] [["b"; "a"; "f"; "c"; "d"; "e"]; ["b"; "f"; "c"; "d"]] (lists_equal comparator_of_compare))

let test_regex_to_nfa = assert_equal ~cmp:dfa_equality (REXPtoDFA.regex_to_dfa test_regex1) test_regex1_to_dfa;
	assert_equal ~cmp:dfa_equality (REXPtoDFA.regex_to_dfa test_regex2) test_regex2_to_dfa


let () = Printf.printf "REXPtoDFA ok\n"



(* Test the DFAtoREXP module *)

let test_build_index_vector = assert_equal (DFAtoREXP.build_index_vector 5 []) [0;1;2;3;4];
	assert_equal (DFAtoREXP.build_index_vector 0 []) [];
	assert_equal (DFAtoREXP.build_index_vector (-1) []) []

let test_transitions_finder = assert_equal (DFAtoREXP.transitions_finder complicated_dfa1 1 3) (true,Character 'b');
	assert_equal (DFAtoREXP.transitions_finder complicated_dfa1 1 5) (false,Emptyset);
	assert_equal (DFAtoREXP.transitions_finder test_long_dfa1 6 6) (true,Or (Character 'b',Or (Character 'a',Epsilon)))

let small_dfa : int DFA.t = DFA.build [1;2] ['a'] [(1,'a',2);(2,'a',2)] 1 [1] comparator_of_compare

let (matA,matB) = DFAtoREXP.build_matrices small_dfa
let test_build_matrices = assert_equal matB (Array.of_list [Epsilon;Emptyset]);
	assert_equal matA (Array.of_list [Array.of_list [Emptyset;Character 'a'];Array.of_list [Emptyset;Or (Character 'a',Epsilon)]])

let solution = DFAtoREXP.solve_matrices small_dfa matA matB
let test_solve_matrices = assert_equal (basic_simplify solution.(1)) Emptyset;
	assert_equal (basic_simplify solution.(0)) Epsilon


let so_dfa : int DFA.t = DFA.build [0;1;2] ['a';'b'] [(0,'a',1);(0,'b',2);(1,'a',2);(1,'b',0);(2,'a',0);(2,'b',1)] 0 [0] comparator_of_compare
let so_solution = DFAtoREXP.dfa_to_regex so_dfa
let test_dfa_to_regex = assert_equal (basic_simplify so_solution) 
	(Star (Or
   		(Concat (Or (Concat (Character 'b', Character 'b'), Character 'a'),
     		Concat (Star (Concat (Character 'a', Character 'b')),
      			Or (Concat (Character 'a', Character 'a'), Character 'b'))),
   		Concat (Character 'b', Character 'a'))))


let () = Printf.printf "DFAtoREXP ok\n"



