
open MinimizeDFA
open MinimizeDFA
open OUnit

let test = assert_equal MinimizeDFA.alphabet ['a';'b'];
			assert_equal (MinimizeDFA.contains 'a' MinimizeDFA.alphabet compare) true;
			assert_equal (MinimizeDFA.contains 'c' MinimizeDFA.alphabet compare) false


let test_lists_equal = assert_equal (MinimizeDFA.lists_equal ['a';'b'] ['b';'a']) true;
	assert_equal (MinimizeDFA.lists_equal ['a';'b'] ['b';'a';'b']) true;
	assert_equal (MinimizeDFA.lists_equal ['a';'b';'c'] ['b';'a']) false;
	assert_equal (MinimizeDFA.lists_equal ['a';'b'] ['b';'a';'c']) false

let test_remove_dups = assert_equal (MinimizeDFA.remove_dups ["a";"b";"b";"c"]) ["a";"b";"c"];
	assert_equal (MinimizeDFA.remove_dups []) [];
	assert_equal (MinimizeDFA.remove_dups ["a";"a";"a";"a"]) ["a"]

let test_union = assert_equal (MinimizeDFA.union ["1";"2";"3"] ["4";"5";"6"]) ["3";"2";"1";"4";"5";"6"];
	assert_equal (MinimizeDFA.union ["1";"2";"3"] ["1";"2";"3"]) ["1";"2";"3"];
	assert_equal (MinimizeDFA.union ["1";"2";"3"] ["1"]) ["3";"2";"1"];
	assert_equal (MinimizeDFA.union ["1"] ["4";"5";"6"]) ["1";"4";"5";"6"];
	assert_equal (MinimizeDFA.union [] []) []




let delta = [("p",'a',"q");("q",' ',"r")]
let delta2 = [("p",'a',"q");("q",'b',"r")]
let test_goto_closure = assert_equal (MinimizeDFA.goto ["p"] delta 'a') ["r";"q"];
	assert_equal (MinimizeDFA.goto ["p"] delta 'b') [];
	assert_equal (MinimizeDFA.goto ["p"] delta2 'a') ["q"];
	assert_equal (MinimizeDFA.goto ["q"] delta2 'b') ["r"]

let test_flatten_once = assert_equal (MinimizeDFA.flatten_once [["a"];["b"];["c"]]) ["c";"b";"a"]

let test_dfa2 = (MinimizeDFA.Dfa ("p",delta2,["p"]))
let test_dfa = (MinimizeDFA.Dfa ("p",delta,[]))
let reachable_states2 = MinimizeDFA.find_reachable_states test_dfa2 ["p"]
let test_find_reachable_states = assert_equal reachable_states2 ["r";"q";"p"];
	assert_equal (MinimizeDFA.find_reachable_states test_dfa ["p"]) ["q";"r";"p"]

let test_dfa2' = MinimizeDFA.Dfa ("p",List.rev delta2, ["p"])
let test_reachable_dfa = assert_equal (MinimizeDFA.reachable_dfa test_dfa2 reachable_states2) test_dfa2'

let test_cept = assert_equal (MinimizeDFA.cept ["1";"2";"3"] []) ["3";"2";"1"];
	assert_equal (MinimizeDFA.cept ["1";"2";"3"] ["1";"2";"3"]) [];
	assert_equal (MinimizeDFA.cept ["1";"2";"3"] ["3";"2"]) ["1"];
	assert_equal (MinimizeDFA.cept ["4"] ["1";"2";"3"]) ["4"]

let test_intersect = assert_equal (MinimizeDFA.intersect ["1";"2";"3"] []) [];
	assert_equal (MinimizeDFA.intersect ["1";"2";"3"] ["1";"2";"3"]) ["3";"2";"1"];
	assert_equal (MinimizeDFA.intersect ["1";"2";"3"] ["3";"2"]) ["3";"2"];
	assert_equal (MinimizeDFA.intersect ["4"] ["1";"2";"3"]) []

let test_generate_transition_to_states = assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["q"]) ["p"];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["q";"r"]) ["p";"q"];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["p"]) [];
	assert_equal (MinimizeDFA.generate_transition_to_states reachable_states2 delta2 ["p";"q";"r"]) ["p";"q"]

(* Test set_of and delta_goto*)


let lstlst1 = [["a";"b";"c"];["a"];["b";"c"]]
let lst1 = ["a";"b";"c"]
let lst2 = ["b";"c"]
let test_remove_set = assert_equal (MinimizeDFA.remove_set lstlst1 lst1) [["b";"c"];["a"]];
	assert_equal (MinimizeDFA.remove_set lstlst1 []) (List.rev lstlst1);
	assert_equal (MinimizeDFA.remove_set [] lst2) [];
	assert_equal (MinimizeDFA.remove_set lstlst1 lst2) [["a"];["a";"b";"c"]];
	assert_equal (MinimizeDFA.remove_set lstlst1 ("d"::lst2)) (List.rev lstlst1)

let test_set_in = assert_equal (MinimizeDFA.set_in lstlst1 lst1) true;
	assert_equal (MinimizeDFA.set_in lstlst1 ("d"::lst2)) false;
	assert_equal (MinimizeDFA.set_in lstlst1 []) false;
	assert_equal (MinimizeDFA.set_in lstlst1 lst2) true


let x = ["1";"2"]
let y = ["2";"3"]
let w = [["1";"4"];["2";"3"]]
let p = [["1";"4"];["2";"3"]]
let w' = [["1";"3"];["2";"4"]]
let test_manipulate_w = assert_equal (MinimizeDFA.manipulate_w x y p w) [["3"];["2"];["1";"4"]];
	assert_equal (MinimizeDFA.manipulate_w x y p w') [["2"];["1";"3"];["2";"4"]]

let p = [["p"];["q";"r"]]
let w = [["p"];["q";"r"]]
let test_create_delta_dfa = MinimizeDFA.Dfa ("p",[("p",'a',"q");("p",'b',"r");("r",'a',"q");("r",'b',"r");("q",'a',"r");("q",'b',"q")],["q";"r"])
let test_create_delta = assert_equal (MinimizeDFA.create_delta test_create_delta_dfa p) [(["q"; "r"], 'b', ["q"; "r"]); (["q"; "r"], 'a', ["q"; "r"]);
 (["p"], 'b', ["q"; "r"]); (["p"], 'a', ["q"; "r"])]

let test_build_dfa = assert_equal (MinimizeDFA.build_dfa test_create_delta_dfa p) (MinimizeDFA.Dfa (["p"],MinimizeDFA.create_delta test_create_delta_dfa p,
	[["q";"r"]]))

let test_partition_states = assert_equal (partition_states test_create_delta_dfa ["p";"q";"r"]) 




