type regexp =
	| Star of regexp
	| Add of regexp * regexp
	| Dot of regexp * regexp
	| Char of char



let test_regexp = Star (Add (Char 'a') (Dot (Char 'b') (Char 'a'))) in
let test_string = ['a';'b';'a'] in
let rec simulateREGEXP exp input : bool = 
	match (exp,input) with
	| (Star exp1,_) -> 
	| (Add exp1 exp2,_) ->
	| (Dot exp1 exp2,_) ->
	| (Char a1, a2) -> (compare a1 a2) == 0
in simulateREGEXP test_regexp test_string
