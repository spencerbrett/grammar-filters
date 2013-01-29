let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [3;2;1] [2;3])
let my_subset_test2 = subset [2;3] [3;2;1]

let my_proper_subset_test0 = proper_subset [] [3]
let my_proper_subset_test1 = not (proper_subset [3] [3;3;3])

let my_equal_sets_test0 = equal_sets ["cat";"dog"] ["dog";"cat"]
let my_equal_sets_test1 = not (equal_sets [1] [2])

let my_computed_fixed_point_test0 = 
	computed_fixed_point (=) (fun x -> x *. x) 2. = infinity 
let my_computed_fixed_point_test1 = 
	computed_fixed_point (=) (fun x -> x *. x) 1. = 1.

type paren_nonterminals = 
	| S | F

let paren_rules = 
	[ S, [N S; N S];
	  S, [T"("; T")"];
	  S, [T"("; N S ; T")"];
	  S, [T"["; T"]"];
	  S, [T"["; N S ; T"]"]]

let paren_grammar = S, paren_rules

let my_paren_test0 = filter_blind_alleys paren_grammar = paren_grammar

let my_paren_test1 = filter_blind_alleys (S, (S,[N F])::paren_rules) = paren_grammar


