let my_subset_test0 = subset [2] [2;1];;
let my_subset_test1 = subset [2;2;2] [2;1];;
let my_subset_test2 = not (subset [5;2] [2;1]);;
let my_subset_test3 = not (subset [5] [2]);;
let my_subset_test4 = not (subset [5] []);;
let my_subset_test5 = subset [] [2];;
let my_subset_test6 = subset [] [];;
let my_subset_test7 = subset [2] [2];;

let my_equal_sets_test0 = not (equal_sets [2] [2;1]);;
let my_equal_sets_test1 = equal_sets [2] [2];;
let my_equal_sets_test2 = equal_sets [2;3;5] [2;3;5];;
let my_equal_sets_test3 = equal_sets [2;3;5] [2;3;2;5];;
let my_equal_sets_test4 = not (equal_sets [2] []);;
let my_equal_sets_test5 = equal_sets [] [];;
let my_equal_sets_test6 = not (equal_sets [] [2]);;
let my_equal_sets_test7 = equal_sets [2;1;3;2;3;2;1;2] [1;2;3];;

let my_set_union_test0 = equal_sets (set_union [] []) [];;
let my_set_union_test1 = equal_sets (set_union [1] []) [1];;
let my_set_union_test2 = equal_sets (set_union [] [1]) [1];;
let my_set_union_test3 = equal_sets (set_union [1] [2]) [1;2];;
let my_set_union_test4 = equal_sets (set_union [1;2] [2]) [1;2;2];;
let my_set_union_test5 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6];;

let my_set_intersection_test0 = equal_sets (set_intersection [] []) [];;
let my_set_intersection_test1 = equal_sets (set_intersection [] [1]) [];;
let my_set_intersection_test2 = equal_sets (set_intersection [1] []) [];;
let my_set_intersection_test3 = equal_sets (set_intersection [1] [1]) [1];;
let my_set_intersection_test4 = equal_sets (set_intersection [1] [2]) [];;
let my_set_intersection_test5 = equal_sets (set_intersection [1] [1;2]) [1];;
let my_set_intersection_test6 = equal_sets (set_intersection [1;2] [1]) [1];;

let my_set_diff_test0 = equal_sets (set_diff [] []) [];;
let my_set_diff_test1 = equal_sets (set_diff [] [1]) [];;
let my_set_diff_test2 = equal_sets (set_diff [1] []) [1];;
let my_set_diff_test3 = equal_sets (set_diff [1] [1]) [];;
let my_set_diff_test4 = equal_sets (set_diff [1;2] [1]) [2];;
let my_set_diff_test5 = equal_sets (set_diff [1] [1;2]) [];;

let my_computed_fixed_point_test0 = 
	let equality x y = x == y in
	(computed_fixed_point equality (fun x -> (x/2)) 2) == 0;;
let my_computed_fixed_point_test1 = equal_sets (computed_fixed_point equal_sets (fun list -> (set_intersection list list)) [3;5;5]) [3;5;5];;
let my_computed_fixed_point_test2 = equal_sets (computed_fixed_point equal_sets (fun list -> (set_diff list [5])) [3;5;5]) [3];;
let my_computed_fixed_point_test3 = 
	let equality x y = x == y in
	(computed_fixed_point equality (fun x -> (x/2)) 10394023) == 0;;

let my_filter_reachable_test0 = filter_reachable (N None, []) = (N None, []);;
type rfc_nonterminals =
	| Msgid | Idright | Nofoldliteral | Dtexts | Dtext | Dotatomtext | Atextplus | Dotwords
let rfc_grammar = (Msgid,
	[Msgid, [T "<"; N Dotatomtext; T "@"; N Idright; T ">"];
	Idright, [N Dotatomtext];
	Idright, [N Nofoldliteral];
	Nofoldliteral, [T "["; N Dtexts; T "]"];
	Dtexts, [];
	Dtexts, [N Dtext; N Dtexts];
	Dtext, [T "something goes here"];
	Dotatomtext, [N Atextplus; N Dotwords];
	Atextplus, [N Dtext];
	Atextplus, [N Dtext; N Atextplus];
	Dotwords, [T "."; N Atextplus; N Dotwords];
	Dotwords, [];
	]);;
let my_filter_reachable_test1 = (filter_reachable rfc_grammar) = rfc_grammar;;
let rfc_grammar2 = (Dtext,
	[Msgid, [T "<"; N Dotatomtext; T "@"; N Idright; T ">"];
	Idright, [N Dotatomtext];
	Idright, [N Nofoldliteral];
	Nofoldliteral, [T "["; N Dtexts; T "]"];
	Dtexts, [];
	Dtexts, [N Dtext; N Dtexts];
	Dtext, [T "something goes here"];
	Dotatomtext, [N Atextplus; N Dotwords];
	Atextplus, [N Dtext];
	Atextplus, [N Dtext; N Atextplus];
	Dotwords, [T "."; N Atextplus; N Dotwords];
	Dotwords, [];
	]);;
let my_filter_reachable_test2 = (filter_reachable rfc_grammar2) = (Dtext,
	[Dtext, [T "something goes here"]]);;
let rfc_grammar3 = (Dtexts,
	[Msgid, [T "<"; N Dotatomtext; T "@"; N Idright; T ">"];
	Idright, [N Dotatomtext];
	Idright, [N Nofoldliteral];
	Nofoldliteral, [T "["; N Dtexts; T "]"];
	Dtexts, [];
	Dtexts, [N Dtext; N Dtexts];
	Dtext, [T "something goes here"];
	Dotatomtext, [N Atextplus; N Dotwords];
	Atextplus, [N Dtext];
	Atextplus, [N Dtext; N Atextplus];
	Dotwords, [T "."; N Atextplus; N Dotwords];
	Dotwords, [];
	]);;
let my_filter_reachable_test3 = (filter_reachable rfc_grammar3) = (Dtexts,
	[Dtexts, [];
	Dtexts, [N Dtext; N Dtexts];
	Dtext, [T "something goes here"]]);;
