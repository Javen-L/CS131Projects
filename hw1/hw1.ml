type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
let subset a b =
	List.for_all (fun element -> (List.mem element b)) a;;
let equal_sets a b =
	((subset a b) && (subset b a));;
let set_union a b =
	List.append a b;;
let set_intersection a b =
	List.filter (fun element -> (List.mem element b)) a;;
let set_diff a b =
	List.filter (fun element -> not (List.mem element b)) a;;
let rec computed_fixed_point eq f x =
	let result = f x in
	if (eq result x) then x
	else computed_fixed_point eq f result;;
let filter_reachable g =
	let first (a, _) = a in
	let second (_, b) = b in
	let start = first g in
	let rules = second g in
	let rec reached array rules = match array with 
		[] -> []
		| [current] ->
			List.filter (fun element -> ((first element) = current)) rules
		| current::_::_ ->
			List.filter (fun element -> ((first element) = current)) rules
	in
	(* recursive function: start with start *)
	(* filter list for rules starting with start *)
	(* use second part of rules as input to more calls *)
	(* get difference of unions of output lists *)
	(* output the list of rules *)
	(start, reached [start] rules);;
