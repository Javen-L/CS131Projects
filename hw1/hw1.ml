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
		| current::rest ->
			(* extract the rules from the first element *)
			let these = List.filter (fun element -> ((first element) = current)) rules in

			(* take the first elements from the list uniquely *)
			let appendifnot el arr = match el with
				N name -> if (List.mem name arr) then arr else name::arr
				| T _ -> arr
			in
			let getuniques tuple arr =
				List.fold_right appendifnot (second tuple) arr
			in
			let next = List.fold_right getuniques these [] in
			(* remove found rules from rules *)
			let newrules = set_diff rules these in

			(* apply this function to them *)
			let results = reached next newrules in

			(* combine the result of that to this *)
			let these2 = set_union these results in

			(* remove the elements of next from the rest *)
			let newnext = set_diff rest next in

			(* remove found rules from newrules *)
			let newnewrules = set_diff newrules these2 in

			(* apply this function to rest of this array *)
			let results = reached newnext newnewrules in

			(* combine the result of that to this *)
			let these3 = set_union results these2 in
			these3
	in
	(start, set_intersection rules (reached [start] rules));;
