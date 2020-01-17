type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
(* 'a * ('a * ('a, 'b) symbol list) list *)
(* 'a * fun 'a -> (('a, 'b) symbol list) list *)
let convert_grammar gram1 =
	let rules keys values term =
		let rec findindex key lst index = match lst with
			| [] -> -1
			| this::rest -> if(this = key) then index else findindex key rest (index+1)
		in
		let matcher key value input = List.nth value (findindex input key 0)
		in
		matcher keys values term
	in
	let first = fst gram1 in
	let second = snd gram1 in
	let fold_func (a1, a2) (b1, b2) =
		let rec findindex key lst index = match lst with
			| [] -> -1
			| this::rest -> if(this = key) then index else findindex key rest (index+1)
		in
		let rec addtoarray input index array count result =
			if (index = count) then
				let newval = input::(List.nth array count) in
				addtoarray input index array (count+1) (newval::result)
			else if (count < List.length array) then 
				addtoarray input index array (count+1) ((List.nth array count)::result)
			else result
		in
		if (List.mem b1 a1) then (a1, addtoarray b2 (findindex b1 a1 0) b2 0 [])
		else (b1::a1, (b2::[])@a2)
	in
	let lists = List.fold_left fold_func ([],[[]]) second in
	let rulematcher = rules (fst lists) (snd lists) in
	(first, rulematcher);;
let parse_tree_leaves tree =
	tree;;
let make_matcher gram =
	gram;;
let make_parser gram =
	gram;;
