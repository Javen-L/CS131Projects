type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
(* 'a * ('a * ('a, 'b) symbol list) list *)
(* 'a * fun 'a -> (('a, 'b) symbol list) list *)
let convert_grammar gram1 =
	let rec findindex key lst index = match lst with
		| [] -> -1
		| this::rest -> if(this = key) then index else findindex key rest (index+1)
	in
	let rules keys values term =
		let matcher key value input = List.nth value (findindex input key 0)
		in
		matcher keys values term
	in
	let first = fst gram1 in
	let second = snd gram1 in
	let fold_func (outputsymbols, outputrules) (elementsymbol, elementrule) =
		let rec addtoarray input index array count result =
			if (index = count) then
				let newval = (List.nth array count)@[input] in
				addtoarray input index array (count+2) (result@[newval])
			else if (count < List.length array) then
				addtoarray input index array (count+1) (result@[(List.nth array count)])
			else result
		in
		if (List.mem elementsymbol outputsymbols) then (outputsymbols, addtoarray elementrule (findindex elementsymbol outputsymbols 0) outputrules 0 [])
		else (outputsymbols@[elementsymbol], outputrules@([[elementrule]]))
	in
	let lists = List.fold_left fold_func ([],[]) second in
	let rulematcher = rules (fst lists) (snd lists) in
	(first, rulematcher);;
let rec parse_tree_leaves tree = 
	let fold_func outputlist element =
		outputlist@(parse_tree_leaves element)
	in
	match tree with
	| Node (value, treelist) -> List.fold_left fold_func [] treelist
	| Leaf value -> [value];;
let make_matcher gram =
	let first (one,two,three) =
		one
	in
	let rec ismatch gram frag = match frag with
		| _::_ ->
(* helper function to be in fold_left *)
(* try to match each subtree *)
(* match each part *)
(* remove leaves *)
(* skip nodes *)
(* false if not match *)
(* test all node possibilities if leaves match *)
(* input accumulator is false *)
(* result ORed with accumulator *)
			let rec matchoption (acc, gram, frag) option =
				if (acc) then (acc, gram, frag)
				else (acc, gram, frag)
			in
			first (List.fold_left matchoption (false, gram, frag) ((snd gram) (fst gram)))
		| [] -> true
	in
	let rec suffix gram frag =
		if (ismatch gram frag) then []
		else
		let reverse = List.rev frag in
		match reverse with
		| hd::tail ->
			let outcome = suffix gram (List.rev tail) in
			if (outcome = []) then [hd]
			else outcome@[hd]
		| [] -> []
	in
	let matcher gram accept frag =
		accept (suffix gram frag)
	in
	matcher gram;;
let make_parser gram =
	let parser gram frag =
		let accept input = match input with
			| _::_ -> Some input
			| [] -> None
		in
		let suffix = make_matcher gram accept frag in
		if (suffix = Some []) then None
(* make make_matcher work first *)
(* might not want to use make_matcher, rather construct tree as you go *)
(* pass tree recursively *)
(* change gram to actual parse tree of frag *)
		else Some gram
	in
	parser gram;;
