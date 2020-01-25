type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
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

let rec matchrule gram rule accept (frag,array) =
                match rule with
                        | [] -> accept (frag,array)
                        | head::tail ->
                                match head with
                                        | T value ->
                                                (match frag with
                                                        | [] -> (None, None)
                                                        | fraghead::fragtail ->
                                                                if (value = fraghead) then
                                                                        matchrule gram tail accept (fragtail,array)
                                                                else (None, None)
                                                )
                                        | N nonterminal ->
                                                let headmatcher = matchrules gram ((snd gram) nonterminal) in
                                                let tailmatcher = matchrule gram tail in
                                                headmatcher (tailmatcher accept) (frag,array)
        and matchrules gram rules accept (frag,array) =
                match rules with
                        | [] -> (None,None)
                        | head::tail ->
                                let headmatcher = matchrule gram head accept (frag, (array@[head])) in
                                let tailmatcher = matchrules gram tail in
                                match headmatcher with
                                        | (None,_) -> tailmatcher accept (frag, array)
                                        | x -> x
;;

let accept2 acceptor (frag, array) = (acceptor frag, Some array);;

let rec make_matcher gram =
        let firstmatchrules rules accept frag =
                match rules with
                        | [] -> None
                        | head::tail ->
                                let headmatcher = matchrule gram head (accept2 accept) (frag, ([head])) in
                                let tailmatcher = matchrules gram tail in
                                match headmatcher with
                                        | (None,_) -> fst (tailmatcher (accept2 accept) (frag, []))
                                        | (x,_) -> x
        in
	firstmatchrules ((snd gram) (fst gram));;

let make_parser gram =
	let accept_empty input =
		match input with
			| [] -> Some []
			| _ -> None
	in
	let firstmatchrules rules accept frag =
		match rules with
                        | [] -> None
                        | head::tail ->
                                let headmatcher = matchrule gram head (accept2 accept) (frag, ([head])) in
                                let tailmatcher = matchrules gram tail in
                                match headmatcher with
                                        | (None,_) -> snd (tailmatcher (accept2 accept) (frag, []))
                                        | (_,y) -> y
        in
	let rhs_list = firstmatchrules ((snd gram) (fst gram)) accept_empty in
	let rec construct_tree rhs_list tree =
		match rhs_list with
			| [] -> tree
			| head::tail -> match head with
				| T terminal -> Leaf terminal
				| N nonterminal -> Node (nonterminal, [])
(* how to add on to tree? *)
	in
	let parser gram frag =
		let accept input = match input with
			| _::_ -> Some input
			| [] -> None
		in
		let suffix = make_matcher gram accept frag in
			match suffix with
				| Some (head::tail) -> None
				| Some [] -> Some (Leaf "Hi")
(* change to actual parse tree of frag *)
				| None -> None
(* make make_matcher work first *)
(* might not want to use make_matcher, rather construct tree as you go *)
(* can take helper functions from make_matcher though *)
(* pass tree recursively *)
	in
	parser gram;;
