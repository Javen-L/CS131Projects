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
                                                | fraghead::fragtail when (fraghead = value) -> matchrule gram tail accept (fragtail,array)
						| _ -> (None, None)
                                        )
                                | N nonterminal ->
                                        let headmatcher = matchrules gram ((snd gram) nonterminal) in
                                        let tailmatcher = matchrule gram tail in
                                        headmatcher (tailmatcher accept) (frag,array)
and matchrules gram rules accept (frag,array) =
        match rules with
                | [] -> (None,None)
                | head::tail ->
                        let headmatcher = matchrule gram head accept (frag, array@[head]) in
                        let tailmatcher = matchrules gram tail in
                        match headmatcher with
                                | (None,_) -> tailmatcher accept (frag, array)
                                | _ -> headmatcher
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
	let matchrules_rhs rules accept frag =
		let matched = matchrules gram rules (accept2 accept) (frag,[]) in
		match matched with
			| (_,value) -> value
        in
	let rec construct_rule rule rhs_list =
(* input: rule (list of symbols) *)
(* output: Node or Leaf array tuple with rhs_list *)
		match rule with
			| [] -> (Some [], Some rhs_list)
			| head::tail ->
				match head with
					| T terminal -> (Some [Leaf terminal], Some rhs_list)
					| N nonterminal ->
						let head_constructor = construct_tree nonterminal rhs_list in
						match head_constructor with
							| (Some node, None) ->  (None, None)
							| (None, _) -> (None, None)
							| (Some node, Some array) ->
								let tail_constructor = construct_rule tail array in
								match tail_constructor with
									| (Some node, None) ->  (None, None)
									| (None, _) -> (None, None)
									| (Some nodearray, Some array2) -> (Some ([node]@nodearray), Some array2)
	and construct_tree start_symbol rhs_list =
(* inputs: right-hand side and the start symbol *)
(* outputs: node for current level tuple with rhs_list *)
		match rhs_list with
			| None -> (None,None)
			| Some [] -> (None,None)
			| Some (head::tail) ->
				let head_constructor = construct_rule head (Some tail) in
				match head_constructor with
					| (None, _) -> (None,None)
					| (Some nodearray, None) -> (None, None)
					| (Some nodearray, Some array) -> (Some (Node (start_symbol, nodearray)), Some array)
	in
	let first_construct_tree start_symbol rhs_list frag =
(* inputs: right-hand side and the start symbol *)
(* outputs: node for current level tuple with rhs_list *)
		let rhs_list2 = rhs_list frag in
		match rhs_list2 with
			| None -> None
			| Some [] -> None
			| Some array ->
				let constructor = construct_tree start_symbol rhs_list2 in
				match constructor with
					| (None, _) -> None
					| (Some tree, None) -> None
					| (Some tree, Some _) -> Some tree
	in
	let accept_empty input =
		match input with
			| [] -> Some []
			| _ -> None
	in
	let rhs_list = matchrules_rhs ((snd gram) (fst gram)) accept_empty in
	first_construct_tree (fst gram) rhs_list;;
