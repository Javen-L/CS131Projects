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
let rec make_matcher gram =
        let rec matchrule rule accept frag =
                match rule with
                        | [] -> accept frag
                        | head::tail ->
                                match head with
                                        | T value ->
                                                (match frag with
                                                        | [] -> None
                                                        | fraghead::fragtail ->
                                                                if (value = fraghead) then
                                                                        matchrule tail accept fragtail
                                                                else None
                                                )
                                        | N nonterminal ->
                                                let headmatcher = matchrules ((snd gram) nonterminal) in
                                                let tailmatcher = matchrule tail in
                                                headmatcher (tailmatcher accept) frag
        and matchrules rules accept frag =
                match rules with
                        | [] -> None
                        | head::tail ->
                                let headmatcher = matchrule head accept frag in
                                let tailmatcher = matchrules tail in
                                match headmatcher with
                                        | None -> tailmatcher accept frag
                                        | x -> x
        in
	matchrules ((snd gram) (fst gram));;

let make_parser gram =
        let rec parserule rule accept frag =
                match rule with
                        | [] -> accept frag
                        | head::tail ->
                                match head with
                                        | T value ->
                                                (match frag with
                                                        | [] -> None
                                                        | fraghead::fragtail ->
                                                                if (value = fraghead) then
                                                                        parserule tail accept fragtail
                                                                else None
                                                )
                                        | N nonterminal ->
                                                let headparser = parserules ((snd gram) nonterminal) in
                                                let tailparser = parserule tail in
                                                headparser (tailparser accept) frag
        and parserules rules accept frag =
                match rules with
                        | [] -> None
                        | head::tail ->
                                let headparser = parserule head accept frag in
                                let tailparser = parserules tail in
                                match headparser with
                                        | None -> tailparser accept frag
                                        | x -> x
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
