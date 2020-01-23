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
let make_matcher gram =
	let rec matchsymbol gram frag symbol =
(* parse nonterminal combinations and provide tail *)
(* try to parse max size first *)
		let first (one,two,three) =
			one
		in
		let reversedrules = List.rev ((snd gram) symbol) in
		if (first (List.fold_left matchrule (false,gram,frag) reversedrules)) then Some []
		else
			let reverse = List.rev frag in
			(match reverse with
				| head::tail ->
					let result = matchsymbol gram (List.rev tail) symbol in
					(match result with
						| Some tail -> Some (tail@[head])
						| None -> None
					)
				| [] -> None
			)
	and matchrule (acc,gram,frag) rule =
		if (acc) then (acc,gram,frag)
		else
		let first (one,two,three) =
			one
		in
		match rule with
		| head::ruletail ->
			(match head with
				| T value ->
					(match frag with
						| head::fragtail ->
							if (value = head) then (first (matchrule (acc,gram,fragtail) ruletail),gram,frag)
							else (false,gram,frag)
						| [] -> (false,gram,frag)
					)
				| N nonterminal ->
					let rec symbolrecursion acc gram frag fragtosearch fragtoappend nonterminal ruletail =
                                                let result = matchsymbol gram fragtosearch nonterminal in
                                                (match result with
                                                        | Some fragtail ->
                                                                let outcome = matchrule (acc,gram,fragtail@fragtoappend) ruletail in
                                                                if (first outcome) then (true,gram,frag)
                                                                else
									let taillength = List.length fragtail in
                                                                        let length = (List.length fragtosearch)-taillength in
                                                                        let rec firstelements length frag =
                                                                                match frag with
                                                                                        | [] -> []
                                                                                        | head::tail ->
                                                                                                if (length = 1) then [head]
                                                                                                else head::(firstelements (length-1) frag)
                                                                        in
                                                                        let newfragtosearch = firstelements length frag in
                                                                        let reverse = List.rev newfragtosearch in
                                                                        (match reverse with
                                                                                | head::tail ->
                                                                                        symbolrecursion acc gram frag (List.rev tail) (head::fragtoappend) nonterminal ruletail
                                                                                | [] -> (false, gram, frag)
                                                                        )
                                                        | None -> (false, gram, frag)
                                                )
                                        in
                                        symbolrecursion acc gram frag frag [] nonterminal ruletail
			)
		| [] -> (frag = [], gram, frag)
	in
	let ismatch gram frag =
(* if match, true *)
(* if not match, false *)
		let first (one, two, three) =
			one
		in
		let reverse = List.rev ((snd gram) (fst gram)) in
		first (List.fold_left matchrule (false,gram,frag) reverse)
	in
	let rec matcher gram suffix accept frag =
		if (ismatch gram frag)
		then
			if((accept frag) != None) then accept suffix
			else
				let reverse = List.rev frag in
				match reverse with
					| head::tail -> matcher gram (head::suffix) accept (List.rev tail)
					| [] -> None
		else
			let reverse = List.rev frag in
			match reverse with
				| head::tail -> matcher gram (head::suffix) accept (List.rev tail)
				| [] -> None
(* test if whole frag is match *)
(* if so, call accept on it and suffix *)
(* if not, test if frag without last is match *)
(* keep going recursively until empty*)
	in
	matcher gram [];;

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
(* can take helper functions from make_matcher though *)
(* pass tree recursively *)
		else Some (Leaf "Hi")
(* change to actual parse tree of frag *)
	in
	parser gram;;
