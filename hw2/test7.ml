type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal

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

let rec parse_tree_leaves tree =
        let fold_func outputlist element =
                outputlist@(parse_tree_leaves element)
        in
	match tree with
        | Node (value, treelist) -> List.fold_left fold_func [] treelist
        | Leaf value -> [value];;

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
                                        | T terminal ->
						let tail_constructor = construct_rule tail rhs_list in
						(match tail_constructor with
							| (Some node, None) -> (None, None)
							| (None, _) -> (None, None)
							| (Some nodearray, Some array2) -> (Some ([Leaf terminal]@nodearray), Some array2)
						)
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

type nonterminals =
        | Start | Terminal;;
let left_recursive_grammar =
        (Start, function
                | Start -> [[N Start; N Terminal]]
                | Terminal -> [[T "Terminal"]]);;
type nonterminals =
        | Start | Terminal;;
let left_recursive_grammar =
        (Start, function
                | Start -> [[N Terminal; T "Start"]]
                | Terminal -> [[N Start; T "Terminal"]]);;

let acceptor input = Some input;;

let matcher = make_matcher left_recursive_grammar;;
let test = matcher acceptor ["Terminal"];;
let parser = make_parser left_recursive_grammar;;
let test = parser ["Terminal"];;
