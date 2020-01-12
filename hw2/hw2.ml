type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
(* 'a * ('a * ('a, 'b) symbol list) list *)
(* 'a * fun 'a -> (('a, 'b) symbol list) list *)
let convert_grammar gram1 =
	gram1;;
let parse_tree_leaves tree =
	tree;;
let make_matcher gram =
	gram;;
let make_parser gram =
	gram;;
