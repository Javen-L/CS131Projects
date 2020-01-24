type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let string_of_awksub value =
	match value with
		| Expr -> "Expr"
		| Term -> "Term"
		| Lvalue -> "Lvalue"
		| Incrop -> "Incrop"
		| Binop -> "Binop"
		| Num -> "Num"
;;

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

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
         [[N Num];
          [N Lvalue];
          [N Incrop; N Lvalue];
          [N Lvalue; N Incrop];
          [T"("; N Expr; T")"]]
     | Lvalue ->
         [[T"$"; N Expr]]
     | Incrop ->
         [[T"++"];
          [T"--"]]
     | Binop ->
         [[T"+"];
          [T"-"]]
     | Num ->
         [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
          [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((make_matcher awkish_grammar accept_all ["ouch"]) = None);;

let test1 =
  ((make_matcher awkish_grammar accept_all ["9"]) = Some []);;

let test2 =
  (make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"]);;
let test2 =
  ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"]) = Some ["+"]);;

let test3 =
  ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None);;

let test = 
	(make_matcher awkish_grammar accept_all
		["("; "$"; "8"; ")"; "-"; "$"; "++"; "$";])

let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some [])
