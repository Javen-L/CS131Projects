type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal

let first (one,two,three) =
	one

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
				let rec symbolrecursion acc gram frag fragtosearch nonterminal ruletail =
					let result = matchsymbol gram fragtosearch nonterminal in
					(match result with
						| Some fragtail ->
							let outcome = matchrule (acc,gram,fragtail) ruletail in
							if (first outcome) then (true,gram,frag)
							else
								let reverse = List.rev fragtosearch in
								(match reverse with
									| head::tail ->
										symbolrecursion acc gram frag (List.rev tail) nonterminal ruletail
									| [] -> (false, gram, frag)
								)
						| None -> (false, gram, frag)
					)
				in
				symbolrecursion acc gram frag frag nonterminal ruletail
		)
	| [] -> (frag = [], gram, frag)
;;
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

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
  first (matchrule (false,awkish_grammar,["ouch"]) [N Term]) = false
let test1 =
  first (matchrule (false,awkish_grammar,["9"]) [N Term]) = true
(* infinite recursion *)
(* not due to loop in searching *)
(* is it just super slow? *)
(* or does it not accept failure? *)
(* when printing out nonterminals, it only goes through 0-9 *)
let test2 =
  first (matchrule (false,awkish_grammar,["9"; "+"; "$"; "1"; "+"]) [N Term]) = false
let test3 =
  first (matchrule (false,awkish_grammar,["9"; "+"; "$"; "1"; "+"]) [N Term]) = false

let test0 =
  first (matchrule (false,awkish_grammar,["ouch"]) [N Term; N Binop; N Expr]) = false
let test1 =
  first (matchrule (false,awkish_grammar,["9"]) [N Term; N Binop; N Expr]) = false
let test2 =
  first (matchrule (false,awkish_grammar,["9"; "+"; "$"; "1"; "+"]) [N Term; N Binop; N Expr]) = false
let test3 =
  first (matchrule (false,awkish_grammar,["9"; "+"; "$"; "1"; "+"]) [N Term; N Binop; N Expr]) = false
