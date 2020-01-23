type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
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
									let reverse = List.rev fragtosearch in
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
		let output = ismatch gram frag in
		if (output)
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

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

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

let test = 
	(make_matcher awkish_grammar accept_all
		["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"])

(*let test4 =*)
(* ((make_matcher awkish_grammar accept_all*)
(*     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";*)
(*      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";*)
(*      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";*)
(*      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";*)
(*      "++"; "+"; "0"])*)
(*  = Some [])*)
