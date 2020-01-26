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
        let accept_empty input =
                match input with
                        | [] -> Some []
                        | _ -> None
        in
	let matchrules_rhs rules accept frag =
                let matched = matchrules gram rules (accept2 accept) (frag,[]) in
                match matched with
                        | (_,value) -> value
        in
	matchrules_rhs ((snd gram) (fst gram)) accept_empty;;

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
