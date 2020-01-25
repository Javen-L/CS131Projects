let accept_all string = Some string

type awksub_nonterminals =
  | Expr | Term | Lvalue | Binop | Num

let awkish_grammar_edited =
  (Expr,
   function
     | Expr ->
         [[N Term];
          [N Term; N Binop; N Expr];
          []]
     | Term ->
         [[N Num];
          [N Lvalue];
          [T"("; N Expr; T")"]]
     | Lvalue ->
         [[T"$"; N Expr]]
     | Binop ->
         [[T"+"];
          [T"-"]]
     | Num ->
         [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
          [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let make_matcher_test = (make_matcher awkish_grammar_edited accept_all ["(";"1";"+";"$";")";"+";"(";")"]) = Some ["+";"(";")"];;
