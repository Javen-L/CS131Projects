type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num
let awksub_grammar = (Expr,
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]);;
let converted = convert_grammar awksub_grammar;;
let expr = (snd converted) Expr;;
let lval = (snd converted) Lvalue;;
let incrop = (snd converted) Incrop;;
let binop = (snd converted) Binop;;
let num = (snd converted) Num;;

let leaf = (Node (Expr, [Leaf "hi"; Node (Binop, [Leaf "+"]); Leaf "bye"]));;
let output = parse_tree_leaves leaf;;

let other_parse_tree = parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]));;
let test = other_parse_tree = [3;4;5];;
