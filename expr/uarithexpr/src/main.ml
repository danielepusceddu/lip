open Ast

let string_of_val = string_of_int

let rec string_of_expr = function
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ string_of_expr e ^ ")"
  | And(e1,e2) -> "And(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Or(e1,e2) -> "Or(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

  | Zero -> "0"
  | Succ(e) -> "succ("^(string_of_expr e)^")"
  | Pred(e) -> "pred("^(string_of_expr e)^")"
  | IsZero(e) -> "iszero("^(string_of_expr e)^")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = function
    If(Succ(_),e1,_) -> e1
  | If(Zero,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)

  | Not(Succ(_)) -> Zero
  | Not(Zero) -> Succ(Zero)
  | Not(e) -> Not(trace1 e)

  | And(Succ(_), e2) -> e2
  | And(Zero, _) -> Zero
  | And(e1, e2) -> And(trace1 e1, e2)

  | Or(Succ(_), _) -> Succ(Zero)
  | Or(Zero, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)

  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(nv)) -> nv
  | Pred(Zero) -> Zero
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> Succ(Zero)
  | IsZero(Succ(_)) -> Zero
  | IsZero(e) -> IsZero(trace1 e)
  
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval expr = 
  (* print_endline (string_of_expr expr); *)
  match expr with
  | If(e0,e1,e2) -> if (eval e0) = 1 then eval e1 else eval e2
  | Not(e) -> if (eval e) = 0 then 1 else 0
  | And(e1, e2) -> if (eval e1) != 0 && (eval e2) != 0 then 1 else 0
  | Or(e1, e2) -> if (eval e1) != 0 || (eval e2) != 0 then 1 else 0

  | Zero -> 0
  | Succ(e) -> (eval e) + 1
  | Pred(Zero) -> 0
  | Pred(e) when (eval e) > 0 -> (eval e) - 1
  | Pred(_) -> 0
  | IsZero(e) -> if (eval e) = 0 then 1 else 0
;;
