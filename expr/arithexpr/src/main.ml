open Ast

let rec string_of_natexpr = function
    Zero -> "0"
  | Succ(n) -> "succ" ^ (string_of_natexpr n)
  | Pred(n) -> "pred" ^ (string_of_natexpr n)
;;


let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e) -> "Not(" ^ string_of_boolexpr e ^ ")"
  | And(e1,e2) -> "And(" ^ string_of_boolexpr e1 ^ ", " ^ string_of_boolexpr e2 ^ ")"
  | Or(e1,e2) -> "Or(" ^ string_of_boolexpr e1 ^ ", " ^ string_of_boolexpr e2 ^ ")"
  | IsZero(n) -> "iszero" ^ (string_of_natexpr n)
;;

let string_of_expr = function
    BoolExpr(b) -> string_of_boolexpr b
  | NatExpr(n) -> string_of_natexpr n
;;

let bool_to_string b = if b then "true" else "false";;

let rec nat_to_string n = 
  if n < 0 then "-"^(nat_to_string (-n))
  else match n with
    0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | m -> (nat_to_string (m/10))^(nat_to_string (m mod 10))
;;

let string_of_val = function
    Bool(b) -> bool_to_string b
  | Nat(n) -> nat_to_string n
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec traceNat1 = function
    Succ(e) -> Succ(traceNat1 e)
  | Pred(Succ(e)) -> e
  | Pred(e) -> Pred(traceNat1 e)
  
  | _ -> raise NoRuleApplies
;;

let rec traceNat e = try
    let e' = traceNat1 e
    in e::(traceNat e')
  with NoRuleApplies -> [e]
;;
  
let rec traceBool1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = traceBool1 e0 in If(e0',e1,e2)

  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(traceBool1 e)

  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(traceBool1 e1, e2)

  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(traceBool1 e1, e2)

  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(n) -> IsZero (traceNat1 (n))
  
  | _ -> raise NoRuleApplies
;;

let rec traceBool e = try
    let e' = traceBool1 e
    in e::(traceBool e')
  with NoRuleApplies -> [e]
;;

let trace = function
    NatExpr(n) -> List.map (fun n' -> NatExpr n') (traceNat n)
  | BoolExpr(b) -> List.map (fun b' -> BoolExpr b') (traceBool b)
;;

let rec eval_nat = function
    Zero -> 0
  | Succ(n) -> (eval_nat n) + 1
  | Pred(n) when (eval_nat n) > 0 -> (eval_nat n) - 1
  | _ -> failwith "Underflow"
;;

let rec eval_bool = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if eval_bool e0 then eval_bool e1 else eval_bool e2
  | Not(e) -> not (eval_bool e)
  | And(e1, e2) -> (eval_bool e1) && (eval_bool e2)
  | Or(e1, e2) -> (eval_bool e1) || (eval_bool e2)
  | IsZero(n) -> (eval_nat n) = 0
;;

let eval = function
    NatExpr(n) -> Nat(eval_nat n)
  | BoolExpr(b) -> Bool(eval_bool b)
;;