open Ast

type exprval = Bool of bool | Nat of int

let getNat v = match v with
    Nat i -> i
  | _ -> failwith "Not a nat"
;;

let getBool v = match v with
    Bool b -> b
  | _ -> failwith "Not a boolean"
;;

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n
;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ string_of_expr e ^ ")"
  | And(e1,e2) -> "And(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Or(e1,e2) -> "Or(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

  | Zero -> "0"
  | Succ(e) -> "succ("^(string_of_expr e)^")"
  | Pred(e) -> "pred("^(string_of_expr e)^")"
  | IsZero(e) -> "iszero("^(string_of_expr e)^")"

  | Var(x) -> x
  | Let(x,e1,e2) -> "let "^x^" = "^(string_of_expr e1)^" in "^(string_of_expr e2)
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec subst x v expr = match expr with
  | If(e1,e2,e3) -> If(subst x v e1, subst x v e2, subst x v e3)
  | Not(e) -> Not(subst x v e)
  | And(e1,e2) -> And(subst x v e1, subst x v e2)
  | Or(e1,e2) -> Or(subst x v e1, subst x v e2)
  | Succ(e) -> Succ(subst x v e)
  | Pred(e) -> Pred(subst x v e)
  | IsZero(e) -> IsZero(subst x v e)

  | Let(y,e1,e2) when x=y -> Let(y,e1,e2)
  | Let(y,e1,e2) -> Let(y, subst x v e1, subst x v e2)
  | Var(y) when x=y -> v
  | Var(y) -> Var(y)

  | True -> True
  | False -> False
  | Zero -> Zero
;;
  
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)

  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)

  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)

  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)

  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(nv)) -> nv
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)

  (* parentheses are necessary, otherwise _ will be used for the next match case... *)
  | Let(x,e1,e2) -> (try (Let(x, trace1 e1, e2)) with _ -> (subst x e1 e2))
  
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(* to evlauate the last element of trace *)
let rec eval_simple = function
    True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Succ(e) -> Nat ((getNat (eval_simple e)) + 1)
  | Pred(e) when (getNat (eval_simple e)) > 0 -> Nat ((getNat (eval_simple e)) - 1)
  | Pred(_) -> failwith "Pred of 0"
  | _ -> failwith "Not terminal"
;;

let eval expr = 
  let rec eval' (expr,rho) = 
    (* print_endline (string_of_expr expr); *)
    match expr with
      True -> Bool true
    | False -> Bool false
    | If(e0,e1,e2) -> Bool (if getBool (eval' (e0,rho)) then getBool (eval' (e1,rho)) else getBool (eval' (e2,rho)))
    | Not(e) -> Bool (not (getBool (eval' (e,rho))))
    | And(e1, e2) -> Bool ((getBool (eval' (e1,rho))) && (getBool (eval' (e2,rho))))
    | Or(e1, e2) -> Bool ((getBool (eval' (e1,rho))) || (getBool (eval' (e2,rho))))

    | Zero -> Nat 0
    | Succ(e) -> Nat ((getNat (eval' (e,rho))) + 1)
    | Pred(e) when (getNat (eval' (e,rho))) > 0 -> Nat ((getNat (eval' (e,rho))) - 1)
    | Pred(_) -> failwith "Pred of 0"
    | IsZero(e) -> if (getNat (eval' (e,rho))) = 0 then Bool true else Bool false

    | Var(x) -> rho x
    | Let(x,e1,e2) -> eval' (e2, fun y -> if y=x then (eval' (e1,rho)) else rho x)
  in eval' (expr,fun _ -> failwith "No binding")
;;
