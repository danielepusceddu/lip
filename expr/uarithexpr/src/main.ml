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
;;

type exprtype = BoolT | NatT
exception TypeError of string

let string_of_type = function
    BoolT -> "Bool"
  | NatT -> "Nat"
;;

let err_msg e t t' = 
  Printf.sprintf "%s has type %s but type %s was expected" (string_of_expr e) (string_of_type t) (string_of_type t')
;;

let rec typecheck expr = 
  match expr with
      True -> BoolT
    | False -> BoolT
    | Zero -> NatT

    | And(e1, e2) -> 
        let e1t = typecheck e1 in
        let e2t = typecheck e2 in
        if not (e1t = BoolT) then
          raise (TypeError(err_msg e1 e1t BoolT))
        else if not (e2t = BoolT) then
          raise (TypeError((err_msg e2 e2t BoolT)))
        else BoolT
        
    | Or(e1, e2) -> 
      let e1t = typecheck e1 in
      let e2t = typecheck e2 in
      if not (e1t = BoolT) then
        raise (TypeError((err_msg e1 e1t BoolT)))
      else if not (e2t = BoolT) then
        raise (TypeError((err_msg e2 e2t BoolT)))
      else BoolT

    | Not(e1) -> 
      let e1t = typecheck e1 in
      if not (e1t = BoolT) then
        raise (TypeError((err_msg e1 e1t BoolT)))
      else BoolT

    | If(e1, e2, e3) -> 
      let e1t = typecheck e1 in
      let e2t = typecheck e2 in
      let e3t = typecheck e3 in
      if not (e1t = BoolT) then
        raise (TypeError((err_msg e1 e1t BoolT)))
      else if not (e2t = BoolT) then
        raise (TypeError((err_msg e2 e2t BoolT)))
      else if not (e3t = BoolT) then
        raise (TypeError((err_msg e3 e3t BoolT)))
      else BoolT

    | Succ(e1) -> 
      let e1t = typecheck e1 in
      if not (e1t = NatT) then
        raise (TypeError((err_msg e1 e1t NatT)))
      else NatT

    | Pred(e1) -> 
      let e1t = typecheck e1 in
      if not (e1t = NatT) then
        raise (TypeError((err_msg e1 e1t NatT)))
      else NatT

    | IsZero(e1) -> 
      let e1t = typecheck e1 in
      if not (e1t = NatT) then
        raise (TypeError((err_msg e1 e1t NatT)))
      else BoolT
    ;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
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
    True -> Bool true
  | False -> Bool false
  | If(e0,e1,e2) -> Bool (if getBool (eval e0) then getBool (eval e1) else getBool (eval e2))
  | Not(e) -> Bool (not (getBool (eval e)))
  | And(e1, e2) -> Bool ((getBool (eval e1)) && (getBool (eval e2)))
  | Or(e1, e2) -> Bool ((getBool (eval e1)) || (getBool (eval e2)))

  | Zero -> Nat 0
  | Succ(e) -> Nat ((getNat (eval e)) + 1)
  | Pred(e) when (getNat (eval e)) > 0 -> Nat ((getNat (eval e)) - 1)
  | Pred(_) -> failwith "Pred of 0"
  | IsZero(e) -> if (getNat (eval e)) = 0 then Bool true else Bool false
;;
