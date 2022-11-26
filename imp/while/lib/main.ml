open Ast

type exprval = Bool of bool | Nat of int        (* value of an expression *)
type state = ide -> exprval                     (* state = map from identifiers to expression values *)
type conf = St of state | Cmd of cmd * state    (* configuration = state | (command,state) *)

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let string_of_val = function
  | Nat(i) -> string_of_int i
  | Bool(b) -> string_of_bool b
;;
let rec string_of_expr = function
  | True -> "true"
  | False -> "false"
  | Var(x) -> x
  | Const(i) -> string_of_int i
  | Not(e) -> "not " ^ (string_of_expr e)
  | And(e1,e2) -> (string_of_expr e1) ^ " and " ^ (string_of_expr e2)
  | Or(e1,e2) -> (string_of_expr e1) ^ " or " ^ (string_of_expr e2)
  | Add(e1,e2) -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Sub(e1,e2) -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | Mul(e1,e2) -> (string_of_expr e1) ^ " * " ^ (string_of_expr e2)
  | Eq(e1,e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
  | Leq(e1,e2) -> (string_of_expr e1) ^ " <= " ^ (string_of_expr e2)

let rec string_of_cmd = function
  | Skip -> "skip"
  | Assign(x,e) -> x ^ " := " ^ (string_of_expr e)
  | Seq(c1,c2) -> (string_of_cmd c1) ^ "; " ^ (string_of_cmd c2)
  | If(e,c1,c2) -> "if " ^ (string_of_expr e) ^ " then " ^ (string_of_cmd c1) ^ " else " ^ (string_of_cmd c2)
  | While(e,c) -> "while " ^ (string_of_expr e) ^ " do " ^ (string_of_cmd c)

let eval_expr _ _ = failwith "Not implemented"
let string_of_state s v = 
  "{"^
  (List.fold_left (fun acc x -> acc ^"("^x^":="^(string_of_val (s x))^"), ") "" v)
  ^ "}"
;;

let string_of_conf vars conf = match conf with
  | St(s) -> string_of_state s vars
  | Cmd(c,s) -> (string_of_cmd c) ^ " with " ^ (string_of_state s vars)

let string_of_trace idel confl = 
  List.fold_left (fun acc con -> acc ^ " -> " ^ (string_of_conf idel con)) "" confl
;;

let eval_expr state e = 
  let rec opI e1 op e2 = 
    let e1' = nat_of_exprval (help e1) in
    let e2' = nat_of_exprval (help e2) in
    Nat(op e1' e2')

  and opB e1 op e2 = 
  let e1' = bool_of_exprval (help e1) in
  let e2' = bool_of_exprval (help e2) in
  Bool(op e1' e2')

  and help = function
    | True -> Bool true
    | False -> Bool false
    | Var(x) -> state x
    | Const(i) -> Nat i
    | Not(e) -> Bool(not (bool_of_exprval (help e)))
    | And(e1, e2) -> opB e1 (&&) e2
    | Or(e1, e2) -> opB e1 (||) e2
    | Add(e1, e2) -> opI e1 (+) e2
    | Sub(e1, e2) -> opI e1 (-) e2
    | Mul(e1, e2) -> opI e1 ( * ) e2
    | Eq(e1, e2) -> (try 
        Bool(bool_of_exprval (help e1) = bool_of_exprval (help e2))
        with TypeException -> 
        Bool(nat_of_exprval (help e1) = nat_of_exprval (help e2))
        )
    | Leq(e1, e2) -> (try 
        Bool(bool_of_exprval (help e1) <= bool_of_exprval (help e2))
        with TypeException -> 
        Bool(nat_of_exprval (help e1) <= nat_of_exprval (help e2))
        )
  in help e
;;

let trace1 _ = failwith "Not implemented"

let trace _ _ = failwith "Not implemented"

let last _ = failwith "Not implemented"

let vars_of_cmd _ = failwith "Not implemented"