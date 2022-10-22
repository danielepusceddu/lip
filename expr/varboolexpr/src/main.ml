open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e) -> "Not(" ^ string_of_boolexpr e ^ ")"
  | And(e1,e2) -> "And(" ^ string_of_boolexpr e1 ^ ", " ^ string_of_boolexpr e2 ^ ")"
  | Or(e1,e2) -> "Or(" ^ string_of_boolexpr e1 ^ ", " ^ string_of_boolexpr e2 ^ ")"
  | Var(s) -> s
;;

let parse (s : string) : boolProg =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies;;

let typecheck ((d,e): boolProg): bool = 
  (* checks that variable x is declared in d *)
  let is_declared (x: string) (d: boolDecl): bool = 
    List.exists (fun (s,_) -> s = x) d in
  
  (* checks whether an expression contains variable use *)
  let rec contains_var = function
      Var(_) -> true
    | True -> false
    | False -> false
    | If(e1,e2,e3) -> contains_var e1 || contains_var e2 || contains_var e3
    | Not(e') -> contains_var e'
    | And(e1, e2) -> contains_var e1 || contains_var e2
    | Or(e1, e2) -> contains_var e1 || contains_var e2 in
  
 (* checks that there are no double declarations
  and that no declaration expression contains a variable*)
  let rec check_decl d = 
    match d with
      [] -> true
    | (s,e')::t -> if (is_declared s t) || (contains_var e')
        then false else check_decl t in 
  
  (* checks that all variables in an expression are declared in d *)
  let rec check_expr = function
      Var(x) -> is_declared x d
    | True -> true
    | False -> true
    | If(e1,e2,e3) -> check_expr e1 && check_expr e2 && check_expr e3
    | Not(e') -> check_expr e'
    | And(e1, e2) -> check_expr e1 && check_expr e2
    | Or(e1, e2) -> check_expr e1 && check_expr e2
  in check_decl d && check_expr e
;; 

(* Solution that permits the use of identifiers in declarations
   Implemented with mutually recursive functions *)
let rec env_of_decl (d: boolDecl)=
  match d with
    [] -> fun x -> failwith ("Identifier " ^ x ^ " not declared")
  | (k,v)::t -> (fun x -> if x = k then eval (t,v) else (env_of_decl t) x)

and eval (p: boolProg) =
 match p with
    (_,True) -> true
  | (_,False) -> false
  | (d,If(e0,e1,e2)) -> if eval (d,e0) then eval (d,e1) else eval (d,e2)
  | (d,Not(e)) -> not (eval (d,e))
  | (d,And(e1, e2)) -> (eval (d,e1)) && (eval (d,e2))
  | (d,Or(e1, e2)) -> (eval (d,e1)) || (eval (d,e2))
  | (d,Var(x)) -> (env_of_decl d) x
;;

(* Solution that permits the use of identifiers in declarations
   Implemented without mutually recursive functions *)
let eval' (p: boolProg) = 
  let rec helper expr rho = match expr with     
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if helper e0 rho then helper e1 rho else helper e2 rho
  | Not(e) -> not (helper e rho)
  | And(e1, e2) -> (helper e1 rho) && (helper e2 rho)
  | Or(e1, e2) -> (helper e1 rho) || (helper e2 rho)
  | Var(x) -> rho x

  in let rec env_of_decl = function
    [] -> fun x -> failwith ("Identifier " ^ x ^ " not declared")
    | (k,v)::t -> fun x -> if x = k then helper v (env_of_decl t) else (env_of_decl t) x

  in match p with
    (d,e) -> let rho = env_of_decl d 
      in helper e rho
;;

(* Solution without the use of identifiers in declarations
   (just calls typecheck first) *)
let eval (p: boolProg) = 
  if not (typecheck p) then failwith "Static type error" else
  let rec helper expr rho = match expr with     
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if helper e0 rho then helper e1 rho else helper e2 rho
  | Not(e) -> not (helper e rho)
  | And(e1, e2) -> (helper e1 rho) && (helper e2 rho)
  | Or(e1, e2) -> (helper e1 rho) || (helper e2 rho)
  | Var(x) -> rho x in 

  let rec env_of_decl = function
    [] -> fun x -> failwith ("Identifier " ^ x ^ " not declared")
  | (k,v)::t -> fun x -> if x = k then helper v (env_of_decl t) else (env_of_decl t) x

  in match p with
    (d,e) -> let rho = env_of_decl d 
      in helper e rho
;;

let rec trace (d,e) = 
  let rec env_of_decl_small (d: boolDecl ) = match d with
    [] -> fun x -> failwith ("Identifier " ^ x ^ " not declared")
  | (k,v)::t -> fun x -> if x = k then v else env_of_decl_small t x

  in let rho = env_of_decl_small d in 

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

  | Var(x) -> rho x

  | _ -> raise NoRuleApplies

  in try
    let e' = trace1 e
    in e::(trace (d,e'))
  with NoRuleApplies -> [e]
;;