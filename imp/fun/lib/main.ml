open Ast
open Types

exception TypeException
let bool_of_memval = function
  | Bool(b) -> b
  | _ -> raise TypeException
;;

let nat_of_memval = function
  | Int(i) -> i
  | _ -> raise TypeException
;;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
  
let ide_to_memval (e: env) (m: mem) (x: ide) : memval =
  match e x with
  |  BVar l -> m l
  |  IVar l -> m l

let eval_expr (en: env) (m: mem) (e: expr) = 
  let rec opI e1 op e2 = 
    let e1' = nat_of_memval (help e1) in
    let e2' = nat_of_memval (help e2) in
    Int(op e1' e2')

  and opB e1 op e2 = 
  let e1' = bool_of_memval (help e1) in
  let e2' = bool_of_memval (help e2) in
  Bool(op e1' e2')

  and help = function
    | True -> Bool true
    | False -> Bool false
    | Var(x) -> ide_to_memval en m x
    | Const(i) -> Int i
    | Not(e) -> Bool(not (bool_of_memval (help e)))
    | And(e1, e2) -> opB e1 (&&) e2
    | Or(e1, e2) -> opB e1 (||) e2
    | Add(e1, e2) -> opI e1 (+) e2
    | Sub(e1, e2) -> opI e1 (-) e2
    | Mul(e1, e2) -> opI e1 ( * ) e2
    | Eq(e1, e2) -> (try 
        Bool(bool_of_memval (help e1) = bool_of_memval (help e2))
        with TypeException -> 
        Bool(nat_of_memval (help e1) = nat_of_memval (help e2))
        )
    | Leq(e1, e2) -> (try 
        Bool(bool_of_memval (help e1) <= bool_of_memval (help e2))
        with TypeException -> 
        Bool(nat_of_memval (help e1) <= nat_of_memval (help e2))
        )
  in help e
;;

exception EmptyMemory
exception NoRuleApplies

(* this allocates memory in reverse order
   I like the code more,
  but the tests don't want it like this *)
(*
let rec eval_decl (d: decl) (s: state) : (env*loc) =
  match d with
    | EmptyDecl -> (topenv s, getloc s)
    | IntVar (x, d) -> 
        let (oldenv, oldloc) = eval_decl d s in
        ((fun x' -> if x'=x then IVar (oldloc) else oldenv x'), oldloc+1) 
    | BoolVar (x, d) ->
        let (oldenv, oldloc) = eval_decl d s in
        ((fun x' -> if x'=x then BVar (oldloc) else oldenv x'), oldloc+1)
*)

(* Gives you the environment to use after seeing a decl *)
(* Note: the point of performing small step is so that we can do a finite amount of steps of programs that may not terminate. The evaluation of declarations, on the other hand, as well as that of expressions, will always terminate. *)
let rec eval_decl (d: decl) (s: state) : (env*loc) =
  match d with
    (* Empty: do not modify the environment *)
    | EmptyDecl -> (topenv s, getloc s)

    (* Otherwise add an IVar / BVar to it. *)
    | IntVar (x, d') ->
        let newenv x' = if x'=x then IVar (getloc s) else (topenv s ) x' in
        let s' = (newenv::(popenv s), getmem s, getloc s + 1) in
        eval_decl d' s'
    | BoolVar (x, d') ->
      let newenv x' = if x'=x then BVar (getloc s) else (topenv s ) x' in
      let s' = (newenv::(popenv s), getmem s, getloc s + 1) in
      eval_decl d' s'

let rec trace1 = function
  | St(_) -> raise NoRuleApplies
  | Cmd(c, s) -> match c with
    | Skip -> St(s)

    (* evaluate e to a val e'
       get loc associated to x in current env
       associate val to that loc*)
    | Assign(x,e) -> (
        let e' = eval_expr (topenv s) (getmem s) e in
        let loc = (topenv s) x in
        let oldmem = getmem s in
        match loc with 
          | BVar l -> (match e' with
              | Bool _ -> let newmem l' = if l'=l then e' else oldmem l' in
              St(getenv s, newmem, getloc s)
              | Int _ -> raise (TypeError("Wanted bool, got int")))

          | IVar l -> (match e' with
            | Int _ -> let newmem l' = if l'=l then e' else oldmem l' in
            St(getenv s, newmem, getloc s)
            | Bool _ -> raise (TypeError("Wanted int, got bool"))))

    | If(e,c1,c2) -> 
        let e' = eval_expr (topenv s) (getmem s) e in (match e' with
          | Bool(true) -> Cmd(c1,s)
          | Bool(false) -> Cmd(c2,s)
          | _ -> raise TypeException)

    | While(e,c) -> 
        let e' = eval_expr (topenv s) (getmem s) e in (match e' with
          | Bool(true) -> Cmd(Seq(c,While(e,c)),s)
          | Bool(false) -> St(s)
          | _ -> raise TypeException)

    | Seq(c1,c2) -> 
        let conf' = trace1 (Cmd(c1,s)) in (match conf' with
          | St(s') -> Cmd(c2,s')
          | Cmd(c1',s') -> Cmd(Seq(c1',c2),s'))

    | Decl(d, c) -> 
        let (env', loc') = eval_decl d s in
        (* need to know this command has its own env
           so we wrap it in a Block *)
        Cmd(Block c, (env'::(getenv s), getmem s, loc'))

    (* step the execution of the wrapped command with its env
       once we're done executing it, discard that env *)
    | Block(c') -> 
        let conf' = trace1 (Cmd(c', s)) in
        match conf' with
          | St(s') -> St((popenv s', getmem s', getloc s'))
          | Cmd(c'', s') -> Cmd(Block c'', s')
;;

let trace n cmd = 
  let rec trace' n con = 
    if n=0 then [con]
    else try (let con' = trace1 con in
      con'::(trace' (n-1) con'))
    with _ -> [con]
  in let init = Cmd(cmd,([(fun x -> raise (UnboundVar x))], (fun _ -> raise EmptyMemory), 0))
  in init::(trace' n init)
;;

let rec last = function
  | [] -> failwith "Empty list" 
  | h::[] -> h
  | _::t -> last t
;;

let vars_of_cmd c = 
  let has l x = List.exists (fun y -> x=y) l in
  let rec help seen = function
    | Skip -> []
    | Assign(x,_) -> if has seen x then seen else seen@[x]
    | Seq(c1,c2) -> (help seen c1)@(help seen c2)
    | If(_,c1,c2) -> (help seen c1)@(help seen c2)
    | While(_,c) -> (help seen c)
    | Decl(_, c) -> (help seen c)
    | Block(c) -> (help seen c)
  in help [] c
;;