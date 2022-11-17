open Ast

let rec string_of_term = function
    Var x -> x
  | Abs(x,t) -> "fun " ^ x ^ ". " ^ string_of_term t
  | App(Var x,Var y) -> x ^ " " ^ y
  | App(Var x,t2) -> x ^ " (" ^ string_of_term t2 ^ ")"
  | App(t1,Var x) -> "(" ^ string_of_term t1 ^ ") " ^ x
  | App(t1,t2) -> "(" ^ string_of_term t1 ^ ") (" ^ string_of_term t2 ^ ")"

let parse (s : string) : term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(**********************************************************************
 max_nat : term -> int

 max_nat t computes the least n such that: 
 for all i : xi in vars_of_term t => i < n
  **********************************************************************)

let rec vars_of_term = function
    Var x -> [x]
  | Abs(x,t) -> x::(vars_of_term t)
  | App(t1,t2) -> (vars_of_term t1) @ (vars_of_term t2)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
            
let is_digit = function '0' .. '9' -> true | _ -> false

let explode s = List.map (fun x -> String.make 1 x |> int_of_string) (List.filter is_digit (List.init (String.length s) (String.get s)))

let nat_of_var xl = List.fold_left (fun x y -> x + y) 0 (List.mapi (fun i x -> x * (pow 10 i)) (List.rev (explode xl)))

let rec max_of_list = function 
  | [] -> 0
  | x::xs -> max x (max_of_list xs)
                
let max_nat t =
  let xl = vars_of_term t in
  let nl = List.map nat_of_var xl in
  1 +  max_of_list nl


(**********************************************************************
 is_free : string -> term -> bool

 Usage: is_free x t = true iff the variable x occurs free in t
 **********************************************************************)
let rec is_free x t = match t with
  | Var(y) when x=y -> true
  | Var(_) -> false

  | Abs(y,_) when x=y -> false
  | Abs(_,t2) -> is_free x t2

  | App(t2,t3) -> is_free x t2 || is_free x t3
;;


(**********************************************************************
 rename : string -> string -> term -> term

 Usage: rename x x' t replaces all free occurrences of x in t as x'

 Pre: x' does not occur (free or bound) in t
 **********************************************************************)
let not_fresh x' = failwith ("Name " ^ x' ^ " is not fresh!");;

(* does the variable x occur in the term t? *)
let rec occurs x t = match t with
  | Var(y) when x=y -> true
  | Var(_) -> false

  | Abs(y,_) when y=x -> true
  | Abs(_,t2) -> occurs x t2

  | App(t2,t3) -> (occurs x t2) || (occurs x t3)
;;

let rec rename x x' t = match t with
  | Var(y) when y=x' -> not_fresh x'
  | Var(y) when y=x -> Var(x')
  | Var(y) -> Var(y)

  | Abs(y,_) when y=x' -> not_fresh x'
  | Abs(y,t2) when y=x -> if occurs x' t2 then not_fresh x' else Abs(y,t2)
  | Abs(y,t2) -> Abs(y,rename x x' t2)

  | App(t2,t3) -> App(rename x x' t2, rename x x' t3)
;;

(**********************************************************************
 equiv : term -> term -> bool

 Usage: equiv t1 t2 = true iff t1 and t2 are alpha-equivalent
 **********************************************************************)   
let rec equiv t1 t2 = match (t1,t2) with
  (* Var alone is a free variable *)
  | (Var(x), Var(y)) when x=y -> true
  | (Var(_), Var(_)) -> false

  (* x and y are bound. 
     When checking t1' and t2' alone, we are 'removing' this bound.
     So, rename them to be the same.
     If they're already the same, there is no need for renaming. *)
  | (Abs(x,t1'), Abs(y,t2')) when x=y -> equiv t1' t2'
  | (Abs(x,t1'), Abs(y,t2')) -> let fresh = "x"^(string_of_int (max (max_nat t1') (max_nat t2')) )
                                in equiv (rename x fresh t1') (rename y fresh t2')

  | (App(t1',t1''),App(t2',t2'')) -> (equiv t1' t2') && (equiv t1'' t2'')

  (* t1 and t2 are different types of terms altogether*)
  | _ -> false
;;
                   

(**********************************************************************
 subst : string -> term -> int -> term -> term * int

 Usage: subst x t1 vars t2 = ([x -> t1] t2,vars')  
        where vars is the index of the next variable to be used for renaming
        and vars' is the next available index after the substitution
 **********************************************************************)

(* We want to implement a substitution that
   - Replaces FREE occurrences of the variable x in t1 with the term t2
   - Avoids variable capture 
   
  The issue of variable capture occurs when we substitute the free variable with a term that contains variables that would be captured in the context.

  For example:
  [x -> z] (fun z . x) = fun z . z     (* WRONG! *)
      
  Our substitution will perform renames on t1 so that the variables in t2 will not be captured. Example:
  [x -> z] (fun z . x) = fun x1 . z 
   
  Note that in this case we will be renaming BOUND occurrences, the opposite of what is done in the rename function previously written. 
  
  Another example with a simple term substitution:
  [x -> z] (fun z . x) (fun y. x) (fun z. z) = (fun x1. z) (fun y. z) (fun x2. x2)

  Term with two variables: 
  [x -> y z] (fun y . x (fun w . x)) = fun x1 . (y z) (fun w . y z)
  
  [x -> y z] fun y . x  (fun z . x y z) = fun x3. (y z) (fun x4. ((y z) x3) x4) 
  
  *)

(* does t1 have variables that are captured by t2? *)
let term_captured t1 t2 = 
  let vars = vars_of_term t1 in
  (* variables that do not occur in t are not part of its free variables.
     variables are captured by t1 only if they occur in it and are not free. *)
  List.exists (fun x -> ((occurs x t2) && (not (is_free x t2)))) vars
;;

let rec subst x t1 vars t2 = match t2 with
  | Var(y) when x=y -> (t1, vars)
  | Var(y) -> (Var(y), vars)
  
  (* x is captured by this term: t3 won't have free occurrences to replace. *)
  | Abs(y,t3) when x=y -> (Abs(y,t3), vars) (* no free occurrences here *)

  (* x is not captured by this term, however y is among the variables
     that occur in t1 and therefore we have to avoid having unbound occurrences of x
     turn into terms that are partially bounded by y. *)
  | Abs(y,t3) when List.exists (fun z -> z=y) (vars_of_term t1)  ->
      print_endline ("DAS RITE! " ^ (string_of_term t1) ^ " captured by " ^ (string_of_term (Abs(y,t3))));
      let fresh_name = "x"^(string_of_int vars) in
      let renamed = rename y fresh_name t3 in
      let (substituted,vars') = subst x t1 (vars+1) renamed in
      (Abs(fresh_name, substituted), vars')
  | Abs(y,t3) -> 
      let (substituted, vars') = subst x t1 vars t3 in
      (Abs(y,substituted), vars')
  
  | App(t3,t4) -> 
      let (substituted3, vars3) = subst x t1 vars t3 in
      let (substituted4, vars4) = subst x t1 vars t4 in
      (App(substituted3,substituted4), max vars3 vars4)
;;

(**********************************************************************
 is_val : term -> bool

 Usage: is_val t = true iff t is a value (i.e., a lambda-abstraction)
 **********************************************************************)

let is_val = function
  | Abs(_,_) -> true
  | _ -> false
;;


exception NoRuleApplies

(**********************************************************************
 trace1 : int -> term -> term * int

 Usage: trace1 vars t performs a step of the small-step call-by-value semantics

 Pre:  xk does not occur in t, for all k>=vars
 **********************************************************************)

let rec trace1 vars t = match t with
  | App(Abs(x,t1), v2) when is_val v2 -> subst x v2 vars t1
  | App(v1,t2) when is_val v1 ->
      let (t2',vars') = trace1 vars t2 in
      (App(v1,t2'),vars')
  | App(t1,t2) -> 
      let (t1',vars') = trace1 vars t1 in 
      (App(t1',t2), vars')
  | _ -> raise NoRuleApplies
;;


(**********************************************************************
 trace_rec : int -> term -> term * int

 Usage: trace_rec n vars t performs n steps of the small-step semantics

 Pre:  xk does not occur in t, for all k>=vars
 **********************************************************************)

let trace n t = 
  let rec trace' n vars t = 
    if n=0 then [t]
    else try (let (t',vars') = trace1 vars t in
      t'::(trace' (n-1) vars' t'))
    with _ -> [t]
  in let vars = max_nat t 
  in trace' n vars t
;;
