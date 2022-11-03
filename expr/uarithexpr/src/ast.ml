(* in this solution I removed True and False, and changed the parser,
   changing the abstract syntax. It works but the idea of the exercise
   was to change only the semantics *)
type expr =
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr
