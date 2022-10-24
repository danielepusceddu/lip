type nat = 
  Zero
| Succ of nat
| Pred of nat
;;

type boolExpr =
    True
  | False
  | Not of boolExpr
  | And of boolExpr * boolExpr
  | Or of boolExpr * boolExpr
  | If of boolExpr * boolExpr * boolExpr
  | IsZero of nat
;;

type expr = BoolExpr of boolExpr | NatExpr of nat;;
type exprval = Bool of bool | Nat of int;;