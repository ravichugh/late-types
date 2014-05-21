type typ = 
  | Int 
  | Arrow of typ * typ

type exp = 
  | EInt of int 
  | EVar of int
  | EApp of exp * exp
  | EFun of typ option * exp 

