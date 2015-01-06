
type base_type = TInt | TBool | TStr

type field = string
type tvar = string
type var = string 
type macro = string

type typ =
  | TBase of base_type
  | TRecd of trecd
  | TArrow of typ * typ
  | TPre of tvar * typ * typ
  | TVar of tvar
  | TMu of tvar * typ
  | TAll of tvar * typ
  | TMacroApp of macro * typ list

and trecd = (field * typ) list

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VRecd of (field * value) list
  | VFun of eval_env * var * exp
  | VTFun of eval_env * tvar * exp

and exp_ =
  | EVal of value
  | EVar of var
  | EFun of typ option * var * exp
  | ETFun of tvar * exp
  | EPremethod of (tvar * typ) option * var * exp
  | EApp of exp * exp
  | ETApp of exp * typ
  | ERecd of (field * exp) list
  | EWith of exp * field * exp
  | EProj of exp * field 
  | EFold of typ * exp
  | EClose of exp
  | EUnfold of exp
  | ELet of var * exp * exp
  | EIf of exp * exp * exp
  | EPrimOp of string * exp list
  | EPrint of string option * exp
  | EMacroDef of macro_def * exp

and exp = { exp:exp_ }

and eval_env = (var * value) list 

and macro_def = macro * tvar list * typ

exception EvalErr of string

(*******************************************************************************)

let pr = Printf.printf
let spr = Printf.sprintf

let rec strVal = function
  | VInt i  -> spr "%d" i
  | VBool b -> spr "%b" b
  | VStr s  -> spr "\"%s\"" s
  | VFun (_, x, _) -> spr "<%s -> ...>" x
  | VTFun (_, x, _) -> spr "<tfun %s -> ...>" x
  | VRecd l -> spr "{%s}" (String.concat "; "
                 (List.map (fun (f,v) -> spr "%s=%s" f (strVal v)) l))

let rec strTyp = function
  | TBase TInt -> "int"
  | TBase TStr -> "str"
  | TBase TBool -> "bool"
  | TRecd l -> strTRecd l
  | TVar a -> a
  | TArrow (s, t) -> spr "(%s -> %s)" (strTyp s) (strTyp t)
  | TPre (a, s, t) -> spr "(%s:%s) => %s" a (strTyp s) (strTyp t)
  | TMu (a, t) -> spr "mu %s. %s" a (strTyp t)
  | TAll (a, t) -> spr "all %s. %s" a (strTyp t)
  | TMacroApp (m, ts) ->
      spr "%s(%s)" m (String.concat ", " (List.map strTyp ts))

and strTRecd l =
  spr "{%s}" (String.concat "; "
    (List.map (fun (f, t) -> spr "%s: %s" f (strTyp t)) l))

let freshVar =
  let i = ref 0 in
  fun () ->
    incr i;
    spr "__%d" !i

let evalErr s = raise (EvalErr s)

let wrapE e = { exp = e }

let eTrue  = wrapE (EVal (VBool true))
let eFalse = wrapE (EVal (VBool false))
let eInt i = wrapE (EVal (VInt i))
let eVar x = wrapE (EVar x)
let eVal x = wrapE (EVal x)

let eThis  = eVar "this"
let eUnit  = eInt 0
let xWild  = "_"
let eWild  = EVar xWild

let eProj x y    = wrapE (EProj (x, y))
let eFun t x y   = wrapE (EFun (t, x, y))
let eApp x y     = wrapE (EApp (x, y))
let eTFun x y    = wrapE (ETFun (x, y))
let eTApp x y    = wrapE (ETApp (x, y))
let eIf x y z    = wrapE (EIf (x, y, z))
let eLet x y z   = wrapE (ELet (x, y, z))

let ePrimOp x ys = wrapE (EPrimOp (x, ys))
let ePlus xs     = ePrimOp "+" xs
let eMinus x y   = ePrimOp "-" [x; y]
let eMul xs      = ePrimOp "*" xs
let eLe x y      = ePrimOp "<=" [x; y]

let eRecd l          = wrapE (ERecd l)
let eWith e1 f e2    = wrapE (EWith (e1, f, e2))

let eClose  e   = wrapE (EClose e)
let eFold t e   = wrapE (EFold (t, e))
let eUnfold e   = wrapE (EUnfold e)

let ePrint s e  = wrapE (EPrint (s, e))

let tInt  = TBase TInt
let tBool = TBase TBool
let tStr  = TBase TStr
let tUnit = tInt


(*******************************************************************************)

let redString s    = Printf.sprintf "\027[31m%s\027[0m" s
let greenString s  = Printf.sprintf "\027[32m%s\027[0m" s
let yellowString s = Printf.sprintf "\027[33m%s\027[0m" s

let terminate () = flush stdout; exit 1

let printBig cap s =
  pr "\n%s\n%s\n\n%s\n\n" (String.make 80 '-') cap s

let printErr cap s =
  printBig cap s;
  pr "%s\n" (redString cap);
  terminate ()

let printParseErr s = printErr "PARSE ERROR!" s

let printTcErr () = printErr "TC: ERROR!" ""

let strPrefix s pre =
  let n = String.length s in
  let m = String.length pre in
    (n >= m) && (String.sub s 0 m = pre)


(*******************************************************************************)

let rec mapTyp f = function
  | TBase b -> f (TBase b)
  | TMu (x, s) -> f (TMu (x, mapTyp f s))
  | TAll (x, s) -> f (TAll (x, mapTyp f s))
  | TVar x -> f (TVar x)
  | TRecd l -> f (TRecd (List.map (fun (g,tf) -> (g, mapTyp f tf)) l))
  | TArrow (t1, t2) -> f (TArrow (mapTyp f t1, mapTyp f t2))
  | TPre (x, s, t) -> f (TPre (x, mapTyp f s, mapTyp f t))
  | TMacroApp (m, ts) -> f (TMacroApp (m, List.map (mapTyp f) ts))


(*******************************************************************************)

let getSomes : ('a option) list -> ('a list) option =
function
  | (Some x)::mxs ->
      List.fold_left (fun acc mx ->
        match mx, acc with
          | Some x, Some xs -> Some (xs@[x])
          | _               -> None
      ) (Some [x]) mxs
  | _ -> None

let fmapMaybe f = function
  | Some x -> f x
  | None   -> None

