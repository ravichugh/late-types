
open Lang

let mapSnd f = List.map (fun (x,y) -> (x, f y))

(* Big-Step Semantics *)

let steps = ref 0
let strDiverge = "Diverging?"

let _int = function VInt i -> Some i | _ -> None
let _str = function VStr s -> Some s | _ -> None

let _ints vs = getSomes (List.map _int vs)
let _strs vs = getSomes (List.map _str vs)

let app g f vs = fmapMaybe f (g vs)

let delta =
  [ ("+",  app _ints (fun is -> Some (VInt (List.fold_left (+) 0 is))))
  ; ("*",  app _ints (fun is -> Some (VInt (List.fold_left ( * ) 1 is))))
  ; ("^",  app _strs (fun ss -> Some (VStr (List.fold_left (^) "" ss))))
  ; ("-",  app _ints (function [i;j] -> Some (VInt (i-j)) | _ -> None))
  ; ("<=", app _ints (function [i;j] -> Some (VBool (i<=j)) | _ -> None))
  ]

let rec eval env e =
  incr steps;
  if !steps > 100000 then evalErr strDiverge
  else match e.exp with
  | EVal v -> v
  | EFun (_,x,e) -> VFun (env, x, e)
  | ETFun (a,e) -> VTFun (env, a, e)
  | EPremethod (_,x,e) ->
      eval env (eTFun "SELF" (eFun (Some (TVar "SELF")) x e))
  | EVar x -> if List.mem_assoc x env then List.assoc x env
              else evalErr (spr "not found: %s" x)
  | ELet (x, e1, e2) -> eval ((x, eval env e1) :: env) e2
  | EPrimOp (op, es) -> evalPrimOp env op es
  | EIf (e1, e2, e3) ->
      (match eval env e1 with
         | VBool true -> eval env e2
         | VBool false -> eval env e3
         | _ -> evalErr "if-guard not a boolean")
  | EApp (e1, e2) ->
      (match eval env e1, eval env e2 with
         | VFun (env',x,e), v2 -> eval ((x,v2)::env') e
         | v1, v2 ->
             evalErr (spr "EApp\n [%s]\n [%s]" (strVal v1) (strVal v2)))
  | ETApp (e, t) ->
      (match eval env e with
         | VTFun (env', _, body) -> eval env' body
         | v -> evalErr (spr "ETApp\n [%s]\n [%s]\n" (strVal v) (strTyp t)))
  | ERecd es -> VRecd (mapSnd (eval env) es)
  | EWith (e1, f, e2) ->
      (match eval env e1 with
         | VRecd l -> VRecd ((f, eval env e2) :: (List.remove_assoc f l))
         | _ -> evalErr "EWith")
  | EProj (e, f) ->
      (match eval env e with
         | VRecd l ->
             if List.mem_assoc f l
             then (* let _ = pr "read %s\n" f; flush stdout in *) List.assoc f l
             else evalErr (spr "field [%s] not found in %s" f (strVal (VRecd l)))
         | _ -> evalErr "EProj")
  | EUnfold e | EFold (_, e) ->
      eval env e
  | EClose e ->
      (match eval env e with
         | VRecd vs ->
             let tDummy = TVar "XXX" in
             eval env (eRecd (mapSnd (fun v -> eTApp (eVal v) tDummy) vs))
         | v -> evalErr (spr "can't close non-record [%s]" (strVal v)))
  | EPrint (so, e) ->
      (* let s = match so with Some s -> spr "%-40s --  " s | None -> "" in *)
      (* let _ = pr "%s%s\n" s (strVal (eval env e)) in *)
      let s = match so with Some s -> s | None -> "" in
      let _ = pr "%s\n  -- %s\n\n" s (strVal (eval env e)) in
      VInt 0
  | EMacroDef (_, e) ->
      eval env e

and evalPrimOp env op es =
  if not (List.mem_assoc op delta)
  then evalErr (spr "operator not implemented: (%s)" op)
  else
    let f = List.assoc op delta in
    let vs = List.map (eval env) es in
    match f vs with
      | Some res -> res
      | None ->
          evalErr (spr "bad arguments to [%s]:\n  [%s]" op
            (String.concat " " (List.map strVal vs)))

let run e =
  try begin
    steps := 0;
    pr "%s = %s\n" (greenString "Result") (strVal (eval [] e));
  end with
    | EvalErr s when s = strDiverge ->
        pr "%s = %s\n" (yellowString "Result") strDiverge
    | e -> raise e

