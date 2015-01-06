
open Lang

exception TcErr of string

let tcErr s = raise (TcErr s)


(*******************************************************************************)

type type_env_binding =
  | VarTyp of var * typ
  | PolyVar of tvar
  | SelfVar of tvar * typ
  | Macro of macro_def

type type_env = type_env_binding list

let rec lookupVar x = function
  | [] -> tcErr (spr "var not defined: %s" x)
  | VarTyp (y,t) :: _  when x = y -> t
  | _::rest -> lookupVar x rest

let rec lookupSelfVar a = function
  | [] -> None
  | SelfVar (b,t) :: _ when a = b -> Some t
  | _ :: gamma -> lookupSelfVar a gamma

let rec lookupMacro m = function
  | [] -> None
  | Macro (m',xs,t) :: _ when m = m' -> Some (xs, t)
  | _ :: gamma -> lookupMacro m gamma


(*******************************************************************************)

(* TODO: capture avoidance *)
let rec subst (x,t) = function
  | TBase b -> TBase b
  | TMu (y, s) -> TMu (y, subst (x,t) s)
  | TAll (y, s) -> TAll (y, subst (x,t) s)
  | TVar y -> if x = y then t else TVar y
  | TRecd l -> TRecd (List.map (fun (f,tf) -> (f, subst (x,t) tf)) l)
  | TArrow (t1, t2) -> TArrow (subst (x,t) t1, subst (x,t) t2)
  | TMacroApp (m, ts) -> TMacroApp (m, List.map (subst (x,t)) ts)
  | TPre (y, s1, s2) -> TPre (y, subst (x,t) s1, subst (x,t) s2)
  (* | TPre (y, s1, s2) when x = y -> TPre (x, subst (x,t) s1, subst (x,t) s2) *)

let subst (x,t) s =
  let s' = subst (x,t) s in
  s'

let rec applySubst s = function
  | []    -> s
  | xt::l -> applySubst (subst xt s) l


(*******************************************************************************)

let expandTyp gamma =
  mapTyp begin function
    | TMacroApp (m, args) ->
        (match lookupMacro m gamma with
           | None -> tcErr (spr "undefined macro [%s]" m)
           | Some (xs, t) ->
               if List.length args <> List.length xs
               then tcErr (spr "wrong num of args to [%s]" m)
               else applySubst t (List.combine xs args))
    | t -> t
  end

let rec expandTypRec gamma t =
  let t' = expandTyp gamma t in
  if t = t' then t else expandTypRec gamma t'

let eqTyp gamma =
  let rec foo t1 t2 = match (expandTypRec gamma t1, expandTypRec gamma t2) with
    | TBase b1, TBase b2 -> b1 = b2
    | TVar x, TVar y -> x = y
    | TRecd l1, TRecd l2 ->
        let l1' = List.sort compare l1 in
        let l2' = List.sort compare l2 in
        List.for_all
          (fun ((f1,t1),(f2,t2)) -> f1 = f2 && foo t1 t2)
          (List.combine l1' l2')
    | TArrow (s1, t1), TArrow (s2, t2) -> foo s1 s2 && foo t1 t2
    (* TODO: alpha equivalence *)
    | TAll (x, t1), TAll (y, t2)
    | TMu (x, t1), TMu (y, t2) -> x = y && foo t1 t2
    (* TODO: TPre case *)
    | t1, t2 -> t1 = t2
  in foo


(*******************************************************************************)

module StrSet =
  Set.Make (struct type t = string let compare = compare end)

let implies : trecd -> trecd -> bool =
fun g r ->
  List.for_all (fun (f,tr) ->
    List.exists ((=) (f,tr)) g
  ) r

let relyThis a =
  List.map (fun (f,tf) ->
    match tf with
      | TArrow (TVar a', TArrow (s, t)) when a = a' -> (f, tf)
      | _ -> tcErr "bad relyThis"
  )

(* TODO: rename self var, if necessary *)
let tcClose gamma l =
  let (vars,rely,guar) =
    List.fold_left (fun (vars,rely,guar) (f,tf) ->
      let stf = strTyp tf in
      match expandTypRec gamma tf with
        | TPre (a, TRecd r, TArrow (s, t)) ->
            let vars = StrSet.add a vars in
            if StrSet.cardinal vars > 1
            then tcErr (spr "record has more than one self var [%s]" stf)
            else 
              let guar = (f, TArrow (TVar a, TArrow (s, t))) :: guar in
              let rely = rely @ relyThis a r in
              (vars, rely, guar)
        | tf' ->
            if tf = tf' then
              tcErr (spr "can't close record with field [%s]" stf)
            else
              let stf' = strTyp tf' in
              tcErr (spr "can't close record with field\n   [%s]\n = [%s]"
                stf stf')
    ) (StrSet.empty, [], []) l
  in
  if implies guar rely
  then let a = StrSet.min_elt vars in TMu (a, TRecd guar)
  else tcErr
         (spr "open record not consistent\n  [%s]\n\nrely\n  %s\n\nguar\n  %s"
           (strTRecd l) (strTRecd rely) (strTRecd guar))


(*******************************************************************************)

let _int = function TBase TInt -> Some true | _ -> None
let _str = function TBase TStr -> Some true | _ -> None

let _ints vs = getSomes (List.map _int vs)
let _strs vs = getSomes (List.map _str vs)

let app g f vs = fmapMaybe f (g vs)

let delta =
  [ ("+",  app _ints (fun is -> Some tInt))
  ; ("*",  app _ints (fun is -> Some tInt))
  ; ("^",  app _strs (fun ss -> Some tStr))
  ; ("-",  app _ints (function [i;j] -> Some tInt  | _ -> None))
  ; ("<=", app _ints (function [i;j] -> Some tBool | _ -> None))
  ]


(*******************************************************************************)

let rec tcVal : type_env -> value -> typ =
fun gamma -> function
  | VInt _   -> tInt
  | VBool _  -> tBool
  | VStr _   -> tStr
  | VRecd vs -> tc gamma (eRecd (List.map (fun (f,v) -> (f, eVal v)) vs))
  | VFun _   -> failwith "tcVal VFun"
  | VTFun _  -> failwith "tcVal VTFun"

and tc : type_env -> exp -> typ =
fun gamma exp -> match exp.exp with
  | EVal v -> tcVal gamma v
  | EVar x -> lookupVar x gamma
  | EFun (None, x, e) -> tcErr "lambda requires annotation"
  | EPremethod (None, _, _) -> tcErr "premethod requires annotation"
  | EFun (Some t1, x, e) ->
      let t2 = tc (VarTyp (x,t1) :: gamma) e in
      TArrow (t1, t2)
  | ETFun (a, e) ->
      let t = tc (PolyVar a :: gamma) e in
      TAll (a, t)
  | EPremethod (Some (a,s), x, e) ->
      let t = tc (SelfVar (a, s) :: VarTyp (x, TVar a) :: gamma) e in
      TPre (a, s, t)
  | EApp (e1, e2) ->
      (match tc gamma e1 with
         | TArrow (t11, t12) ->
             let t2 = tc gamma e2 in
             if eqTyp gamma t2 t11
             then t12
             else tcErr (spr "expected\n  [%s]\n\nactual\n  [%s]"
                    (strTyp t11) (strTyp t2))
         | t ->
             tcErr (spr "trying to call non-function [%s]" (strTyp t)))
  | ETApp (e, tInst) ->
      (match tc gamma e, tInst with
         | TAll (a, s), _ -> subst (a, tInst) s
         | TPre (a, sa, t), TVar b ->
             (match lookupSelfVar b gamma with
                | Some sb ->
                    if subst (a, TVar b) sa = sb
                    then TArrow (TVar b, subst (a, TVar b) t)
                    else tcErr (spr "ETApp bad bounds [%s] [%s]"
                           (strTyp sa) (strTyp sb))
                | None -> tcErr "ETApp 3")
         | t, _ -> tcErr
                     (spr "ETApp 1:\n [%s]\n\n [%s]\n"
                       (strTyp t) (strTyp tInst)))
  | EPrimOp (op, es) ->
      if not (List.mem_assoc op delta)
      then evalErr (spr "no type for operator : (%s)" op)
      else
        let f = List.assoc op delta in
        let ts = List.map (tc gamma) es in
        (match f ts with
           | Some res -> res
           | None ->
               tcErr (spr "bad arguments to [%s]:\n  [%s]" op
                 (String.concat " " (List.map strTyp ts))))
  | ELet (x, e1, e2) ->
      let t1 = tc gamma e1 in
      let _ =
        if false
        then pr "%s : %s\n" x (strTyp t1)
        else pr "%s OK\n" x
      in
      tc (VarTyp (x, t1) :: gamma) e2
  | EIf (e1, e2, e3) ->
      (match tc gamma e1, tc gamma e2, tc gamma e3 with
         | TBase TBool, t, t' when t = t' -> t
         | _ -> tcErr "bad if-expression")
  | ERecd l ->
      TRecd (List.map (fun (f, e) -> (f, tc gamma e)) l)
  | EProj (e, f) ->
      let t = tc gamma e in
      (match expandTyp gamma t with
         | TRecd l when List.mem_assoc f l -> List.assoc f l
         | TRecd _ -> tcErr (spr "tc proj: field [%s] not found in\n  [%s]"
                        f (strTyp t))
         | t       -> tcErr (spr "tc proj: not record: %s" (strTyp t)))
  | EWith (e1, f, e2) ->
      (match tc gamma e1 with
         | TRecd l when List.mem_assoc f l ->
             TRecd ((f, tc gamma e2) :: (List.remove_assoc f l))
         | TRecd _ -> tcErr "record doesn't already have field"
         | _ -> tcErr "can't extend non-record")
  | EUnfold e ->
      (match expandTyp gamma (tc gamma e) with
         | TMu (x, s) as t -> subst (x,t) s
         | TVar a ->
             (match lookupSelfVar a gamma with
                | Some t -> t
                | None   -> tcErr (spr "can't unfold type var [%s]" a))
         | t ->
             tcErr (spr "can't unfold [%s]" (strTyp t)))
  | EFold (t, e) ->
      let t0 = expandTyp gamma t in
      (match t0 with
         | TMu (x, s) ->
             let t' = tc gamma e in
             let t'' = subst (x,t0) s in
             if eqTyp gamma t' t'' then t
             else
               tcErr (spr "can't fold\n [%s]\n\ninto\n [%s]\n\nbecause\n [%s]"
                 (strTyp t') (strTyp t) (strTyp t''))
         | _ -> tcErr (spr "can't fold [%s]" (strTyp t)))
  | EClose e ->
      let t = tc gamma e in
      (match expandTyp gamma t with
         | TRecd l -> tcClose gamma l
         | _ -> tcErr (spr "can't close non-record type [%s]" (strTyp t)))
  | EPrint (_, e) ->
      let _ = tc gamma e in
      tInt
  | EMacroDef (md, e) ->
      tc (Macro md :: gamma) e


(*******************************************************************************)

let tcExp e =
  try
    pr "%s : %s\n\n" (greenString "Type OK") (strTyp (tc [] e))
  with TcErr s ->
    pr "%s :\n%s\n\n" (redString "Type Error") s

