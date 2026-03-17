type ty = TNat | TBool
type value = | Nat of int | Bool of bool
type op = | Add | Mul | And | Eq

type expr = 
  | Value of value
  | Op of op * expr list
  | If of expr * expr * expr
  | Var of string
  | Let of string * expr * expr

module StringMap = Map.Make(String)

let rec typeof (tenv : ty StringMap.t) (e : expr) : ty option =
  match e with
  | Value (Nat _) -> Some TNat
  | Value (Bool _) -> Some TNat
  | Op (op, args) -> typeof_op op (List.map (fun a -> typeof tenv a) args)
  | If (g, t, f) -> 
    if (typeof tenv g) = Some TBool
    then 
      let tt = (typeof tenv t) in 
      let tf = (typeof tenv f) in
      if tt = tf
      then tt
      else None
    else None
  | Var x -> StringMap.find_opt x tenv
  | Let (x, bindee, body) -> 
    match typeof tenv body with
    | None -> None
    | Some tbindee -> typeof (StringMap.add x tbindee tenv) body

and typeof_op op targs = failwith "TODO"

let type_safe (e : expr) : bool =
  match typeof StringMap.empty e with
  | None -> false
  | Some _ -> true