type value = int
type op = | Add | Mul | Or | And | LT | EQ
type instr =
  | Push of value
  | PrimApp of op
  | Set of string
  | Get of string

  (* TODO: needed for ite *)
  | Jump of int
  | JumpIfNotZero of int

let eval_op (op : op) (args : value list) : value =
  match (op, args) with
  | (Add, [n1; n2]) -> (n1 + n2)
  | (Mul, [n1; n2]) -> (n1 * n2)
  | (And, [b1; b2]) -> (if b1 = 1 && b2 = 1 then 1 else 0)
  | (Or, [b1; b2]) -> (if b1 = 1 || b2 = 1 then 1 else 0)
  | (LT, [n1; n2]) -> (if n1 < n2 then 1 else 0)
  | (EQ, [e1; e2]) -> (if e1 = e2 then 1 else 0)
  | _ -> failwith "runtime error: arity mismatch or unexpected argument"
  
module StrMap = Map.Make(String)

type config = instr list * value list * value StrMap.t
let step (cfg : config) : config = 
  match cfg with
  | ([], s, h) -> cfg
  | (hd::tl, s , h) -> (match hd with
    | Push v -> (tl, v::s, h)
    | PrimApp op -> (match s with 
      | l::r::rst -> (tl, (eval_op op [l; r]::rst), h)
      | _ -> failwith "PANIC! malformed program")
    | Set x -> (match s with 
      | v::rst -> (tl, rst, (StrMap.add x v h))
      | _ -> failwith "PANIC! malformed program")
    | Get x -> (match StrMap.find_opt x h with
      | None -> failwith "PANIC"
      | Some v -> (tl, v::s, h))
    
    | Jump _ -> failwith "..."
    | JumpIfNotZero _ -> failwith "...")


let execute (lst : instr list) : value =
  let rec loop cfg =
    let cfg' = step cfg in
    if cfg' = cfg
    then (match cfg' with
      |  ([], hd::tl, _) -> hd
      | _ -> failwith "PANIC")
    else loop cfg'
  in
  loop (lst, [], StrMap.empty)

let ex0 = [Push 2; Push 3; PrimApp Add]
let ex1 = [Push 1; Push 3; PrimApp Or]
let ex2 = [Push 1; Push 3; PrimApp Or; Set "potatoes"; Push 3; PrimApp Add]