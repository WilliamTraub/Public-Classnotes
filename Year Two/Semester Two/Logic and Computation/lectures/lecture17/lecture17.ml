type value = 
  | Nat of int
  | Bool of bool

type op = | Add | Mul | And | Eq

type expr =
  | Value of value
  | Var of string
  | Op of op * expr list
  | If of expr * expr * expr
  | Let of string * expr * expr

(*let rec eval_op op xs =
  (match op, List.map eval xs with
  | Add, [Nat a; Nat b] -> Nat (a + b))*)