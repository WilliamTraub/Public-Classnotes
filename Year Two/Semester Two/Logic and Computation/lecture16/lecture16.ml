type expr =
  | Nat of int
  | Add of expr * expr
  | Mul of expr * expr

let rec string_of_expr e =
  match e with
  | Nat i -> string_of_int i
  | Add (l, r) -> "(" ^ string_of_expr l ^ " + " ^ string_of_expr r ^ ")"
  | Mul (l, r) -> "(" ^ string_of_expr l ^ " * " ^ string_of_expr r ^ ")"

type token =
  | NAT of int
  | PLUS
  | TIMES
  | LPAREN
  | RPAREN
  | EOF