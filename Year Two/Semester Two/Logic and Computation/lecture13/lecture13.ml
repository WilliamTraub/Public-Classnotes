(* Part 1 - Functors cont *)
module type SetSig = sig
  type elem
  type t
  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool
end

module type EqType = sig
  type t
  val equals : t -> t -> bool
end

module ListSet (T : EqType) : (SetSig with type elem = T.t) = struct
  type elem = T.t
  type t = elem list
  let empty = []
  let add x xs = x :: xs
  let rec mem x xs =
    match xs with
    | [] -> false
    | y :: ys -> if T.equals x y then true else mem x ys
end

module StringEq : (EqType with type t = string) = struct
  type t = string
  let equals s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
end

module StringSet = ListSet(StringEq)

module ListEq (T : EqType) : (EqType with type t = T.t list) = struct
  type t = T.t list
  let rec equals xs ys =
    match xs, ys with 
    | [] , [] -> true
    | x :: xs' , y :: ys' -> T.equals x y && equals xs' ys'
    | _ , _ -> false
end

module StringListSet = ListSet(ListEq(StringEq))

let _ =
  let xs = StringListSet.(add["a";"b"] empty) in
  ()



(* Part 2 - Mutability *)
let add_one (x : int ref) = !x + 1
let f () =
  let x = ref 1 in
  let y = fun _ -> add_one x in
  x := 7;
  print_endline ("Y : " ^ string_of_int (y ()))

let _ = f ()

let test () =
  let f x =
    let y = ref 10 in
    y := !y + 1;
    x + !y
  in
  let y = ref 0 in
  f 2
  (* returns 13 and the second to last line is ignored *)

let f = 
  let y = ref 0 in
    fun () ->
      y := !y + 1;
      !y

let p_to_f : (int -> int) ref = ref (fun x -> x)
  let double_p_to_f () =
    let f = !p_to_f in
      p_to_f := (fun x -> f x * 2)
