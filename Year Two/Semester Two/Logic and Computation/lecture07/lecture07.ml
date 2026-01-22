(* Code 1 *)
let rec all (xs : int list) (p : int -> bool) : bool = 
    match xs with
    | [] -> true
    | x :: xs' -> p x && all xs' p

let forall_0_5 = all [0; 1; 2; 3; 4; 5]

(* Code 3 *)
(*Property 1*)
let rec sorted (ys : int list) : bool =
  match ys with
  | [] | _ :: [] -> true (* fall through *)
  | i :: j :: ys' -> i <= j && sorted (j :: ys')
(*Prop 2*)
let rec ndups (e : int) (xs : int list) : int 
  = match xs with
  | [] -> 0
  | x :: xs' -> (if x = e then 1 else 0) + (ndups e xs')
let equal_element_count (xs : int list) (ys : int list) : bool 
  = all xs (fun e -> ndups e xs = ndups e ys) 
  && all ys (fun e -> ndups e xs = ndups e ys)

(* Code 4*)
let prop_sort_correct (xs : int list) (sort : int list -> int list) : bool =
  let ys = sort xs in 
  sorted ys && equal_element_count xs ys

(*Code 5*)
let get_uniform_list (len_bound : int) : unit -> int list = fun () ->
  let len = Random.int len_bound in
  List.init len (fun _ix -> Random.int 100)

(* Code 6*)
let rec forall_intlist 
  (gen : unit -> int list)
  (prop : int list -> bool)
  (trials : int) 
  : (int list) option
  = if trials <= 0
    then None
    else
      let xs = gen () in
      if not (prop xs)
      then Some xs
      else forall_intlist gen prop (trials - 1)