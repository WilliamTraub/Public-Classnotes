module type EvenSig = sig
  type t
  val zero : t
  val two : t
  val add : t -> t -> t
  val mult : t -> int -> t    
  val get : t -> int
end


module Even : EvenSig = struct
  type t = int 
  let zero = 0   
  let two = 2  
  let well_formed i = i mod 2 = 0

  let add x y = x + y 
  let mult x y = x * y
  let get i = i
end

module type CounterSig = sig
  type t
  val fresh : unit -> t
  val bump : t -> t
  val get : t -> int
end

module Counter (* CounterSig *) = struct
  type t = int
  let fresh () = 0
  let bump i = i + 1
  let get x = x
end

(* let rec count_evens (xs: int list) (ctr : Counter.t) =
  match xs with
  | [] -> Counter.get ctr 
  | x :: xs' ->
    let ctr' = if x mod 2 = 0 then Counter.bump ctr else ctr in
    count_evens xs' ctr' *)

module CounterWithId (* : CounterSig *) = struct
  type t = {
    uid : int;
    value : int;
  }
  let fresh () =
    let uid = Random.int 1000 in
    { uid ; value = 0 }
  
  let bump (ctr : t) : t =
    let uid = Random.int 1000 in
    let _ = print_endline ("Bumping the counter, new uid: " ^ string_of_int uid) in { uid = ctr.uid ; value = ctr.value + 1 }

    let get c = c.value
end

module AppBuilder ( C : CounterSig) = struct
  let rec count_evens (xs: int list) (ctr : C.t) =
    match xs with
    | [] -> C.get ctr 
    | x :: xs' ->
      let ctr' = if x mod 2 = 0 then C.bump ctr else ctr in
      count_evens xs' ctr'
end
module App = AppBuilder (Counter)
module AppDebug = AppBuilder (CounterWithId)

module CounterTester (C : CounterSig) = struct
  let rec gen_counter (n : int) : C.t =
    if n <= 0
    then C.fresh ()
    else C.bump (gen_counter (n - 1))

    let test_counter (n : int) : bool =
      let open C in
      let c = gen_counter n in
      get (bump c) = get c + 1
end

module type MySetSig = sig
  type 'a t 
  val empty : 'a t  
  val mem : 'a -> 'a t -> bool  
  val add : 'a -> 'a t -> 'a t  
  val remove : 'a -> 'a t -> 'a t 
end

module type EqType = sig
  type t
  val equals : t -> t -> bool
end

module MySet : MySetSig = struct 
  type 'a t = 'a list
  let empty : 'a t = []
  let rec mem (x : 'a) (s : 'a t) : bool = List.mem x s
  let add (el : 'a) (s : 'a t) : 'a t = el :: s
  let rec remove (x : 'a) (s : 'a t) : 'a t 
    = List.filter ((<>) x) s
end

module UnorderedPair = struct
  type t = int * int
  let equals (p1 : t) (p2 : t) : bool
  = (fst p1 = fst p2 && snd p1 = snd p2)
  && (snd p1 = fst p2 && fst p1 = snd p2)
end