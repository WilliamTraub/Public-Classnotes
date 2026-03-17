let f (x : int ref) (y : int ref) : int option =
  if x == y
  then None
  else (y := 0; Some(!x))

let fwrapper (x : int) (y : int) (alias : bool) : (int option * int * int) =
  let (xref, yref) =
    let xref = ref x in 
    if alias
    then (xref, xref)
    else 
      let yref = ref y in
      (xref, yref)
  in
  let o = (f xref yref) in
  (o, !xref, !yref)

let prop_mut (ix, iy, alias) : bool =
  let (o, fx, fy) = fwrapper ix iy alias in
  if alias
  then o = None && ix = fx && iy = fy
  else 
    match o with
    | None -> false
    | Some v -> v = fx && fy = 0 && fx = ix