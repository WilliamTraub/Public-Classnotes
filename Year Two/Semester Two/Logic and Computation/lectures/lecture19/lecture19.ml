type matrix = {
  ncols : int;
  nrows : int;
  value : float list list;
}

let sum_list (xs : float list) =
  List.fold_left (+.) 0. xs

let build_matrix 
  (n : int) 
  (m : int) 
  (f : int -> int -> float) 
  : matrix =
  { nrows = n; ncols = m; 
  value = List.init n 
    (fun i -> List.init m
      (fun j ->
        f i j))
  }

let gen_matrix n m mx =
  build_matrix n m (fun i j -> Random.float 1.0)

let ix i j (m : matrix) : float
  = List.nth (List.nth m.value i) j
let rec mmult (a : matrix) (b : matrix) : matrix =
  build_matrix a.nrows b.ncols 
    (fun x y -> 
      let products = 
        List.init a.ncols (fun i -> 
          (ix x i a) *. (ix i y b))
        in
          sum_list products
      
    )

