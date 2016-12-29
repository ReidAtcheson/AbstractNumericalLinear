module type HilbertSpace = sig
  (*Vector type*)
  type vect
  (*Complex number type*)
  type ct 
  (*Null vector*)
  val nullvector : vect
  (*Basis functions. basis i = i-th basis function*)
  val basis      : int -> vect
  (*Scalar multiplication: constant times a vector*)
  val scalarmul  : ct -> vect -> vect
  (*Vector addition*)
  val       add  : vect -> vect -> vect
  (*Inner product*)
  val innerprod  : vect -> vect -> ct
  (*Vector Norm*)
  val norm       : vect -> ct
  (*Convenience printing function*)
  val to_string  : vect -> string
end;;


module HilbertSpaceDSL (C : Gencomplex.Sig) (H : HilbertSpace with type ct=C.t)  = struct

  type t = 
    | Vect of H.vect 
    | Num of H.ct

  let num_to_t n  = Num n
  let num_from_t n = match n with
    Num  c -> c
  | _ -> raise (Failure "Expected a number")


  let vect_to_t x = Vect x
  let vect_from_t x = match x with
   Vect  v -> v
  | _ -> raise (Failure "Expected a vector")




  (*Complex number members*)
  (*Complex conjugate*)
  let conj n = match n with
       Num  c -> Num (C.conj c)
    | _ -> raise (Failure "Expected a number")
  (*Square root*)
  let sqrt n = match n with
     Num  c -> Num (C.sqrt c)
    | _ -> raise (Failure "Expected a number")
  (*Complex absolute value*)
  let abs n = match n with
      Num  c -> Num (C.abs c)
    | _ -> raise (Failure "Expected a number")
  (*Additive identity*)
  let zero = Num (C.zero)
  (*Multiplicative identity*)
  let one = Num (C.one)
  let almost_equal m n tol = match m,n with
    Num c,Num d -> C.almost_equal c d tol
   |    _,_       -> raise (Failure "Expects numbers")




  (*Hilbert space members*)
  let nullvector = Vect (H.nullvector)
  let basis i = Vect (H.basis i)
  let innerprod x y = match x,y with
    Vect u,Vect   v -> Num (H.innerprod u v)
  | _,_               -> raise (Failure "Inner product only supports vectors")

  let norm x = match x with
     Vect u -> Num (H.norm u)
  | _       -> raise (Failure "Vector norm attempted of non-vector")

  let to_string x = match x with
      Vect u -> H.to_string u
    | Num  c  -> C.to_string c

  let re r = Num (C.re r)
  let im i = Num (C.im i)




  let ( * ) x y = match x,y with
    Vect u, Num   c -> Vect (H.scalarmul c u)
  | Num  c, Vect  u -> Vect (H.scalarmul c u)
  | Num  c, Num    d -> Num (C.mul c d)
  |      _,_         -> raise (Failure "Argument mismatch in *")

  let ( + ) x y = match x,y with
     Vect u,Vect   v -> Vect (H.add u v)
  |  Num  c, Num     d -> Num (C.add c d)
  |      _,_           -> raise (Failure "Argument mismatch in +")


  let ( - ) x y = match x,y with
     Vect u,Vect   v -> Vect (H.add u (H.scalarmul (C.neg C.one) v))
  |  Num  c, Num     d -> Num (C.add c (C.mul (C.neg C.one) d))
  |      _,_           -> raise (Failure "Argument mismatch in -")



  let ( / ) x y = match x,y with
    Vect u, Num   c ->  Vect (H.scalarmul (C.inv c) u)
  | Num   c, Num   d ->  Num (C.mul (C.inv d) c)
  |       _,_        ->  raise (Failure "Argument mismatch in /")








end;;


module type Orthogonalizable = sig
  (*Type of complex numbers*)
  type ct
  (*Type of vectors*)
  type vect
  (*Orthogonalize two vectors*)
  val orthogonalize2 : vect -> vect -> vect list
  (*Orthogonalize array of vectors*)
  val orthogonalize  : vect array -> vect array
  (*QR factorization of array of vectors*)
  val qr : vect array -> (vect array) * (ct array array)
end;;


module type OperatorSpace = sig
  (*Type of complex numbers*)
  type ct
  (*Type of input vectors*)
  type vect1
  (*Type of output vectors*)
  type vect2
  (*Compute adjoint of linear operator*)
  val adj            : int -> (vect1 -> vect2) -> vect2 -> vect1
  (*Compute operator norm of linear operator*)
  val opnorm         : int -> int -> (vect1 -> vect2) -> ct
end;;

module type SquareOperatorSpace = sig
  (*Type of complex numbers*)
  type ct
  (*Type of input vectors*)
  type vect
  (*Type of output vectors*)
  (*Compute adjoint of linear operator*)
  val adj            : int -> (vect -> vect) -> vect -> vect
  (*Compute operator norm of linear operator*)
  val opnorm         : int -> int -> (vect -> vect) -> ct
  (*Run QR iteration: ideally results in eigenvalues*)
  val qriters        : int -> int -> (vect -> vect) -> ct array
end;;




module MakeOrthogonalizable (C : Gencomplex.Sig) (H : HilbertSpace with type ct=C.t) : Orthogonalizable with type vect=H.vect with type ct = C.t= struct


  type ct=C.t
  type vect=H.vect
  let orthogonalize2 x y = 
    let proj u v = H.scalarmul (C.mul (H.innerprod u v) (C.inv (H.innerprod u u))) u in
    let normalize u = H.scalarmul (C.inv (H.norm u)) u in
    let nx = normalize x in
    let ey = H.add y (H.scalarmul (C.neg C.one) (proj x y)) in
    let ny = normalize ey in
    [nx;ny]


  let orthogonalize xs = 
    let normalize u = H.scalarmul (C.inv (H.norm u)) u in
    let m = Array.length xs in
    let qs=xs in
    for k = 0 to (m-1) do
      let w = ref xs.(k) in
      for j = 0 to (k-1) do
        let rjk = H.innerprod (!w) qs.(j) in
        w := H.add (!w) (H.scalarmul (C.neg rjk) qs.(j));
      done;
      qs.(k) <- normalize !w
    done;
    qs


  let qr xs_ = 
    let xs = Array.copy xs_ in
    let normalize u = H.scalarmul (C.inv (H.norm u)) u in
    let m = Array.length xs in
    let r = Array.make_matrix m m (C.zero) in
    let qs=xs in
    for k = 0 to (m-1) do
      let w = ref qs.(k) in
      for j = 0 to (k-1) do
        let rjk = H.innerprod (!w) qs.(j) in        
        w := H.add (!w) (H.scalarmul (C.neg rjk) qs.(j));
        r.(j).(k) <- rjk;
      done;
      r.(k).(k) <- H.norm !w;
      qs.(k) <- normalize !w;
    done;
    (qs,r)





end;;





module TestHilbertSpace (C : Gencomplex.Sig) (H : HilbertSpace with type ct=C.t) = struct

  module Dsl = HilbertSpaceDSL (C) (H) ;;


  let () = Random.init 472398 
  let zr  = (Random.float 10.0) -. (Random.float 10.0)
  let zi  = (Random.float 10.0) -. (Random.float 10.0)

  let test_nullvector = 
    Dsl.(
      let z = nullvector in
      let b = basis 3 in
      let res = z+b in
      let err = norm (res - b) in
      let is_correct = almost_equal err zero 1e-5 in
      if (is_correct) then (print_endline "PASS: nullvector") else (print_endline "FAIL: nullvector")
    )


  let test_lincomb1 = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let orig = z*(b1 + b2) in
      let expand = z*b1 + z*b2 in
      let err = norm (orig - expand) in
      let is_correct = almost_equal err zero 1e-5 in
      if (is_correct) then (print_endline "PASS: linear combinations 1") else (print_endline "FAIL: linear combinations 1")
    )



  let test_lincomb2 = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let orig = z*(b1 + b2) in
      let expand = z*b1 + z*b2 in
      let err = norm (orig - expand) in
      let is_correct = almost_equal err zero 1e-5 in
      if (is_correct) then (print_endline "PASS: linear combinations 2") else (print_endline "FAIL: linear combinations 2")
    )

  let test_lincomb3 = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let b  = b2 - b1 in
      let orig = z*(b + b) in
      let expand = z*b + z*b in
      let err = norm (orig - expand) in
      let is_correct = almost_equal err zero 1e-5 in
      if (is_correct) then (print_endline "PASS: linear combinations 3") else (print_endline "FAIL: linear combinations 3")
    )

  let test_lincomb4 = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let b  = b2 - b1 in
      let orig = z*(b + b) in
      let expand = z*b + z*b in
      let err = norm (orig - expand) in
      let is_correct = almost_equal err zero 1e-5 in
      if (is_correct) then (print_endline "PASS: linear combinations 4") else (print_endline "FAIL: linear combinations 4")
    )

  let test_norm1 = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let b  = z*(b2 - b1) in
      let res = norm (b/(norm b)) in
      let is_correct = almost_equal res one 1e-5 in
      if (is_correct) then (print_endline "PASS: norm 1") else (print_endline "FAIL: norm 1")
    )

  let test_norm_homogeneous = 
    Dsl.(
      let z = (re zr)  + (im zi) in
      let b1 = basis 3 in
      let b2 = basis 4 in
      let b  = z*(b2 - b1) in
      let res = norm (z*b) in
      let hom = (abs z) * norm(b) in
      let is_correct = almost_equal (res-hom) zero 1e-5 in
      if (is_correct) then (print_endline "PASS: norm homogeneous") else (print_endline "FAIL: norm homogeneous")
    )


end;;



module MakeOperatorSpace (C : Gencomplex.Sig) (H1 : HilbertSpace with type ct=C.t) (H2 : HilbertSpace with type ct=C.t) : OperatorSpace with type vect1=H1.vect with type vect2=H2.vect with type ct = C.t = struct


  module Orth1 = MakeOrthogonalizable (C) (H1)
  module Orth2 = MakeOrthogonalizable (C) (H2)
  type ct = C.t
  type vect1 = H1.vect
  type vect2 = H2.vect

  let adj (n:int) (a:vect1->vect2) = 
    let bs  = Array.make (n+1) (H1.nullvector) in
    for i = 0 to n do
      bs.(i) <- H1.basis i;
    done;
    let obs = Orth1.orthogonalize bs in
    let avs = Array.map a obs in
    fun u ->
    let l av = H2.innerprod (av) u in
    let ls  = Array.map l avs in
    let cls = Array.map C.conj ls in
    let ccls = Array.map2 H1.scalarmul cls obs in
    let z    = Array.fold_left H1.add H1.nullvector ccls in
    z
    ;;
   


        
  (*let opnorm maxit n a = C.zero*)
  let opnorm maxit n a = 
    let normalize u = H1.scalarmul (C.inv (H1.norm u)) u in
    let m = ref H1.nullvector in
    for i = 0 to n do
      m := H1.add (!m) (H1.basis i)
    done;
    m := normalize !m;
    let adja = adj n a in
    let adja_a u = adja (a u) in
    let b = adja_a in
    for j=0 to maxit do
      m := normalize (b !m)
    done;
    C.sqrt (H1.norm (b !m))



end;;


module MakeSquareOperatorSpace (C : Gencomplex.Sig) (H : HilbertSpace with type ct=C.t) : SquareOperatorSpace with type vect=H.vect with type ct = C.t = struct
  type vect = H.vect
  type ct = C.t
  module Mop : OperatorSpace with type vect1 = vect with type vect2 = vect  with type ct=ct = MakeOperatorSpace (C) (H) (H);;
  module Morth =  MakeOrthogonalizable (C) (H)
  (*Type helpers. Ocaml doesn't quite understand that vect1=vect2*)
  let vec1_from_vec (u : vect) : Mop.vect1 = u
  let vec2_from_vec (u : vect) : Mop.vect2 = u
  let vec_from_vec1 (v : Mop.vect1) : vect = v
  let vec_from_vec2 (v : Mop.vect2) : vect = v
  let sq2rect       (a : vect -> vect) : (Mop.vect1 -> Mop.vect2) = 
    fun u -> vec2_from_vec (a (vec_from_vec1 u))
  let rect2sq       (a : Mop.vect1 -> Mop.vect2) : (vect -> vect) = 
    fun u -> vec_from_vec2 (a (vec1_from_vec u))
  (*End type helpers*)

  let adj n a = rect2sq (Mop.adj n (sq2rect a))
  let opnorm maxit n a = Mop.opnorm maxit n (sq2rect a)
  (*Warning: Not complete!*)
  let qriters maxit n a = 
    let bs   = Array.init n H.basis in
    let obs  = ref (Morth.orthogonalize bs) in
    obs := Array.map a (!obs);
    let (qt,rt) = (Morth.qr !obs) in
    obs := qt;
    (*
    for i = 0 to maxit do
    (*  obs := Array.map a (!obs);
      let (q,r) = (Morth.qr !obs) in
      obs := q;*)
    done;
    *)
    obs := Array.map a (!obs);
    let (q,r) = (Morth.qr !obs) in
    obs := q;
    obs := Array.map a (!obs);
    let (qq,rr) = (Morth.qr !obs) in
    Array.init n (fun i->rr.(i).(i))
end;;



