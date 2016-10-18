module type ComplexNumber = sig  
  (*Type for real part*)
  type ret
  (*Type for imaginary part*)
  type imt
  (*Type for complex number*)
  type t
  (*Make a complex number out of real and imaginary parts*)
  val mk : ret -> imt -> t
  (*Real part of complex number*)
  val re : t -> ret
  (*Imaginary part of complex number*)
  val im : t -> imt
  (*Complex conjugate*)
  val conj  : t -> t
  (*Multiplication*)
  val  mul  : t -> t -> t
  (*Addition*)
  val  add  : t -> t -> t
  (*Multiplicative inverse*)
  val  inv  : t -> t
  (*Additive inverse*)
  val  neg  : t -> t
  (*Square root*)
  val sqrt : t -> t
  (*Additive identity*)
  val zero : t
  (*Multiplicative identity*)
  val one  : t

  (* "to_string" convenience function*)
  val show : t -> string
end;;



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
  val show       : vect -> string
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
  (*Compute adjoint of linear operator*)
end;;


module type Operator = sig
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


module MakeOrthogonalizable (C : ComplexNumber) (H : HilbertSpace with type ct=C.t) : Orthogonalizable with type vect=H.vect with type ct = C.t= struct
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


end;;





module MakeOperatorSpace (C : ComplexNumber) (H1 : HilbertSpace with type ct=C.t) (H2 : HilbertSpace with type ct=C.t) : Operator with type vect1=H1.vect with type vect2=H2.vect with type ct = C.t = struct


  module Orth1 = MakeOrthogonalizable (C) (H1)
  module Orth2 = MakeOrthogonalizable (C) (H2)
  type ct = C.t
  type vect1 = H1.vect
  type vect2 = H2.vect

  let adj (n:int) (a:vect1->vect2) (u:vect2) = 
    let bs  = Array.make (n+1) (H1.nullvector) in
    for i = 0 to n do
      bs.(i) <- H1.basis i;
    done;
    let obs = Orth1.orthogonalize bs in
    let l v = H2.innerprod (a v) u in
    let ls  = Array.map l obs in
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




