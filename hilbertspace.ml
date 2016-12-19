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
  (*Complex absolute value*)
  val  abs  : t -> t
  (*Additive identity*)
  val zero : t
  (*Multiplicative identity*)
  val one  : t
  val almost_equal : t -> t -> float -> bool

  (* "to_string" convenience function*)
  val to_string : t -> string
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
  val to_string  : vect -> string
end;;

module HilbertSpaceDSL (C : ComplexNumber) (H : HilbertSpace with type ct=C.t) = struct

  type t = Vect of H.vect | Num of H.ct

  let vect_to_t x = Vect x
  let vect_from_t x = match x with
    Vect v -> v
  | Num  c -> raise (Failure "Expected a vector")



  let nullvector = Vect (H.nullvector)
  let basis i = Vect (H.basis i)
  let innerprod x y = match x,y with
   Vect u,Vect   v -> Num (H.innerprod u v)
  | Vect u, Num   c -> raise (Failure "Inner product only supports vectors")
  | Num  c, Vect  u -> raise (Failure "Inner product only supports vectors")
  | Num  c, Num   d -> raise (Failure "Inner product only supports vectors")

  let norm x = match x with
    Vect u -> Num (H.norm u)
    |Num  c -> raise (Failure "Vector norm attempted of number")

  let to_string x = match x with
     Vect u -> H.to_string u
    |Num  c -> C.to_string c




  let ( * ) x y = match x,y with
    Vect u,Vect   v -> raise (Failure "Tried to multiply two vectors")
  | Vect u, Num   c -> Vect (H.scalarmul c u)
  | Num  c, Vect  u -> Vect (H.scalarmul c u)
  | Num  c, Num   d -> Num  (C.mul c d)

  let ( + ) x y = match x,y with
    Vect u,Vect   v -> Vect (H.add u v)
  | Vect u, Num   c -> raise (Failure "Tried to add number to vector")
  | Num  c, Vect  u -> raise (Failure "Tried to add number to vector")
  | Num  c, Num   d -> Num (C.add c d)


  let ( - ) x y = match x,y with
    Vect u,Vect   v -> Vect (H.add u (H.scalarmul (C.neg C.one) v))
  | Vect u, Num   c -> raise (Failure "Tried to add number to vector")
  | Num  c, Vect  u -> raise (Failure "Tried to add number to vector")
  | Num  c, Num   d -> Num (C.add c (C.mul (C.neg C.one) d))


  let ( / ) x y = match x,y with
    Vect u,Vect   v -> raise (Failure "Attempted to divide one vector into another vector")
  | Vect u, Num   c -> Vect (H.scalarmul (C.inv c) u)
  | Num  c, Vect  u -> raise (Failure "Attempted to divide number by vector")
  | Num  c, Num   d -> Num (C.mul (C.inv d) c)








end;;

  (*
module HilbertSpaceDSL (H : HilbertSpace) = struct
  type t = Vect of H.vect | Num of H.ct 
  (*Null vector*)
  let nullvector : t = Vect H.nullvector
  (*Basis functions. basis i = i-th basis function*)
  let basis i = Vect (H.basis i)
  (*Scalar multiplication: constant times a vector*) 
  let ( * ) x y = match x with
    Vect u -> (match y with
               Vect v -> Vect v
              | Num  a -> (H.mul a u) )
  | Num a  -> (match y with
               Vect v -> (H.mul a v)
              | Num a -> Num a )
  (*Vector addition*)
  (*val       add  : t -> t -> t*)
  let (+) x y = match x with
    Vect u -> (match y with
              Vect v -> (H.add u v))

  (*Inner product*)
  val innerprod  : t -> t -> t
  (*Vector Norm*)
  val norm       : t -> t
  (*Convenience printing function*)
  val to_string  : t -> string

end;;
  *)
 


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

  module Dsl = HilbertSpaceDSL (C) (H);;

  type ct=C.t
  type vect=H.vect
  let orthogonalize2 x_ y_ = 
    Dsl.(
    let x = vect_to_t x_ in 
    let y = vect_to_t y_ in 
    let ip = innerprod in
    let proj u v = ((ip u v) / (ip u u))*u in
    let normalize u = u / (norm u) in
    let nx = normalize x in
    let ey = y - (proj x y) in
    let ny = normalize ey in
    List.map vect_from_t [nx;ny]
    )


  let orthogonalize xs_ = 
    let m = Array.length xs_ in
    let n = m-1 in
    let min1 k = k-1 in
    Dsl.(
    let xs = Array.map vect_to_t xs_ in
    let m = Array.length xs in
    let qs=xs in
    for k = 0 to n do
      let w = ref xs.(k) in
      for j = 0 to (min1 k) do
        let rjk = innerprod (!w) qs.(j) in
        w := (!w) - rjk*qs.(j)
      done;
      qs.(k) <- !w / (norm !w)
    done;
    (Array.map vect_from_t qs)
    )


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




