module type ComplexNumber = sig  
  type ret
  type imt
  type t

  val mk : ret -> imt -> t

  val re : t -> ret
  val im : t -> imt

  val conj  : t -> t
  val  mul  : t -> t -> t
  val  add  : t -> t -> t
  val  inv  : t -> t
  val  neg  : t -> t

  val sqrt : t -> t


  val zero : t
  val one  : t


  val show : t -> string

end;;



module type HilbertSpace = sig
  type vect
  type ct 
  val nullvector : vect
  val basis      : vect list
  val scalarmul  : ct -> vect -> vect
  val       add  : vect -> vect -> vect
  val innerprod  : vect -> vect -> ct
  val norm       : vect -> ct
  val show       : vect -> string
end;;


module type Orthogonalizable = sig
  type ct
  type vect
  val orthogonalize2 : vect -> vect -> vect list
  val orthogonalize  : vect list -> vect list
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


  let orthogonalize xs = xs
end;;
