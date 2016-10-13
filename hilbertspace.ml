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
  val inv   : t -> t


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
end;;


module type Orthogonalizable = sig
  type ct
  type vect
  val orthogonalize : vect list -> vect list
  val inprd2sum : vect -> vect -> ct
end;;


module MakeOrthogonalizable (C : ComplexNumber) (H : HilbertSpace with type ct=C.t) : Orthogonalizable with type vect=H.vect with type ct = C.t= struct
  type ct=C.t
  type vect=H.vect
  let orthogonalize xs = xs
  let inprd2sum x y = C.add (H.innerprod x y) (H.innerprod x y)
end;;
