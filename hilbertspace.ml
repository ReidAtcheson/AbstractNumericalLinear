module type ComplexNumber = sig  
  type ret
  type imt
  type t

  val re : t -> ret
  val im : t -> imt

  val conj  : t -> t
  val (<*>) : t -> t -> t
  val (<+>) : t -> t -> t
  val inv   : t -> t


  val zero : t
  val one  : t

end;;





module type HilbertSpace = sig
  type vect
  type ct

  val nullvector : vect
  val scalarprod : ct -> vect -> vect
  val vectoradd  : vect -> vect -> vect
  val innerprod  : vect -> vect -> ct

end;;



module MakeHilbertSpace(C : ComplexNumber) : HilbertSpace


