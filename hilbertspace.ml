module type ComplexNumbers = sig  
  type ret
  type imt
  type t

  val re : t -> ret
  val im : t -> imt

  val conj  : t -> t
  val (<*>) : t -> t -> t
  val (<+>) : t -> t -> t


  val zero : t
  val one  : t




end;;


module type HilbertSpace = sig
  type a
end;;
