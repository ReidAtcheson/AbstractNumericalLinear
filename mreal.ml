open Hilbertspace;;


(*A fake imaginary number type*)
type fkim = | NoImag

module Real : ComplexNumber with type t=float with type ret=float with type imt=fkim  = 
  struct
    type ret = float
    type imt = fkim
    type t   = float

    let mk re im = re

    let re c = c
    let im c = NoImag

    let conj c = c

    let mul = ( *. )
    let add  = ( +. )

    let inv c  = 1.0 /. c
    let neg c  = 0.0 -. c
    let sqrt   = sqrt
    let abs    = abs_float 


    let almost_equal a b (tol:float) = (re (abs (add (neg b) a))) < tol
  

    let zero   = 0.0
    let one    = 1.0

    let to_string  = string_of_float

    let real r = mk r 0.0
    let imag i = mk 0.0 i



  end
;;

