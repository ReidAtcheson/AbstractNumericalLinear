
(*A fake imaginary number type*)
type fkim = | NoImag

module Real : Gencomplex.Sig with type t=float with type ret=float with type imt=fkim  = 
  struct
    type ret = float
    type imt = fkim
    type t   = float

    let precision = 1e-10

    let mk re im = re

    let real c = c
    let imag c = NoImag

    let conj c = c

    let mul = ( *. )
    let add  = ( +. )

    let inv c  = 1.0 /. c
    let neg c  = 0.0 -. c
    let sqrt   = sqrt
    let abs    = abs_float 


    let almost_equal a b (tol:float) = (real (abs (add (neg b) a))) < tol
  

    let zero   = 0.0
    let one    = 1.0

    let to_string  = string_of_float

    let re r = mk r 0.0
    let im i = mk 0.0 i



  end
;;

