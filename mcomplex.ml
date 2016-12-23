
open Hilbertspace;;

module FloatComplex : ComplexNumber with type ret=float with type imt=float  = 
  struct
    type ret = float
    type imt = float
    type t   = { re : ret; im : imt;}

    let mk re im = {re=re;im=im;}

    let re c = c.re
    let im c = c.im

    let conj c = {re=c.re; im=(0.0 -. c.im);}

    let mul c1 c2 = 
      let a=c1.re in
      let b=c1.im in
      let c=c2.re in
      let d=c2.im in    
      {re=a*.c-.b*.d;im=a*.d+.b*.c}
    let add c1 c2 = {re=c1.re +. c2.re;im=c1.im +. c2.im}

    let inv c = 
      let a=c.re in
      let b=c.im in
      {re=a/.(a*.a+.b*.b);im=0.0 -. b /. (a*.a +. b*.b)}
    let neg c = 
      let a=c.re in
      let b=c.im in
      {re=0.0-.a;im=0.0-.b}

    let sqrt z = 
      let rez = z.re in
      let imz = z.im in
      let zz  = {Complex.re=rez;im=imz} in
      let sqrtz = Complex.sqrt zz in
      mk (sqrtz.Complex.re) (sqrtz.Complex.im)
    let abs c = 
      sqrt (mul (conj c) c)


    let almost_equal a b (tol:float) = 
      let  e = compare (re (abs (add (neg b) a))) tol in          
      e<=0
    ;;
      

    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}

    let to_string z = (string_of_float (re z)) ^ " + " ^ (string_of_float (im z)) ^ "i"

    let real r = mk r 0.0
    let imag i = mk 0.0 i



  end
;;

