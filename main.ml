open Hilbertspace;;

module FloatComplex : ComplexNumber with type ret=float with type imt=float  = 
  struct
    type ret = float
    type imt = float
    type t   = { re : ret; im : imt;}

    let re c = c.re
    let im c = c.im

    let conj c = {re=c.re; im=(0.0 -. c.im);}

    let (<*>) c1 c2 = 
      let a=c1.re in
      let b=c1.im in
      let c=c2.re in
      let d=c2.im in    
      {re=a*.c-.b*.d;im=a*.d+.b*.c}
    let (<+>) c1 c2 = {re=c1.re +. c2.re;im=c1.im +. c2.im}

    let inv c = 
      let a=c.re in
      let b=c.im in
      {re=a/.(a*.a+.b*.b);im=0.0 -. b /. (a*.a +. b*.b)}

    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}



  end
;;




let () = print_endline "test"
