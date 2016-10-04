open Hilbertspace;;

module FloatComplex : ComplexNumbers with type ret=float with type imt=float  = 
  struct
    type ret = float
    type imt = float
    type t   = { re : ret; im : imt;}

    let re c = c.re
    let im c = c.im

    let conj c = {re=c.re; im=(0.0 -. c.im);}
    let (<*>) c1 c2 = {re=c1.re;im=c1.im}
    let (<+>) c1 c2 = {re=c1.re;im=c1.im}

    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}



  end
;;




let () = print_endline "test"
