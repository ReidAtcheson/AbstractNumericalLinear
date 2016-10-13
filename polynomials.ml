
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

    (*Note: very wrong, but only really needed for positive reals.*)
    let sqrt z = mk (sqrt (re z)) 0.0

    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}

    let show z = (string_of_float (re z)) ^ " + " ^ (string_of_float (im z)) ^ "i"



  end
;;


type poly  = 
    Val of FloatComplex.t
  | Var of poly 
  | Add of (poly*poly)
  | Mul of (poly*poly)
;;


let rec simp p = 
  match p with
    Val z      -> Val z
  | Var        -> Var
  | Add (Var,Val z) -> Add (Var,Val z)
  | Add (Val z,Var) -> Add (Val z,Var)
  | Add (Var,Var)   -> Mul (Val (FloatComplex.mk 2.0 0.0),Var)
  | Add (Var,Mul (Val z,Var))   -> Mul (Val (FloatComplex.add z (FloatComplex.mk 1.0 0.0)),Var)
  | Add (Mul (Val z,Var),Var)   -> Mul (Val (FloatComplex.add z (FloatComplex.mk 1.0 0.0)),Var)



module PolynomialHilbert : Hilbertspace with type vect=poly with type ct = FloatComplex.t = struct
  type vect = poly
  type ct   = FloatComplex.t
  let nullvector = Val (FloatComplex.mk 0.0 0.0)
  let basis = [Val FloatComplex.one;Var]
  let scalarmul s p = Mul (Val s,p)
  let add x y = Add (x,y)
end;;






let () = print_endline "yes"
