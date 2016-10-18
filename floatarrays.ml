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
      

    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}

    let show z = (string_of_float (re z)) ^ " + " ^ (string_of_float (im z)) ^ "i"



  end
;;

module ArrayHilbert : HilbertSpace with type vect=FloatComplex.t array with type ct = FloatComplex.t = struct
  let m=5

  type vect = FloatComplex.t array
  type ct   = FloatComplex.t
  let nullvector = Array.make m (FloatComplex.mk 0.0 0.0)
  let basis i    = 
    let b = Array.make m (FloatComplex.mk 0.0 0.0) in
    for j = 0 to i do
      b.(j)<-(FloatComplex.mk 1.0 0.0);
    done;
    b
  let scalarmul  x y = 
    let mulx z = FloatComplex.mul x z in
    Array.map mulx y
  ;;
  let add x y = Array.map2 (FloatComplex.add) x y

  let innerprod  x y = 
    let xy = Array.map2 (FloatComplex.mul) x y in
    Array.fold_left (FloatComplex.add) (FloatComplex.zero) xy
  ;;

  let norm x = FloatComplex.sqrt (innerprod x x)

  let show x = 
    let catarr x y = x ^ "," ^ y in
    let str = Array.fold_left catarr ("") (Array.map FloatComplex.show x) in
    "[" ^ str ^ "]"


end;;


module MyOrth = MakeOrthogonalizable (FloatComplex) (ArrayHilbert)
module MyOp   = MakeOperatorSpace    (FloatComplex) (ArrayHilbert) (ArrayHilbert)




let x1 = Array.of_list [FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x2 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x3 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x4 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0]
let x5 = Array.of_list [FloatComplex.mk 5.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0]

let xs = MyOrth.orthogonalize (Array.of_list [x1;x2;x3;x4;x5])
let ys = (Array.of_list [x1;x2;x3;x4;x5])

let a u = 
  let v = ref (Array.make 5 (FloatComplex.mk 0.0 0.0)) in
  for i = 0 to 4 do
    v := ArrayHilbert.add !v (ArrayHilbert.scalarmul u.(i) ys.(i));
  done;
  !v
;;


let e i = 
  let m = Array.make 5 (FloatComplex.mk 0.0 0.0) in
  m.(i) <- FloatComplex.mk 1.0 0.0;
  m

let adja1 = MyOp.adj 4 a (e 4)
let a1    = a (e 4)



let na = MyOp.opnorm 10 4 a

let () = print_endline (FloatComplex.show na)
