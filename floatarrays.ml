open Hilbertspace;;
open Mcomplex;;


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
    let conjy = Array.map (FloatComplex.conj) y in
    let xy = Array.map2 (FloatComplex.mul) x (conjy) in
    Array.fold_left (FloatComplex.add) (FloatComplex.zero) xy
  ;;

  let norm x = FloatComplex.sqrt (innerprod x x)

  let to_string x = 
    let catarr x y = x ^ "," ^ y in
    let str = Array.fold_left catarr ("") (Array.map FloatComplex.to_string x) in
    "[" ^ str ^ "]"


end;;


module MyOrth = MakeOrthogonalizable (FloatComplex) (ArrayHilbert)
module MyOp   = MakeOperatorSpace    (FloatComplex) (ArrayHilbert) (ArrayHilbert)
module FloatArrayTest = TestHilbertSpace (FloatComplex) (ArrayHilbert)




let x1 = Array.of_list [FloatComplex.mk 5.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x2 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x3 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x4 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0]
let x5 = Array.of_list [FloatComplex.mk 5.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0]

