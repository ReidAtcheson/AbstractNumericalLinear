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
    let xy = Array.map2 (FloatComplex.mul) x y in
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




let x1 = Array.of_list [FloatComplex.mk 5.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x2 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x3 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0]
let x4 = Array.of_list [FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0;FloatComplex.mk 0.0 0.0]
let x5 = Array.of_list [FloatComplex.mk 5.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 0.0 0.0;FloatComplex.mk 1.0 0.0]

let xs = MyOrth.orthogonalize (Array.of_list [x1;x2;x3;x4;x5])
let ys = (Array.of_list [x1;x2;x3;x4;x5])
(*
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
let (qs, r) = MyOrth.qr ys
*)

let () =
  print_endline (ArrayHilbert.to_string x1);
  print_endline (ArrayHilbert.to_string x2);
  print_endline (ArrayHilbert.to_string x3);
  print_endline (ArrayHilbert.to_string x4);
  print_endline (ArrayHilbert.to_string x5);
  print_newline ();
  print_newline ();
  let zs      = (Array.of_list [x1;x2;x3;x4;x5]) in
  let (qs,r)  = MyOrth.qr zs in
  for i = 0 to 4 do
    print_endline (ArrayHilbert.to_string zs.(i))
  done;
(*  for i = 0 to 4 do
    print_endline (ArrayHilbert.to_string ys.(i))
  done;
  *)
(*  for i = 0 to 4 do
    for j = 0 to 4 do
      print_string (FloatComplex.to_string r.(i).(j));
      print_string ", "
    done;
    print_endline ""
    
  done;
    *)
