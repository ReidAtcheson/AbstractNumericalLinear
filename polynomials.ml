
open Hilbertspace;;
open Mcomplex;;



let rec pow n x = 
  match n with
    0 -> FloatComplex.mk 1.0 0.0
  | _ -> FloatComplex.mul x (pow (n-1) x)

type poly  = 
    Val of FloatComplex.t
  | Var
  | Add of (poly*poly)
  | Mul of (poly*poly)
;;

(*Note: taken from http://stackoverflow.com/a/10893700/412345*)
let cartesian l l' = 
    List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let rec conj p = 
  match p with
     Val z -> Val (FloatComplex.conj z)
  |  Var   -> Var
  |  Add (p1,p2) -> Add (conj p1,conj p2)
  |  Mul (p1,p2) -> Mul (conj p1,conj p2)


let prod (p1,p2) = Mul (p1,p2)
let product p1 p2 = List.map prod (cartesian p1 p2)

let rec simp p = 
  match p with
    Var         -> [Var]
  | Val z       -> [Val z]
  | Add (p1,p2) -> (simp p1) @ (simp p2)
  | Mul (p1,p2) -> product (simp p1) (simp p2)


let rec degconst p = 
  match p with
   Val z  ->  (z,0)
  | Var   ->  (FloatComplex.mk 1.0 0.0,1)
  | Mul (p1,p2) -> 
      let (v1,n1) = degconst p1 in
      let (v2,n2) = degconst p2 in
      (FloatComplex.mul v1 v2,n1+n2)                
  | _           -> raise (Failure "Should only contain Val Var or Mul")



let deg p = 
  match p with
  (v,n) -> n


let maxdeg ps = List.fold_left max 0 (List.map deg ps)

let normal p = 
  let ps   = simp p in
  let dgs  = List.map degconst ps in
  let n    = (maxdeg dgs)+1 in
  let arr  = Array.make n (FloatComplex.mk 0.0 0.0) in
  let inc x = 
    match x with
    (v,n) -> (arr.(n) <- FloatComplex.add arr.(n) v) in
  List.iter inc dgs;
  arr

let normal2expr arr = 
  let rec basis i = if i==0 then (Val (FloatComplex.mk 1.0 0.0)) else (Mul (basis (i-1), Var)) in
  let ps = Array.mapi (fun i x -> Mul (Val x,(basis i))) arr in
  Array.fold_left (fun x y -> Add (x,y)) (Val (FloatComplex.mk 0.0 0.0)) ps


let filterpoly p = normal2expr (normal p)

let integrate a b p = 
  let pir = normal p in
  let aa  = FloatComplex.mk a 0.0 in
  let bb = FloatComplex.mk b 0.0 in
  let tmp = ref (FloatComplex.mk 0.0 0.0) in
  let f i x = 
    let r = FloatComplex.mul (pow (i+1) bb) (FloatComplex.inv (FloatComplex.mk (float_of_int (i+1)) 0.0)) in
    let l = FloatComplex.mul (pow (i+1) aa) (FloatComplex.inv (FloatComplex.mk (float_of_int (i+1)) 0.0)) in
    let rml = FloatComplex.add r (FloatComplex.mul (FloatComplex.mk (0.0-.1.0) 0.0) l) in
    tmp := FloatComplex.add (!tmp) (FloatComplex.mul x rml) in
  Array.iteri f pir;
  !tmp

let rec string_of_poly p = 
  match p with
    Val z           -> "(" ^ (FloatComplex.to_string z) ^ ")"
  | Var             -> "x"
  | Add (p1,p2)     -> "(" ^ (string_of_poly p1) ^ "+" ^(string_of_poly p2) ^ ")"
  | Mul (p1,p2)     -> "(" ^ (string_of_poly p1) ^ "*" ^(string_of_poly p2) ^ ")"
let rec string_of_poly_list ps =  List.fold_left (fun p1 p2 -> p1 ^" + "^ p2) "" (List.map string_of_poly ps)



  
module PolynomialHilbert : HilbertSpace with type vect=poly with type ct = FloatComplex.t = struct
  let a=0.0-.1.0
  let b= 1.0
  type vect = poly
  type ct   = FloatComplex.t
  let nullvector = Val (FloatComplex.mk 0.0 0.0)
  let rec basis i = if i==0 then (Val (FloatComplex.mk 1.0 0.0)) else (Mul (basis (i-1), Var))
  let scalarmul s p  = Mul (Val s,p)
  let add x y = filterpoly (Add (x,y))
  let innerprod p1 p2 = integrate a b (Mul (p1,conj p2))
  let norm p = FloatComplex.sqrt (innerprod p p)

  let to_string p = string_of_poly p
end;;


module MyOp   = MakeOperatorSpace (FloatComplex) (PolynomialHilbert) (PolynomialHilbert)
module MyOrth = MakeOrthogonalizable (FloatComplex) (PolynomialHilbert)
module TestPolynomial = TestHilbertSpace (FloatComplex) (PolynomialHilbert)
  


let rec diff p = 
  match p with
    Val z       -> Val (FloatComplex.mk 0.0 0.0)
  | Var         -> Val (FloatComplex.mk 1.0 0.0)
  | Add (p1,p2) -> Add ((diff p1),(diff p2))
  | Mul (p1,p2) -> Add ((Mul ((diff p1),p2)),(Mul (p1,(diff p2))))

let mk z        = Val z
let x           = Var
let (<+>) p1 p2 = Add (p1,p2)
let (<*>) p1 p2 = Mul (p1,p2)





let adjdiff = MyOp.adj 6 diff
(*let orth = MyOrth.orthogonalize (Array.of_list [mk (FloatComplex.mk 1.0 0.0); x;(x <*> x)])*)

let sadj = normal (adjdiff x<*>x)

let ip1  = PolynomialHilbert.innerprod (diff (x<*>x<*>x<+>x)) (x<*>x<+>x)
let ip2  = PolynomialHilbert.innerprod ((x<*>x<*>x<+>x)) (adjdiff (x<*>x<+>x))




let () = 
  print_endline "TESTING OPERATOR NORMS"
;;

let start = Unix.gettimeofday ()

let () = 
  let i = 1 in 
  let ndiffrt   = MyOp.opnorm 100 i diff in
  let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
  let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 3.0 0.0) (1e-5) in
  if correct then (print_endline "Derivative at i=1 correct (PASS)") else (print_endline "Derivative at i=1 incorrect (FAIL)")
;;

let () = 
  let i = 2 in 
  let ndiffrt   = MyOp.opnorm 100 i diff in
  let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
  let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 15.0 0.0) (1e-5) in
  if correct then (print_endline "Derivative at i=2 correct (PASS)") else (print_endline "Derivative at i=2 incorrect (FAIL)")
;;

let () = 
  let i = 3 in 
  let ndiffrt   = MyOp.opnorm 100 i diff in
  let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
  let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 42.53122 0.0) (1e-5) in
  if correct then (print_endline "Derivative at i=3 correct (PASS)") else (print_endline "Derivative at i=3 incorrect (FAIL)")
;;

let () = 
  let i = 4 in 
  let ndiffrt   = MyOp.opnorm 100 i diff in
  let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
  let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 95.058782 0.0) (1e-5) in
  if correct then (print_endline "Derivative at i=4 correct (PASS)") else (print_endline "Derivative at i=4 incorrect (FAIL)");
;;

let stop = Unix.gettimeofday ()
let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)


