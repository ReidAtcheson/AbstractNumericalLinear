open Hilbertspace;;
open Mreal;;
open Gk;;


module FunctionHilbert : HilbertSpace with type vect = (float -> float) with type ct = Real.t = struct
  let a = -1.0
  let b = 1.0
  let tol = 1e-6
  let rec poly i x = if i==0 then (1.0) else (Real.mul x (poly (i-1) x))

  type vect = (float -> float)
  type ct = Real.t
  let nullvector = fun x -> 0.0
  let basis i = poly i
  let scalarmul s f = fun x -> Real.mul s (f x)
  let add f1 f2 = fun x -> Real.add (f1 x)  (f2 x)
  let innerprod f1 f2 = 
    let f = fun x -> Real.mul (f1 x)  (f2 x) in
    Quad.gkint f a b tol
  let norm p = (Real.sqrt (innerprod p p))
  let to_string p = "opaque"
end;;

module MyOp   = MakeOperatorSpace (Real) (FunctionHilbert) (FunctionHilbert)
module MyOrth = MakeOrthogonalizable (Real) (FunctionHilbert)
module TestFunction = TestHilbertSpace (Real) (FunctionHilbert)


(*Compute operator norm of linear operator Af = sin*f*)
let a f = fun x -> (sin x) *. (f x)
let opnorm = MyOp.opnorm 130 10 a
let () = print_endline (Real.to_string opnorm)
