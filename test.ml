open Hilbertspace;;

let () = print_endline "Testing Function Space Properties"
module Mreal = Floatreal;;
module Mcomplex = Floatcomplex;;
module TestFunction = TestHilbertSpace (Mreal.Real) (Infinite.FunctionHilbert)


let () = Infinite.(Mreal.(
  (*Compute operator norm of linear operator Af = sin*f*)
  let opnorm = MyOp.opnorm 150 8 a in
  let correct = Real.almost_equal opnorm (0.83) (0.1) in
  if correct then (print_endline "Function space operator norm correct (PASS) ") else (print_endline "Function space operator norm incorrect (FAIL)")
))

(*
let () = Infinite.(Mreal.(
  (*Compute operator norm of a compact *)
  let opnorm  = MyOp.opnorm 150 8 k in
  (*let eigvals = MySqOp.qriters 5 5 k in*)
  print_endline (Real.to_string opnorm)
  (*
  let correct = Real.almost_equal opnorm (0.83) (0.1) in
  if correct then (print_endline "Function space operator norm correct (PASS) ") else (print_endline "Function space operator norm incorrect (FAIL)")
  *)
))
*)


let () = print_endline "________________________________"
let () = print_newline ()
let () = print_newline ()
let () = print_endline "Testing Polynomial Space Properties"





module TestPolynomial = TestHilbertSpace (Mcomplex.FloatComplex) (Polynomials.PolynomialHilbert);;


(*
let bs  = Array.init 5 (Polynomials.PolynomialHilbert.basis)
let obs = Polynomials.MyOrth.orthogonalize bs
let ()  = Array.iter (fun x -> print_endline (Polynomials.PolynomialHilbert.to_string x)) obs
let () = Polynomials.( Mcomplex.(    
  for i = 1 to 10 do
    let ndiffrt   = MyOp.opnorm 100 i diff in
    let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
    print_endline (FloatComplex.to_string ndiff)
  done
))
*)


let () = Polynomials.( Mcomplex.(    
    let i = 1 in 
    let ndiffrt   = MyOp.opnorm 100 i diff in
    let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
    let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 3.0 0.0) (1e-5) in
    if correct then (print_endline "Derivative at i=1 correct (PASS)") else (print_endline "Derivative at i=1 incorrect (FAIL)");
))

  let () = 
    Polynomials.(Mcomplex.(
    let i = 2 in 
    let ndiffrt   = MyOp.opnorm 100 i diff in
    let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
    let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 15.0 0.0) (1e-5) in
    if correct then (print_endline "Derivative at i=2 correct (PASS)") else (print_endline "Derivative at i=2 incorrect (FAIL)")
))

  let () = 
    Polynomials.(Mcomplex.(
    let i = 3 in 
    let ndiffrt   = MyOp.opnorm 100 i diff in
    let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
    let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 42.53122 0.0) (1e-5) in
    if correct then (print_endline "Derivative at i=3 correct (PASS)") else (print_endline "Derivative at i=3 incorrect (FAIL)")
))

  let () = 
    Polynomials.(Mcomplex.(
    let i = 4 in 
    let ndiffrt   = MyOp.opnorm 100 i diff in
    let ndiff     = FloatComplex.mul (ndiffrt) (ndiffrt) in
    let correct = FloatComplex.almost_equal ndiff (FloatComplex.mk 95.058782 0.0) (1e-5) in
    if correct then (print_endline "Derivative at i=4 correct (PASS)") else (print_endline "Derivative at i=4 incorrect (FAIL)")
))




let () = print_endline "________________________________"
let () = print_newline ()
let () = print_newline ()
let () = print_endline "Testing Float Array Space Properties"
module TestFloatArray = TestHilbertSpace (Mcomplex.FloatComplex) (Floatarrays.ArrayHilbert)
let () = print_endline "________________________________"


