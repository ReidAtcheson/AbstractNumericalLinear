
let () = print_endline "Testing floating point based complex numbers"
let () = print_endline "_______________________________________________________________"
module TestFloatComplex = Gencomplex.PropCheck(Floatcomplex.FloatComplex);;
let () = print_endline "_______________________________________________________________"
let () = print_newline ()
let () = print_newline ()
let () = print_newline ()



let () = print_endline "Testing floating point based real numbers"
let () = print_endline "_______________________________________________________________"
module TestFloatReal = Gencomplex.PropCheck(Floatreal.Real);;
let () = print_endline "_______________________________________________________________"
let () = print_newline ()
let () = print_newline ()




let () = print_endline "Testing rational numbers based complex numbers"
let () = print_endline "_______________________________________________________________"
module TestRationalComplex = Gencomplex.PropCheck(Rationalcomplex.RationalComplex);;
let () = print_endline "_______________________________________________________________"
let () = print_newline ()
let () = print_newline ()
