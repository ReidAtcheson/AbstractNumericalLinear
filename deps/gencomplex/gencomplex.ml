module type Sig = sig  
  (*Type for real part*)
  type ret
  (*Type for imaginary part*)
  type imt
  (*Type for complex number*)
  type t
  (*Get precision as floating point number*)
  val precision : float
  (*Make a complex number out of real and imaginary parts*)
  val mk : ret -> imt -> t
  (*Real part of complex number*)
  val real : t -> ret
  (*Imaginary part of complex number*)
  val imag : t -> imt
  (*Complex conjugate*)
  val conj  : t -> t
  (*Multiplication*)
  val  mul  : t -> t -> t
  (*Addition*)
  val  add  : t -> t -> t
  (*Multiplicative inverse*)
  val  inv  : t -> t
  (*Additive inverse*)
  val  neg  : t -> t
  (*Square root*)
  val sqrt : t -> t
  (*Complex absolute value*)
  val  abs  : t -> t
  (*Additive identity*)
  val zero : t
  (*Multiplicative identity*)
  val one  : t
  val almost_equal : t -> t -> float -> bool


  (*Convenience functions *)
  (* Convert complex number to string*)
  val to_string : t -> string
  (*Specify complex number approximately with floats*)
  val re : float -> t
  val im : float -> t
end;;

module DSL (C : Sig) = struct
  let mk = C.mk
  let real = C.real
  let imag = C.imag
  let conj = C.conj
  let ( * ) = C.mul
  let ( + ) = C.add
  let ( / ) x y = C.mul (x) (C.inv y)
  let ( - ) x y = C.add (x) (C.neg y)
  let sqrt = C.sqrt
  let abs = C.abs
  let zero = C.zero
  let one = C.one
  let almost_equal = C.almost_equal
  let re = C.re
  let im = C.im
end;;


module PropCheck (C : Sig) = struct

  module D = DSL (C)

  let () = Random.init 2341047
  let ntests = 100
  let tol = 1000.0 *. C.precision
  let rf () = 
    let x = (Random.float (10.0)) -. (Random.float (10.0)) in
    x


  let check_almost_equals = 
    for i = 1 to ntests do
      D.(
        let eps = re (0.01*.tol) in
        let bad_eps = re (10000.0 *. tol)  in
        let x = re (rf ()) + im ( rf()) in
        let correct1  = almost_equal x (x+eps) tol in
        let correct2  = not (almost_equal x (x+bad_eps) tol) in
        let correct3  = almost_equal x x tol in
        let tmp = eps in
        if (correct1 && correct2 && correct3) then () else (Printf.printf "val = %s\n" (C.to_string tmp))
      )
    done

  let check_abs_conj = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let cx = conj x in
        let absx = abs x in
        let myabsx = sqrt (cx*x) in
        let correct  = almost_equal absx myabsx tol in
        if (correct) then () else print_endline "check_abs_conj: Failed"
      )
    done


  let check_div = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let myone = x / x in
        let correct  = almost_equal myone one tol in
        if (correct) then () else print_endline "check_div: Failed"
      )
    done

  let check_sub = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let myzero = x - x in
        let correct  = almost_equal myzero zero tol in
        if (correct) then () else print_endline "check_mult: Failed"
      )
    done



  let mult_ident = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let lres = x*one in
        let rres = one*x in
        let lcorrect  = almost_equal x lres tol in
        let rcorrect  = almost_equal x rres tol in
        if (rcorrect && lcorrect) then () else print_endline "mult_ident: Failed"
      )
    done


  let add_ident = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let lres = x+zero in
        let rres = zero+x in
        let lcorrect  = almost_equal x lres tol in
        let rcorrect  = almost_equal x rres tol in
        if (rcorrect && lcorrect) then () else print_endline "add_ident: Failed"
      )
    done

  let mult_commute = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let y = re (rf ()) + im ( rf()) in
        let xy = x*y in
        let yx = y*x in
        let correct  = almost_equal xy yx tol in
        if correct then () else print_endline "mult_commute: Failed"
      )
    done

  let add_commute = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let y = re (rf ()) + im ( rf()) in
        let xy = x+y in
        let yx = y+x in
        let correct  = almost_equal xy yx tol in
        if correct then () else print_endline "add_commute: Failed"
      )
    done


  let mult_assoc = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let y = re (rf ()) + im ( rf()) in
        let z = re (rf ()) + im ( rf()) in
        let x_yz = x*(y*z) in
        let xy_z = (x*y)*z in
        let correct  = almost_equal x_yz xy_z tol in
        if correct then () else print_endline "mult_assoc: Failed"
      )
    done

  let add_assoc = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let y = re (rf ()) + im ( rf()) in
        let z = re (rf ()) + im ( rf()) in
        let x_yz = x+(y+z) in
        let xy_z = (x+y)+z in
        let correct  = almost_equal x_yz xy_z tol in
        if correct then () else print_endline "add_assoc: Failed"
      )
    done


  let distrib = 
    for i = 1 to ntests do
      D.(
        let x = re (rf ()) + im ( rf()) in
        let y = re (rf ()) + im ( rf()) in
        let z = re (rf ()) + im ( rf()) in
        let xyz = z*x+z*y in
        let z_xy = z*(x+y) in
        let correct  = almost_equal xyz z_xy tol in
        if correct then () else print_endline "distrib: Failed"
      )
    done



    (*This test we have to be careful in case the incoming
     * field is actually just Real. Simple check:
       mk 5 5 == 5 --> is real
       * *)
  let sqrt_works = 
    for i = 1 to ntests do
      D.(
        let testr = 5.0 in
        let testi = 5.0 in
        if (almost_equal ((re testr)+(im testi)) (re testr) tol) 
        then
          let z = re (rf ()) + im ( rf()) in
          let x = z*z in
          let sqrtx = sqrt x in
          let y = sqrtx*sqrtx in
          let correct  = almost_equal x y tol in
          if (correct) then () else print_endline "sqrt_works (Real case): Failed"
        else
          let x = re (rf ()) + im ( rf()) in
          let sqrtx = sqrt x in
          let y = sqrtx*sqrtx in
          let correct  = almost_equal x y tol in
          if (correct) then () else print_endline "sqrt_works (Complex case): Failed"
      )
    done

end;;
