open Num
module RationalComplex : Gencomplex.Sig  = struct



  let weak_num_of_float f = 
    let is_float_integer fl =
      float_of_int((int_of_float fl)) = fl
    in
    let rec sub_num_of_float fl beg =
      if is_float_integer fl then
        num_of_int (int_of_float fl) // num_of_int beg
      else
        sub_num_of_float (fl*.10.0) (beg*10)
    in
    sub_num_of_float f 1


  let num_of_float fl = 
    let (x,n) = frexp fl in
    let bign  = num_of_int n in
    let two   = num_of_int 2 in
    let exp_two_n = power_num two bign in
    let bigx  = weak_num_of_float x in
    exp_two_n */ bigx


    type ret = num
    type imt = num
    type t   = { re : ret; im : imt;}
    let precision = 1e-25
    let mk re im = {re=re;im=im;}
    let real c = c.re
    let imag c = c.im
    let conj c = {re=c.re; im=(minus_num c.im);}
    let mul c1 c2 = 
      let a=c1.re in
      let b=c1.im in
      let c=c2.re in
      let d=c2.im in    
      {re=a*/c-/b*/d;im=a*/d+/b*/c}
    let add c1 c2 = {re=c1.re +/ c2.re;im=c1.im +/ c2.im}
    let inv c = 
      let a=c.re in
      let b=c.im in
      {re=a//(a*/a+/b*/b);im=minus_num b // (a*/a +/ b*/b)}
    let neg c = 
      let a=c.re in
      let b=c.im in
      {re=minus_num a;im=minus_num b}

    let sqrt_re_fl rez = 
      let rezfl      = float_of_num rez in
      let sqrt_guess = num_of_float (sqrt rezfl) in
      if  eq_num rez (num_of_int 0)
      then
        sqrt_guess
      else
        let halfrez    = rez // (num_of_int 2) in
        let threehalf  = (num_of_int 3) // (num_of_int 2) in
        let inv_sqrt   = ref ((num_of_int 1) // (sqrt_guess)) in
        inv_sqrt       := (!inv_sqrt) */ (threehalf -/ halfrez */ !inv_sqrt */ !inv_sqrt );
        (num_of_int 1) // (!inv_sqrt)


    let sqrt_c z = 
      let a = z.re in
      let b = z.im in
      let s = sqrt_re_fl (real (mul (conj z) z)) in
      if (eq_num b (num_of_int 0)) 
      then
        if (lt_num a (num_of_int 0))
        then
          mk (num_of_int 0) (sqrt_re_fl s)
        else
          mk (sqrt_re_fl s) (num_of_int 0)
      else
        let ur = sqrt_re_fl ((s +/ a) // (num_of_int 2)) in
        let ui = (num_of_int (sign_num b)) */ sqrt_re_fl ( (s -/ a) // (num_of_int 2)) in
        mk ur ui

    let sqrt = sqrt_c
    let abs c = 
          sqrt (mul (conj c) c)


    let almost_equal a b (tol:float) =  (float_of_num (real  (abs  (add (neg b)  a)))) < tol
    let zero   = {re=num_of_int 0;im=num_of_int 0}
    let one    = {re=num_of_int 1;im=num_of_int 0}
    let to_string z = (string_of_num (real z)) ^ " + " ^ (string_of_num (imag z)) ^ "i"

    let re r = mk (num_of_float r) (num_of_int 0)
    let im i = mk (num_of_int 0) (num_of_float i)



end;;
