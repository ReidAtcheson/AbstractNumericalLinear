

module type Gausskronrodquad = sig
  val gkint : (float -> float) -> float -> float -> float -> float
end;;

module GkTest( GK : Gausskronrodquad ) = struct
  Random.init 12389;
  let tol = 1e-6 in
  for i = 1 to 200 do
    let s = (Random.float 200.0) -. (Random.float 200.0) in
    let a = 0.0 -. Random.float 5.0 in
    let b = Random.float 5.0 in
    let f x = sin (s *. x) in
    let intf = GK.gkint f a b tol in    
    let aintf x = 0.0 -. (cos (s *.x )) /. s in
    let sintf = (aintf b) -. (aintf a) in
    if ((abs_float (sintf -. intf)) > tol) then (print_endline "Failed") else (print_endline "Passed")
  done;
end;;

module Quad : Gausskronrodquad = struct

let gknodes = Array.of_list [-0.991455371120813;-0.949107912342759;-0.864864423359769;-0.741531185599394;-0.586087235467691;-0.405845151377397;-0.207784955007898;0.0;0.207784955007898;0.405845151377397;0.586087235467691;0.741531185599394;0.864864423359769;0.949107912342759;0.991455371120813]

let gnodes = Array.of_list [-0.949107912342759;-0.741531185599394;-0.405845151377397;0.0;0.405845151377397;0.741531185599394;0.949107912342759]

let kweights = Array.of_list [0.022935322010529;0.063092092629979;0.104790010322250;0.140653259715525;0.169004726639267;0.190350578064785;0.204432940075298;0.209482141084728;0.204432940075298;0.190350578064785;0.169004726639267;0.140653259715525;0.104790010322250;0.063092092629979;0.022935322010529]

let gweights = Array.of_list [0.129484966168870;0.279705391489277;0.381830050505119;0.417959183673469;0.381830050505119;0.279705391489277;0.129484966168870]


let map_interval a b r s x = (r *. (b -. x) /. (b -. a)) +. (s *. (x -. a) /. (b -. a))

let rec gkint f a b tol = 
  let y  = (b -. a) *. 0.5 in
  let g  = fun x -> y *. (f (map_interval (-1.0) (1.0) a b x)) in
  let ggs = Array.map g gnodes in
  let ggws = gweights in
  let gks = Array.map g gknodes in
  let gkws = kweights in
  let gquad = Array.fold_left (+.) 0.0 (Array.map2 ( *. ) ggs ggws) in
  let kquad = Array.fold_left (+.) 0.0 (Array.map2 ( *. ) gks gkws) in
  let err = 200.0 *. ( (abs_float (gquad -. kquad)) ** (1.5) ) in
  let a1 = a in
  let b1 = (a +. b) *. 0.5 in
  let a2 = b1 in
  let b2 = b in
  if (err < tol) then kquad else ((gkint f a1 b1 tol) +. (gkint f a2 b2 tol))
end;;



