

let () = 
  let start = Unix.gettimeofday () in
  let _ = Infinite.MyOp.opnorm 150 10 Infinite.a in
  let stop = Unix.gettimeofday () in
  Printf.printf "Function modulation by sin,operator norm,150,10,%fs\n%!" (stop -. start)
  


let () = 
  let start = Unix.gettimeofday () in
  let _ = Polynomials.MyOp.opnorm 150 10 Polynomials.diff in
  let stop = Unix.gettimeofday () in
  let () = Printf.printf "polynomial derivative,operator norm,150,10,%fs\n%!" (stop -. start) in
  Printf.printf "Derivative evaluations = %d\n" !(Polynomials.count_diff)
