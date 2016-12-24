



let () = 
  (*First time the function normally*)
  let start = Unix.gettimeofday () in
  let _ = Infinite.MyOp.opnorm 150 10 Infinite.a in
  let stop = Unix.gettimeofday () in
  (*Now count operator evaluations*)
  let counter = ref 0 in
  let a v = 
    let () = counter := !counter + 1 in
    Infinite.a v in
  let _ = Infinite.MyOp.opnorm 150 10 a in
  Printf.printf "Function modulation by sin,operator norm,150,10,%d,%fs\n%!" !counter (stop -. start)
  


let () = 
  let start = Unix.gettimeofday () in
  let _ = Polynomials.MyOp.opnorm 150 10 Polynomials.diff in
  let stop = Unix.gettimeofday () in
  let counter = ref 0 in
  let a v = 
    let () = counter := !counter + 1 in
    Polynomials.diff v in
  let _ = Polynomials.MyOp.opnorm 150 10 a in
  Printf.printf "polynomial derivative,operator norm,150,10,%d,%fs\n%!" !counter (stop -. start)
