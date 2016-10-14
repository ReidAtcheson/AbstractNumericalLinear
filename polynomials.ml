
open Hilbertspace;;

module FloatComplex : ComplexNumber with type ret=float with type imt=float  = 
  struct
    type ret = float
    type imt = float
    type t   = { re : ret; im : imt;}

    let mk re im = {re=re;im=im;}

    let re c = c.re
    let im c = c.im

    let conj c = {re=c.re; im=(0.0 -. c.im);}

    let mul c1 c2 = 
      let a=c1.re in
      let b=c1.im in
      let c=c2.re in
      let d=c2.im in    
      {re=a*.c-.b*.d;im=a*.d+.b*.c}
    let add c1 c2 = {re=c1.re +. c2.re;im=c1.im +. c2.im}

    let inv c = 
      let a=c.re in
      let b=c.im in
      {re=a/.(a*.a+.b*.b);im=0.0 -. b /. (a*.a +. b*.b)}
    let neg c = 
      let a=c.re in
      let b=c.im in
      {re=0.0-.a;im=0.0-.b}

    let sqrt z = 
      let rez = z.re in
      let imz = z.im in
      let zz  = {Complex.re=rez;im=imz} in
      let sqrtz = Complex.sqrt zz in
      mk (sqrtz.Complex.re) (sqrtz.Complex.im)


    let zero   = {re=0.0;im=0.0}
    let one    = {re=1.0;im=0.0}

    let show z = (string_of_float (re z)) ^ " + " ^ (string_of_float (im z)) ^ "i"



  end
;;

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
    Val z           -> "(" ^ (FloatComplex.show z) ^ ")"
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
  let basis = [Val FloatComplex.one;Var]
  let scalarmul s p  = Mul (Val s,p)
  let add x y = Add (x,y)
  let innerprod p1 p2 = integrate a b (Mul (p1,conj p2))
  let norm p = FloatComplex.sqrt (innerprod p p)

  let show p = string_of_poly p
end;;


module MyOrth = MakeOrthogonalizable (FloatComplex) (PolynomialHilbert)
  



let mk z        = Val z
let x           = Var
let (<+>) p1 p2 = Add (p1,p2)
let (<*>) p1 p2 = Mul (p1,p2)



let orth = MyOrth.orthogonalize2 x (x <*> x)

let () = print_endline (PolynomialHilbert.show (List.nth orth 0))
let () = print_endline (PolynomialHilbert.show (List.nth orth 1))

