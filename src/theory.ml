
(*s The equality theories. *)

type t = 
  | Uninterp
  | Interp of interp

and interp = A | T | B | E | BV | NLA

let u = Uninterp
let a = Interp(A)
let t = Interp(T)
let e = Interp(E)
let bv = Interp(BV)
let b = Interp(B)
let nla = Interp(NLA)

let of_eqn (x,y) =
  match Term.theory_of x, Term.theory_of y with
    | Sym.A, _ | _, Sym.A -> a
    | Sym.T, _ | _, Sym.T -> t
    | Sym.BV, _ | _, Sym.BV -> bv
    | Sym.E, _ | _, Sym.E -> e
    | Sym.B, _ | _, Sym.B -> b
    | Sym.NLA, _ | _, Sym.NLA -> nla
    | _ -> u

let of_term x =
 match Term.theory_of x with
   | Sym.A -> a
   | Sym.T -> t
   | Sym.BV -> bv
   | Sym.E -> e
   | Sym.B -> b
   | Sym.NLA -> nla
   | Sym.U -> u  

let name_of =
  let u = "u" in
  let a = "a" in
  let b = "b" in
  let bv = "bv" in
  let e = "e" in
  let t = "t" in
  let nla = "nla" in
  let p = "p" in
  function
    | Uninterp -> u
    | Interp(eqth) ->
	(match eqth with
	   | A -> a
	   | T -> t
	   | B -> b
	   | E -> e
	   | BV -> bv
	   | NLA -> nla)

let of_name = function
  | "a" -> a
  | "b" -> b
  | "t" -> t
  | "e" -> e
  | "bv" -> bv
  | "nla" -> nla
  | "u" -> u
  | _ -> raise (Invalid_argument "Unknown equality theory")

