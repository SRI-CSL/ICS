
(*i*)
open Hashcons
open Mpa
open Term
open Poly
open Format
(*i*)

let le (x,y) = 
  if x == y then
    Bool.tt
  else
    let p = Arith.sub (y,x) in
    if Arith.is_nonneg p then
      Bool.tt
    else if Arith.is_neg p then 
      Bool.ff
  else
    Term.hc (Atom(Le(x,y)))

let lt (x,y) =
  if Arith.is_integer x && Arith.is_integer y then
    le (Arith.incr x, y)
  else
    let p = Arith.sub (y,x) in
    if Arith.is_pos p then
      Bool.tt
    else if Arith.is_neg p then
      Bool.ff
    else
      Term.hc (Atom(Lt(x,y)))

let solve ((x,y) as e) =
  match x.node, y.node with                (* x1 <= x2  --> x1 = x2-c, c >= 0 *)
    | Atom(Le(x1,x2)), Bool(True) ->
	let c = Var.new_var "zzz" Nonneg in
	[x1,Arith.sub (x2,c)]
    | Atom(Le(x1,x2)), Bool(False) ->          (* x1 > x2 --> x1 = x2 + c, c > 0 *)
	let c = Var.new_var "zzz" Pos in
	[x1, Arith.add2 (x2,c)]
    | Atom(Lt(x1,x2)), Bool(True) ->           (* x1 < x2  --> x1 = x2-c, c > 0 *)
	let c = Var.new_var "zzz" Pos in
	[x1,Arith.sub (x2,c)]
    | Atom(Lt(x1,x2)), Bool(False) ->          (* x1 >= x2 --> x1 = x2 + c, c >= 0 *)
	let c = Var.new_var "zzz" Nonneg in
	[x1, Arith.add2 (x2,c)]
    | _ ->
	[e]
					      
  
