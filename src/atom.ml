
(*i*)
open Hashcons
open Term
open Mpa
open Poly
open Format
(*i*)

(*s Canonizing Equalities *)

let rec eq (a,b) =
  if a == b then
     Bool.tt
  else if is_const a && is_const b then
    Bool.ff
  else
    match a.node,b.node with
      | Bool(Ite(a1,a2,a3)), _ ->
	  Bool.ite a1 (eq (a2,b)) (eq (a3,b))
      | _, Bool(Ite(b1,b2,b3)) ->
	  Bool.ite b1 (eq (a,b2)) (eq (a,b3))
      | _ ->
          hc (Atom(Equal(a,b)))
 

(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let deq (a,b) = Bool.neg (eq (a,b))
 
let is_deq a =
  match a.node with
    | Bool (Ite({node=Atom(Equal _)},
		    {node=Bool False},
		    {node=Bool True})) -> true
    | _ -> false

let destructure_deq a =
  match a.node with
    | Bool (Ite({node=Atom(Equal (x,y))},
		{node=Bool False},
		{node=Bool True})) -> Some(x,y)
    | _ -> None


(* Inequalities *)

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


					      
(*s Constructor for integer constraint *)

let rec is_integer t =
  match t.node with
    | Var _ when Var.is_integer t -> true
    | Arith a ->
	(match a with
	   | Num q -> Q.is_integer q
	   | Times l -> List.for_all is_integer l
	   | Plus l -> List.for_all is_integer l)
    | _ -> false

let int t =
  if is_integer t then Bool.tt
  else if Term.is_const t then
    match t.node with
      | Arith (Num q) when Q.is_integer q -> Bool.tt
      | _ -> Bool.ff
  else
    hc (Atom(Integer(t)))
	
(* Solving *)

let solve ((a,b) as e) =
  match a.node, b.node with
    | Atom(Equal(x,y)), Bool(True) ->
	[x,y]       
    | Atom(Le(x1,x2)), Bool(True) ->
	let c = Var.create ("zzz",Some Var.Nonneg) in   (* x1 <= x2  --> x1 = x2-c, c >= 0 *)
	[x1,Arith.sub (x2,c)]
    | Atom(Le(x1,x2)), Bool(False) ->          (* x1 > x2 --> x1 = x2 + c, c > 0 *)
	let c = Var.create ("zzz",Some Var.Pos) in
	[x1, Arith.add2 (x2,c)]
    | Atom(Lt(x1,x2)), Bool(True) ->           (* x1 < x2  --> x1 = x2-c, c > 0 *)
	let c = Var.create ("zzz",Some Var.Pos) in
	[x1,Arith.sub (x2,c)]
    | Atom(Lt(x1,x2)), Bool(False) ->          (* x1 >= x2 --> x1 = x2 + c, c >= 0 *)
	let c = Var.create ("zzz",Some Var.Nonneg) in
	[x1, Arith.add2 (x2,c)]
    | _ ->
	[e]
