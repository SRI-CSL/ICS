
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
      | Bool(Ite(x,y,z)), _ ->
	  Bool.ite x (eq (y,b)) (eq (z,b))
      | _, Bool(Ite(x,y,z)) ->
	  Bool.ite x (eq (a,y)) (eq (a,z))
      | (Cnstrnt _ | Equal _), (Cnstrnt _ | Equal _) ->
	  Bool.iff a b
      | _ ->
          hc (Equal(a,b))

(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let deq (a,b) = Bool.neg (eq (a,b))
 
let is_deq a =
  match a.node with
    | Bool (Ite({node=Equal _},
		    {node=Bool False},
		    {node=Bool True})) -> true
    | _ -> false

let destructure_deq a =
  match a.node with
    | Bool (Ite({node=Equal (x,y)},
		{node=Bool False},
		{node=Bool True})) -> Some(x,y)
    | _ -> None


let cnstrnt (c,t) =
  match t.node with
    | Arith(Times({node = Arith (Num q)} :: l)) when Q.equal q (Q.minus Q.one) ->
	(match c with
	   | Pos -> hc (Cnstrnt (Neg, Arith.mult l))
	   | Neg -> hc (Cnstrnt (Pos, Arith.mult l))
	   | Nonneg -> hc (Cnstrnt (Nonpos, Arith.mult l))
	   | Nonpos -> hc (Cnstrnt (Nonneg, Arith.mult l))
	   | _ -> hc (Cnstrnt(c,t)))
    | _ ->
	hc (Cnstrnt(c,t))
					      
(*s Constructor for integer constraint *)

let rec is_integer t =
  match t.node with
    | Arith a ->
	(match a with
	   | Num q -> Q.is_integer q
	   | Times l -> List.for_all is_integer l
	   | Plus l -> List.for_all is_integer l)
    | _ -> false

let int t =
  match t.node with
    | Bv _ | Set _ | Tuple _ | Set _ | Equal _ | Cnstrnt _ ->
	Bool.ff
    | _ -> 
	if is_integer t then Bool.tt
	else if Term.is_const t then
	  match t.node with
	    | Arith (Num q) when Q.is_integer q -> Bool.tt
	    | _ -> Bool.ff
	else
	  cnstrnt (Int, t)

let real t =
  match t.node with
    | Bv _ | Set _ | Tuple _ | Set _ | Equal _ | Cnstrnt _ ->
	Bool.ff
    | _ ->
	cnstrnt (Real, t)

let is_atom t =
  match t.node with
    | Cnstrnt _
    | Equal _ -> true
    | _ -> false
      
(* Inequalities *)

let le (x,y) = 
  if x == y then
    Bool.tt
  else
    let p = Arith.sub (y,x) in
    cnstrnt (Nonneg, p)

      
let lt (x,y) =
  if is_integer x && is_integer y then
    le (Arith.incr x, y)
  else
    let p = Arith.sub (y,x) in
    cnstrnt (Pos, p)

let pos x = lt (Arith.num Q.zero, x)
let neg x = lt (x,Arith.num Q.zero)
	      
let nonneg x = le (Arith.num Q.zero, x)
let nonpos x = le (x, Arith.num Q.zero)
		 
(* Solving *)

let solve ((a,b) as e) =
  match a.node, b.node with
    | Equal(x,y), Bool(True) ->
	[x,y]
    | Cnstrnt(_, {node=Var _}), Bool(True) ->
	[e]
    | Cnstrnt(Pos,x), Bool(True) ->
	let k = Var.create "z" in
	[x, k; pos k, Bool.tt]
    | Cnstrnt(Neg,x), Bool(True) ->
	let k = Var.create "z" in
	[x, k; neg k, Bool.tt]
    | Cnstrnt(Nonneg,x), Bool(True) ->
	let k = Var.create "z" in
	[x, k; nonneg k, Bool.tt]
    | Cnstrnt(Nonpos,x), Bool(True) ->
	let k = Var.create "z" in
	[x, k; nonpos k, Bool.tt]
    | _ ->
	[e]



