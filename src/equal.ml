
(*i*)
open Term
open Hashcons
(*i*)

(*s Canonizing Equalities *)

let rec equal a b =
  if a == b then
     Bool.tt
  else if is_const a && is_const b then
    Bool.ff
  else
    match a.node,b.node with
      | Bool(Ite(a1,a2,a3)), _ ->
	  Bool.ite a1 (equal a2 b) (equal a3 b)
      | _, Bool(Ite(b1,b2,b3)) ->
	  Bool.ite b1 (equal a b2) (equal a b3)
      | _ ->
          hc (Atom(Equal(a,b)))
 

(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let diseq a b = Bool.neg (equal a b)
 
let is_disequality a =
  match a.node with
    | Bool (Ite({node=Atom(Equal _)},
		    {node=Bool False},
		    {node=Bool True})) -> true
    | _ -> false

let destructure_disequality a =
  match a.node with
    | Bool (Ite({node=Atom(Equal (x,y))},
		{node=Bool False},
		{node=Bool True})) -> Some(x,y)
    | _ -> None
 
	
(* Solving *)

let solve ((a,b) as e) =
  match a.node, b.node with
    | Atom(Equal(x,y)), Bool(True) ->
	[x,y]
    | _ ->
	[e]


