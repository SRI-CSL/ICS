
(*i*)
open Term
open Hashcons
(*i*)

(*s Canonizing Equalities *)

let rec equal a b =
  if eq_term a b then
     Term.ptrue ()
  else if is_const a && is_const b then
    Term.pfalse ()
  else
    match a.node,b.node with
      | Bool(Ite(a1,a2,a3)), _ ->
	  ite a1 (equal a2 b) (equal a3 b)
      | _, Bool(Ite(b1,b2,b3)) ->
	  ite b1 (equal a b2) (equal a b3)
      | _ ->
	  Term.equal a b

(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let diseq a b = Bool.neg (equal a b)
 
let is_disequality a =
  match a.node with
    | Bool (Ite({node=Equal _},
		    {node=Bool False},
		    {node=Bool True})) -> true
    | _ -> false

let destructure_disequality a =
  match a.node with
    | Bool (Ite({node=Equal (x,y)},
		{node=Bool False},
		{node=Bool True})) -> Some(x,y)
    | _ -> None
 
	
(* Solving *)

let solve e = [e]


