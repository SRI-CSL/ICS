
(*i*)
open Term
open Hashcons
(*i*)

(*s Power products. *)

type t = Term.t list

let is_pproduct a =
  match a.node with
    | Var _ -> true
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Mult, _::_::_ -> true
	   | Div, [_;_] -> true
	   | _ -> false)
    | _ -> false

let of_term a =
  if is_mult a then d_mult a else [a]

let to_term b =
  mk_mult b

let partition b =             (* in head and tail. *)
  assert(List.length b > 0);
  let x = List.hd b in
  match List.partition (fun y -> y === x) b with
    | _, [] -> None
    | b1, b2 -> Some(b1,b2)

let rec remove x b =
  match b with
    | [] -> b
    | a :: al -> if x === a then al else a :: remove x al

let rec inter (b1,b2) =
  match b1 with
    | [] -> []
    | a :: l -> 
	if List.memq a b2 then 
	  a :: inter (l, remove a b2)
	else
	  inter (l,b2)

let rec diff (b1,b2) =
  match b2 with
    | [] -> b1
    | a :: l -> 
	if List.memq a b1 then 
	  diff (remove a b1, l)
	else 
	  diff (b1,l)

let divides (b1,b2) =
  diff (b2,b1) = []

let cancel (b1,b2) =
  let common = inter (b1,b2) in
  (diff (b1,common), diff (b2,common))


let cp (b1,b2) =
  let rec loop acc1 acc2 l1 l2 =
    match l1, l2 with
      | [], [] -> (acc1, acc2)
      | [], _ -> (l2 @ acc1, acc2)
      | _, [] -> (acc1, l1 @ acc2)
      | x :: xl, y :: yl ->
	  if x === y then
	    loop acc1 acc2 xl yl
	  else
	    loop (y :: acc1) (x :: acc2) xl yl
  in
  match b1, b2 with
    | [_], [_] -> None                (* two linear power products. *)
    | _::_, _::_ ->  Some(loop [] [] b1 b2)
    | _ -> assert false


let mult (b1,b2) =                      (* Multiply two power products. *)
  Sort.merge (<<<) b1 b2
 
