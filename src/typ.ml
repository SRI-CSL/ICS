
(*i*)
open Term
open State
open Hashcons
open Mpa
(*i*)

(* Type lattice:
     Real < T, Nonint < T,
     Int < Real, NonintReal < Real, NonintReal < Nonint, NonReal < Nonint
     F < Int,  F < NonintReal, F < Nonreal
 *)

type typ = F | Int | Real | Nonint | Nonreal | NonintReal | T

  
(*s Sign interpretation of arithmetic terms. *)

    
let inf (s1,s2) =
  if s1 = s2 then
    s1
  else
    match s1,s2 with
      | T, _ -> s2
      | _, T -> s1
      | Int, Real -> Int
      | Real, Int -> Int
      | Nonint, Nonreal -> Nonreal
      | Nonreal, Nonint -> Nonreal
      | (Nonint | Real), NonintReal -> NonintReal
      | NonintReal, (Nonint | Real) -> NonintReal
      | _ -> F
  
let ( ** ) s1 s2 =
  match s1,s2 with
    | F, _    -> F
    | _, F    -> F
    | Int, Int -> Int
    | Int, (Real | NonintReal) -> Real
    | Real, (Real | Int | NonintReal) -> Real
    | NonintReal, (Int | Real | NonintReal) -> Real
    | _ -> T
  
let typ s t =
  let rec typ_of_term t =
    match t.node with
      | Arith a ->
	  inf (typ_from_state t,
	       match a with
		 | Num q ->
		     if Q.is_integer q then Int else NonintReal
		 | Times l -> typ_of_list l
		 | Plus l -> typ_of_list l)
      | Bv _ | Tuple _ | Bool _ | Set _ ->
	  Nonreal
      | _ ->
	  typ_from_state t
	    
  and typ_of_list l =
    match l with
      | [] -> Int
      | [x] -> typ_of_term x
      | x :: l -> typ_of_term x ** typ_of_list l
	    
  and typ_from_state t =
    let f t' acc =
      if find s t' == Bool.tt then 
	match t'.node with
	  | Cnstrnt(c,x) ->
	      let y = find s x in
	      if t == y then 
		match c with Term.Int -> Int | _ -> Real
	      else
		acc
	  | _ -> acc
      else
	acc
    in
    Tset.fold f (use s t) T
  in
  typ_of_term t

let inconsistent s (t1,t2) =
  inf (typ s t1, typ s t2) = F


let infer s (t1,t2) =
  let s1 = typ s t1 in
  let s2 = typ s t2 in
  if s1 = s2 then
    []
  else
    match s1, s2 with
      | T, _ | _, T ->
	  []
      | Int, Real ->
	  [Atom.int t2, Bool.tt]
      | Real, Int ->
	  [Atom.int t1, Bool.tt]
      | Nonint, Nonreal ->
	  []
      | Nonreal, Nonint ->
	  []
      | (Nonint | Real), NonintReal ->
	  []
      | NonintReal, (Nonint | Real) ->
	  []
      | _ ->
	  begin
	    Format.printf "\nInconsistent: "; Pretty.eqn (t1,t2);
	    raise (Exc.Inconsistent "typ")
	  end





