
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
 i*)

(*i*)
open Sym
open Term
(*i*)

let is_interp a =
  Term.is_app a &&
  match Sym.destruct (Term.sym_of a) with
    | Sym.Interp(Sym.Tuple _) -> true
    | _ -> false

let d_interp a =
  if Term.is_var a then
    None
  else 
    match Sym.destruct (Term.sym_of a) with
      | Interp(Tuple(f)) -> Some(f, Term.args_of a)
      | _ -> None

let d_tuple a =
  if Term.is_var a then None else
    match destruct a with
      | f, xl when Sym.eq f Sym.mk_tuple -> Some(xl)
      | _ -> None

let d_proj a =
  if Term.is_var a then None else 
    let f, l = Term.destruct a in
    match Sym.destruct f, l with
      | Interp(Tuple(Proj(i,n))), [x] -> Some(i,n,x)
      | _ -> None


(*s Apply [f] at uninterpreted positions. *)

let rec iter f a = 
  match d_interp a with
    | Some(Product, l) -> List.iter (iter f) l
    | Some(Proj _, [x]) -> f x
    | _ -> f a

(*s Fold iterator  *)

let rec fold f a e = 
  match d_interp a with
    | Some(Product, l) -> List.fold_right (fold f) l e
    | Some(Proj _, [x]) -> fold f x e
    | _ -> f a e
 
(*s Smart constructors for tuples and projections. *)

let mk_tuple = function
  | [x] -> x
  | l -> Term.mk_app Sym.mk_tuple l

let rec mk_proj i n a =
  match d_tuple a with
    | Some(xl) -> List.nth xl i
    | None -> Term.mk_app (Sym.mk_proj i n) [a]


(*s Apply term transformer [f] at uninterpreted positions. *)

let rec map f a =
  match d_interp a with
    | Some(Sym.Product, l) -> mk_tuple (Term.mapl (map f) l)
    | Some(Sym.Proj(i,n), [y]) -> mk_proj i n (map f y)
    | _ -> f a

(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | Product, _ -> mk_tuple l
    | Proj(i,n), [x] -> mk_proj i n x
    | _ -> assert false

(*s Fresh variables. *)

let freshvars = ref Term.Set.empty
let _ =  Tools.add_at_reset (fun () -> freshvars := Term.Set.empty)

let mk_fresh () = 
  let x = Term.mk_fresh (Name.of_string "t") in
  freshvars := Term.Set.add x !freshvars;
  x
  
let is_fresh x =
  Term.Set.mem x !freshvars

(*s Solving tuples. *) 

let rec solve e =
  solvel [e] []

and solvel el sl =
  match el with
    | [] -> sl
    | (a,b) :: el1 ->
	if Term.eq a b then 
	  solvel el1 sl
	else 
	  match d_proj a with
	    | Some(i,n,x) ->
		solvel (proj_solve i n x b :: el1) sl
	    | None ->
		(match d_proj b with
		   | Some(j,n,y) ->
		       solvel (proj_solve j n y a :: el1) sl
		   | None ->
		       (match d_tuple a, d_tuple b with
			  | Some(al), Some(bl) ->
			      solvel (tuple_tuple_solve al bl @ el1) sl
			  | Some(al), None ->
			      if is_consistent b al then
				solvel el1 (add (b,a) sl)
			      else 
				raise Exc.Inconsistent
			  | None, Some(bl) ->
			      if is_consistent a bl then
				solvel el1 (add (a,b) sl)
			      else
				raise Exc.Inconsistent
			  | None, None ->
			      let sl1 = add (Term.orient(a,b)) sl in
			      solvel el1 sl1))

and is_consistent a bl =
  not(List.exists (fun y -> Term.eq a y) bl)

(*s [solve ((s0,...,sn), (t0,...,tn)) = [(s0,t0),...(sn,tn)] *)  

and tuple_tuple_solve al bl = 
   List.fold_right2 (fun a b acc -> add (a, b) acc) al bl []

(*s [solve (proj i n s, t) = (s, \list{c0,...,t,...cn-1})]
     where [ci] are fresh, [s] at [i]-th position. *)

and proj_solve i n s t =
  let rec args j acc =
    if j = -1 then acc
    else
      let a =
	if i = j
	then t
	else
	  mk_fresh () (* equals [mk_proj j n s] *)
      in
      args (j - 1) (a :: acc)
  in
  (s, mk_tuple (args (n - 1) []))

and add (a,b) el =
  if Term.eq a b then 
    el
  else
    let el' = List.map (fun (x,y) -> (x, subst1 y a b)) el in 
    (a,b) :: el'

and subst1 a x b =      (* substitute [x] by [b] in [a]. *)
  map (fun y -> if Term.eq x y then b else y) a
