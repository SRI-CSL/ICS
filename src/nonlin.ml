
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
open Tools
open Hashcons
open Mpa
open Term
open Sym
(*i*)


(*s Theory-specific recognizers *)

let is_interp a =
  Sym.is_nonlin (Term.sym_of a)


(*s Destructors. *)

let d_mult a =
  if Sym.eq Sym.mk_mult (Term.sym_of a) then
    Some(Term.args_of a)
  else 
    None

let d_expt a =
  let f,l = Term.destruct a in
  match Sym.destruct f, l with
    | Interp(Nonlin(Expt(n))), [x] -> Some(n,x)
    | _ -> None

(*s Apply [f] at uninterpreted positions. *)

let rec iter f a =
  match d_mult a with
    | Some(l) -> 
	List.iter (iter f) l
    | None -> 
	(match d_expt a with
	   | Some(_,x) -> iter f x
	   | None -> f a)

(*s Fold functional. *)

let rec fold f a e =
  match d_mult a with
    | Some(l) -> 
	List.fold_right (fold f) l e
    | None -> 
	(match d_expt a with
	   | Some(_,x) -> fold f x e
	   | None -> f a e)


(*s [occurs a b] iff [a] occurs interpreted in [b].. *)

let rec occurs a b =    
  (Term.eq a b) ||
  (match d_mult b with
     | Some(l) -> List.exists (occurs a) l
     | None -> 
	 (match d_expt b with
	    | Some(_,x) -> occurs a x
	    | None -> false))


(* [normal_of a] returns a normalized representation of a nonlinear
 multiplication; e.g. [x^3 * y * z^2] is represented as [(3,x),(1,y),(2,z)] *)

let expt_of a =
  match d_expt a with
    | Some(n,x) -> (n,x)
    | None -> (1,a)


(*s Constructors. *)

let rec mk_mult (a,b) =
  if Term.eq a b then mk_expt 2 a else
  match d_expt a, d_expt b with
    | Some(n,x), Some(m,y) when Term.eq x y ->
	mk_expt (n + m) x
    | Some(n,x), _ when Term.eq x b ->
	mk_expt (n + 1) x
    | _, Some(m,y) when Term.eq y a ->
	mk_expt (m + 1) y
    | _ ->
	(match Linarith.d_interp a with
	   | Some(f,xl) ->
	       mk_linmult f xl b
	   | None ->
	       (match Linarith.d_interp b with
		  | Some(g,yl) ->
		      mk_linmult g yl a
		  | None ->
		      (match d_mult a, d_mult b with
			 | Some(xl), Some(yl) ->
			     of_list (multl xl yl)
			 | Some(xl), None ->
			     of_list (multl xl [b])
			 | None, Some(yl) ->
			     of_list (multl yl [a])
			 | _ ->
			     let (a,b) = Term.order a b in
			     Term.mk_app Sym.mk_mult [a;b])))

and mk_multl al = 
  match al with
    | [] -> Linarith.mk_num Q.one
    | [x] -> x
    | x :: xl -> mk_mult (x, mk_multl xl)

and mk_linmult f al b =
  match f, al with
    | Sym.Num(q),[] ->
	Linarith.mk_multq q b
    | Sym.Multq(q), [x] ->
	Linarith.mk_multq q (mk_mult (x,b))
    | Sym.Add, xl ->
	let yl = List.map (fun x -> mk_mult (b,x)) xl in
	Linarith.mk_addl yl
    | _ ->
	failwith "Ill-formed expression"

and multl l1 l2 =             
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' ->
	let n1,x1 = expt_of m1 in
	let n2,x2 = expt_of m2 in
	let cmp = Term.cmp x1 x2 in
	if cmp = 0 then
	  let n = n1 + n2 in
	  if n = 0 then 
	    multl l1' l2'
	  else 
	    (mk_expt n x1) :: (multl l1' l2')
	else if cmp < 0 then
	  m1 :: multl l1' l2
	else (* cmp > 0 *)
	  m2 :: multl l1 l2'

and of_list l =
  match l with
    | [] -> Linarith.mk_one
    | [x] -> x
    | _ -> Term.mk_app Sym.mk_mult l

and mk_expt n a =
  if n = 0 then
    Linarith.mk_one
  else if n = 1 then
    a
  else 
    (match Linarith.d_interp a with
       | Some(f,xl) ->
	   (match f, xl with
	     | Sym.Num(q),[] ->
		 Linarith.mk_num (Mpa.Q.expt q n)
	     | Sym.Multq(q), [x] ->
		 Linarith.mk_multq (Mpa.Q.expt q n) (mk_expt n x)
	     | Sym.Add, [x;y] ->
		 mk_mult (mk_expt (n - 2) a,
			  Linarith.mk_addl [mk_expt 2 x;
					    Linarith.mk_multq (Mpa.Q.of_int 2) (mk_mult (x,y));
					    mk_expt 2 y])		
	     | _ ->
		 Term.mk_app (Sym.mk_expt(n)) [a])
       | None ->
	   (match d_expt a with
	      | Some(m, x) when Term.eq x a ->
		  mk_expt (n + m) a
	      | _ ->
		  (match d_mult a with
		     | Some(xl) ->
			 of_list (List.map (mk_expt n) xl)
		     | None ->
			 Term.mk_app (Sym.mk_expt(n)) [a])))


(*s Apply term transformer [f] at uninterpreted positions. *)

let rec norm f =
  let rec loop a =
    match d_mult a with
      | Some([x;y]) -> 
	  mk_mult (loop x, loop y)
      | Some(l) -> 
	  mk_multl (Term.mapl loop l)
      | None ->
	  (match d_expt a with
	     | Some(n,x) ->
		 mk_expt n (loop x)
	     | None ->
		 f a)
  in 
  loop


(*s Interface for sigmatizing arithmetic terms. *)

let rec sigma op l =
  match op, l with
    | Mult, [x; y] -> mk_mult (x,y)
    | Mult, _::_::_ -> mk_multl l
    | Expt(n), [x] -> mk_expt n x
    | _ -> failwith("Nonlin.sigma: ill-formed argument")

(*s Fresh variables *)

let k = ref 0 
let _ = Tools.add_at_reset (fun () -> k := 0)

let rename () =
  incr(k);
  Term.mk_const (Sym.make(Sym.Internal(Sym.FreshNla(!k, Number.mk_real))))

(*s Solving of nonlinear equations. *)

let rec solve (a,b) =
  match is_interp a, is_interp b with
    | true, true ->                    (* case: [x^2*y = x*y^2] *)
	let x = rename () in
	let y = rename () in
	[(order x y); (x,a); (y,b)]
    | false, true ->                   (* case: [x^2*y = z] *)      
        if occurs a b then
	  let x = rename () in
	  [(order x a); (x, b)]
	else 
	  [a,b]
    | true, false ->                          (* case: [x^2*y = z] *)
	if occurs b a then
	  let x = rename () in
	  [(order x b); (x, a)]
	else
	  [b,a]
    | false, false ->
	[order a b]
