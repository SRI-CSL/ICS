
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


exception Non_number


(*s Static type. *)

let rec of_term ctxt a =
  let f,l = Term.destruct a in
  match Sym.destruct f with
    | Sym.Uninterp(op,sgn) ->
	of_uninterp ctxt op sgn l
    | Sym.Interp(op) ->
	of_interp ctxt op l

and of_uninterp ctxt op sgn l =
  match Arity.destruct sgn, l with
    | Arity.Constant(c), [] -> c
    | Arity.Functorial(dl,r), _ 
	when List.length l = List.length dl -> r
    | _ -> Type.mk_top

and of_interp ctxt op l =
  match op with
    | Sym.Arith(op) -> of_linarith ctxt op l
    | Sym.Nonlin _ -> Type.mk_real
    | Sym.Enum(e) -> Type.mk_enumerative e.Sym.elems
    | Sym.Bv _ -> Type.mk_bitvector None
    | _ -> Type.mk_top

and of_linarith ctxt op l =
  match op, l with
     | Sym.Num(q), [] -> 
	 Type.mk_number (Number.mk_singleton q)
     | Sym.Multq(q), [x] -> 
	 (match Type.d_number (of_term ctxt x) with
	    | Some(c) -> Type.mk_number (Number.multq q c)
	    | None -> Type.mk_top)
     | Sym.Add, _ ->
	 (try
	    Type.mk_number
	      (List.fold_right
		 (fun x acc ->
		    match Type.d_number (of_term ctxt x) with
		      | Some(c) -> Number.add c acc
		      | None -> raise Non_number)
		 l (Number.mk_singleton Mpa.Q.zero))
	  with
	      Non_number -> Type.mk_top)
     | _ -> assert false


(*s Type from static information only. *)

let of_term0 = 
  Cache.cache 17 
   (fun a -> let empty _ = Type.mk_top in of_term empty a)
  
