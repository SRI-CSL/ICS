(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** A {i forward chain} is of the form [hyp1,...,hypn ==> concl] 
  with [hypi] and [concl] atoms. Variables are assumed to be implicitly
  universally quantified. *)

type t = chain list

and chain = hyps * concl

and hyps = atom list

and concl = atom

and atom = 
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t

val pp : Format.formatter -> t -> unit

val is_diseq : t -> Term.t -> Term.t -> bool

val normalize : t -> Funsym.t -> Term.t list -> Term.t


module Flat : sig

  type t = chain list

  and chain = hyps * concl

  and hyps = hyp list

  and hyp = 
    | Equal of var * term
    | Diseq of var * var

  and conc = 
    | Equal of term * term
    | Diseq of term * term

  and var = Name.t

  and term = 
    | Var of Name.t
    | App of Funsym.t * Name.t list
	
end 


val flatten : t -> Flat.t
