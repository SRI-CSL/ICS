
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
open Hashcons
open Sym
open Term
(*i*)

let is_interpreted a =
  match a.node with
    | App({node=Interp(Enum _)},_) -> true
    | _ -> false

let d_enum a =
 match a.node with
    | App({node=Interp(Enum({elems=ns;idx=n}))},_) -> Some(ns,n)
    | _ -> None


let mk_enum ns n =
  Term.make(Sym.mk_enum ns n,[])

let is_enum = is_interpreted

let d_enum a =
  match a.node with
    | App({node=Interp(Enum({elems=ns;idx=n}))},[]) -> 
	Some(ns,n)
    | _ ->
	None

(*s Typing. *)

let tau {elems=ns;idx=n} l =
  assert(l = []);
  if Name.Set.mem n ns then Type.mk_enumerative ns else Type.mk_bot

let type_of f a =
  match a.node with
    | App({node=Interp(Enum(e))},[]) -> tau e []
    | _ -> f a

(*s Sigmatizing. *)

let sigma ({elems=ns;idx=n} as a) l =
  match l with
    | [] -> (mk_enum ns n)
    | _ -> failwith "Enum.sigma: ill-formed expression"


(*s Normalize a term with respect to a given substitution. *)

let norm s a =
  try
    Ptmap.find a s
  with
      Not_found -> a


(*s Solving in the theory of enumeration types. *)

let solve (a,b) =
  match d_enum a, d_enum b with
    | Some(ns,n), Some(ms,m)
	when not (Name.Set.equal ns ms) || not(Name.eq n m) ->
	  raise Exc.Inconsistent
    | Some _, None ->
	[(b,a)]
    | None, Some _ ->
	[(a,b)]
    | _ ->
	[order a b]


