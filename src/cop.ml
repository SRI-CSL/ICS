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


(** Encoding of the theory of coproducts as a Shostak theory. *)
module T: Shostak.T = struct
  let th = Theory.of_string "cop"
  let map = Coproduct.map
  let solve e =
    let (a, b, rho) = e in
      try
	let sl = Coproduct.solve (a, b) in
	let inj (a, b) = (a, b, rho) in
	  (List.map inj sl, Term.Varset.empty)
      with
	  Exc.Inconsistent -> raise(Jst.Inconsistent(rho))
  let disjunction _ = raise Not_found
end


(** Inference system for coproducts as an instance of
  a Shostak inference system. *)
module S: Shostak.S =
  Shostak.Make(T)

module Component = 
  Shostak.Component(T)

module P = 
  E.Register(Component)
