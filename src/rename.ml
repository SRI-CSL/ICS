
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

(*s Construction of renaming variables for purification. *)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let mk_fresh () =
  let f = Sym.make(Sym.Internal(Sym.Label(!k))) in
  incr(k);
  Term.mk_const f

let is_fresh a =
  match Sym.destruct (Term.sym_of a) with
    | Sym.Internal(Sym.Label _) -> true
    | _ -> false
