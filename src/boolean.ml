(*
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
 *)

(** {6 Constants} *)

let mk_true () = Bitvector.mk_one 1
let mk_false () = Bitvector.mk_zero 1
  (* These constants needs to be rebuild after
     resetting symbol tables. *)


(** {6 Recognizers} *)

let is_true a = (Term.eq a (mk_true()))
let is_false a = (Term.eq a (mk_false()))

