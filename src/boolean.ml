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


(** {6 Recognizers} *)

let is_true a = (Term.eq a (mk_true()))
let is_false a = (Term.eq a (mk_false()))


(** {6 Connectives} *)

let mk_conj a b = 
  Bitvector.mk_bitwise 1 a b (mk_false())

let mk_disj a b = 
  Bitvector.mk_bitwise 1 a (mk_true()) b

let mk_xor a b = 
  Bitvector.mk_bitwise 1  a
    (Bitvector.mk_bitwise 1 b (mk_false()) (mk_true()))
    a
let mk_neg a = 
  Bitvector.mk_bitwise 1 a (mk_false()) (mk_true())
