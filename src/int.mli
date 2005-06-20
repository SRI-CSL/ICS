(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)
    

(** {i Integers}

  The signature matches the totally ordered type {!Type.ORDERED}. 

  @author Harald Ruess
*)

type t = int
    (** Integers. *)

val equal : t -> t -> bool
  (** Equality on integers. *)

val compare : t -> t -> int
  (** The result of [compare n m]
    - equals [0] iff [n = m],
    - is negative if [n < m], and
    - it is positive [n > m]. *)

val hash : t -> int
  (** [hash n] returns a nonnegative hash value. *)

val pp : Format.formatter -> t -> unit
  (** Pretty-print an integer on the given formatter. *)
