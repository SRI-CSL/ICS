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

(** Datatype of names. There is a string associated with every name.
  
  @author Harald Ruess
*)

type t

val of_string : string -> t
  (** [of_string s] constructs a name with associated string [s]. *)

val to_string : t -> string
  (** [to_string n] returns the string associated with [n]. *)

val eq : t -> t -> bool
  (** [eq n m] holds if the strings associated with [n] and [m] are
    equal. This test has constant runtime. *)

val cmp : t -> t -> int
  (** If [s] ([t]) is the string associated to [n] ([m]), then
    [cmp n m] is just [Pervasives.compare s t]. *)
  
val pp : t Pretty.printer
  (** Pretty-printing of names. *)

val hash : t -> int
  (** Return hash values for names [n]. Not injective. *)

module Set : (Set.S with type elt = t)
  (** Sets of names. *)

module Map : (Map.S with type key = t)
  (** Maps with names in the domain. *)

val pp_map: 'a Pretty.printer -> 'a Map.t Pretty.printer
  (** Given a printer [p] for elements of the codomain,
    [pp_map p] prints a name map. *)
  
