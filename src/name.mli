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

(** Datatype of strings with constant equality
  
  @author Harald Ruess
*)


type t
  (** Representation of strings with constant time equality. 
    Notice that {!Tools.do_at_reset} flushes internal structures
    and therefor invalidates any use of names. In particular,
    names should never be used in global variables. *)
  

val of_string : string -> t
  (** [of_string s] constructs a name with associated string [s]. *)
  

val to_string : t -> string
  (** [to_string n] returns the string associated with [n]. *)
  

val eq : t -> t -> bool
  (** [eq n m] holds if the strings associated with [n] and [m] are
    equal. This test has constant runtime. *)


val compare : t -> t -> int
  (** If [s] ([t]) is the string associated to [n] ([m]), then
    [compare n m] equals [0] iff [eq n m]. Furthermore, if [compare n m]
    equals [i], then [compare m n] equals [-i]. *)

  
val pp : t Pretty.printer
  (** Pretty-printing of names. *)

  
val hash : t -> int
  (** Return hash values for names [n]. Not injective. *)

  
module Set : (Set.S with type elt = t)
  (** Sets of names. *)

  
module Map : (Map.S with type key = t)
  (** Maps with names in the domain. *)

  
module Hash :  (Hashtbl.S with type key = t)
  (** Hash table with names as keys. *)
  
