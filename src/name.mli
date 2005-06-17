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

(** {i Datatype of names}

  A {i name} is a string with constant time equality test. This 
  is achieved by associating a unique integer with every string.

  @author Harald Ruess
*)


type t
  (** Representation of strings with constant time equality. *)

val to_string : t -> string
  (** [to_string n] returns the string associated with [n]. *)

val of_string : string -> t
  (** [of_string s] constructs a name with associated string [s]. *) 

val of_int : int -> t
  (* [of_int i] is the same as [of_string (Pervasives.string_of_int i)]. *)

val fresh : unit -> t
  (** [fresh s] return a name [n] with [to_string n] of the form [s!k]
    with [k] an integer. *)

val hash : t -> int
  (** Returns a hash value for a name [n]; that is,
    [to_string n = to_string m] implies [hash n = hash m]. *)
  
val equal : t -> t -> bool
  (** [equal n m] holds iff [to_string n] equals [to_string m]. 
    This equality test has constant runtime. *)

val compare : t -> t -> int
  (** If [s] ([t]) is the string associated to [n] ([m]), then
    [compare n m] equals [0] iff [equal n m]. Furthermore, 
    if [compare n m] equals [i], then [compare m n] equals [-i]. *)

val length : t -> int
  (** Length of a name. *)

val pp : Format.formatter -> t -> unit
  (** Pretty-printing of names. *)

module Set : (Set.S with type elt = t)
  (** Sets of names. *)

module Map : (Map.S with type key = t)
  (** Maps with names in the domain. *)

module Hash :  (Hashtbl.S with type key = t)
  (** Hash table with names as keys. *)
  
