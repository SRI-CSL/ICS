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

(** Function symbols

  @author Harald Ruess

  This module provides functionality for registering 
  function symbols and for accessing associated names and 
  theories. 
*)

(** A word of caution: some functions here break Ocaml's 
  Hindley-Milner type system in that they implement a 
  dependent function type [i: Th.t -> Sym(i)].  Instead
  of [Sym(i)] in this interface description we use the
  polymorphic ['sym]. *)

type t 
  (** {i Function symbols} have a
    - a name of type {!Name.t}
    - and a theory in {!Th.t}
    associated with it. *)

val create : Theory.t -> Name.t -> t

val name_of : t -> Name.t
   (** [get i f] returns [op] if function symbol [f] has been obtained 
     through a registration using {!Funsym.create}[i op]. The result is of
     type ['sym] as used in the registration of theory [i] using {!Funsym.register}.
     {!Obj.magic} needs to be applied to convert the result of [get i f] to the
     correct type [Sym(i)]. *)

val theory_of : t -> Theory.t
  (** [theory_of f] returns the theory of type {!Th.t} associated with [f]. *)

val is_interp : Theory.t -> t -> bool
  (** [is_interp i f] holds iff [theory_of f] equals [i]. *)
  
val eq : t -> t -> bool
  (** [eq f g] succeeds iff [f] and [g] represent the same function symbol.
    This test is performed in constant time (in particular, independent
    of the length of names of uninterpreted function symbols). *)

val cmp : t -> t -> int
  (** [cmp f g] returns [0] iff [eq f g] holds, and [cmp f g] is positive
    iff [cmp g f] is negative. Otherwise, the result is unspecified. 
    [cmp f g] might thus be viewed as representing a {i total ordering} [<=] on 
    function symbols with [f <= g] iff [cmp f g] is, say, nonpositive. *)

val hash : t -> int
  (** Nonnegative hash value for function symbols.  This value is not unique 
    to a function symbol. *)

val pp : Format.formatter -> t -> unit
  (** Pretty-printing applications of symbols to an argument list.
    The exact form of printing function symbols is determined by the 
    flag {!Pretty.flag}. *)

val to_string : t -> string
  (** Print a function symbol to a string (see also {!Funsym.pp}). *)

module Map : (Map.S with type key = t)

module Set : (Set.S with type elt = t)

(** Signature for theory [th]. Two operations
 of type [Sig.t] are considered to be equal iff if
 their corresponding names are equal. *)
module type SIG = sig
  val th : Theory.t
  type t
  val name : t -> Name.t
end

module Make(Sig: SIG) : sig

  val inj : Sig.t -> t

  val out : t -> Sig.t

  val is_interp : t -> bool
	
end 
