
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 * 
 * Author: Harald Ruess
 i*)


(*s Module [Th]: Datatype for manipulating logical contexts 
 of interpreted theories. *)


module type INTERP = sig
  val name : string
  val is_th :  Term.t -> bool
  val iter : (Term.t -> unit) -> Term.t -> unit  
end

module Make(Th: INTERP) : sig

  type t

  val empty: unit -> t
  val copy : t -> t

  val subst_of : t -> Subst.t
  val use_of : t -> Term.ts Term.Map.t
  val inv_of : t -> Subst.t

  val inv : t -> Term.t -> Term.t
  val use : t -> Term.t -> Term.ts
  val mem : t -> Term.t -> bool
  val invmem : t -> Term.t -> bool

  (*s [find s a] returns the canonical form for
   non-fresh variables [a] with respect to [s]. *)

  val apply : t -> Term.t -> Term.t
  val find : t -> Term.t -> Term.t

  (*s [extend s (a,b)] extends the domain of [s]
   with [a] and destructively updates [s] such that
   [find s a] equals [b], [inv s b] equals [a], and
   the [use] structure of all [x] that occur interpreted
   in [b] are updated to also contain [b]. It assumes
   that the argument term [a] is not yet in the domain 
   of [s], i.e. [mem s a] is assumed to be false. *)
 
  val extend : t -> Eqn.t -> unit

  (*s [restrict s a] removes [a] from domain of [s]. *)

  val restrict : t -> Term.t -> unit

  (*s [union s (a,b)] sets the find of [a] to [b]. *)

  val union : t -> Eqn.t -> unit

end
