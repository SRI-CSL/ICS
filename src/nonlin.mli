
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

(*i*)
open Mpa
open Term
(*i*)

(*s Module [Nonlin]: Constructors, recognizers, and accessors for
 nonlinear arithmetic terms. *)

(*s [is_interp a] holds iff [a] is a nonlinear arithmetic terms; that is,
  [a] is a nonlinear multiplication ([mult]) *)

val is_interp: Term.t -> bool

(* All non-arithmetic terms, that is, terms [a] for which [is_linarith a]
   fails are considered to be uninterpreted. [iter f a] is used to apply
   procedure [f] at uninterpreted positions of [a]. *)

val iter: (Term.t -> unit) -> Term.t -> unit        


(*s [fold f a e] applies [f] at uninterpreted positions of [a] and 
 accumulates the results starting with [e]. *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a

(*s Constructors for building up nonlinear arithmetic terms. *)

val mk_mult : Term.t * Term.t -> Term.t
val mk_expt : int -> Term.t -> Term.t

(*s Destructors. *)

val d_mult : Term.t -> Term.t list option
val d_expt : Term.t -> (int * Term.t) option

(*s sigmatize *)

val sigma : Sym.nonlin -> Term.t list -> Term.t

(*s Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in polynomial normal form.  Thus, [norm] can be thought
  of as the the composition of applying substitution [s] to [a] followed by 
  sigmatizing each arithmetic subterm by the function [sigma] above. *)

val norm: (Term.t -> Term.t) -> Term.t -> Term.t

(*s Partial solver. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list

