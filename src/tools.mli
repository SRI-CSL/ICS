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

(** Collection of useful functions used throughout ICS.

  @author Jean-Christophe Filliatre
  @author Harald Ruess
 *)


(** {6 Global variables} *)

val linenumber : int ref
  (** [linenumber] used by lexer and parser. *) 

val profiling : bool ref


(** {6 Exit functions} *)

val add_at_exit : (unit -> unit) -> unit
  (** Procedures [f] without parameters can be registered as exit procedures
    by [add_to_exit f].  Exit procedures can, for example be used for 
    displaying statistics after running ICS. *)

val do_at_exit : unit -> unit
  (** Procedures [f] registered by {!Tools.add_at_exit} are called 
    by [do_at_exit()]. The order in which these functions are called is
    unspecified. *)

(** {6 Reset functions} *)
  
val add_at_reset : (unit -> unit) -> unit
  (** [add_to_reset f] registers [f] as a reset procedure *)

val do_at_reset : unit -> unit
  (** [do_at_reset()]. which are then called, one-by-one, by [do_at_reset()]. *)

(** {6 Timings} *)

val utime : ('a -> 'b) -> 'a -> 'b * float
  (** [utime f a] returns not only the result of applying [f] to [a]
    but also the time required to compute the function. *)

   
(** {6 Profiling} *) 

val profile : string -> ('a -> 'b) -> ('a -> 'b)
  (** [profile str f] profiles function [f], and
    registers an exit function which outputs the number of calls
    of this function, and the total time spent in this function;
    the argument [str] is usually just the name of the function. *)


(** {6 Accumulation of side effects} *)

val acc1 : 'a list ref -> ('b -> 'c * 'a) -> 'b -> 'c
  (** [acc1 acc f b] calls [f b] to obtain [(c, a)].
    Now, [a] is consed to [acc] as a side effect
    and the result [c] is returned. *)

val acc2 : 'a list ref -> ('b1 -> 'b2 -> 'c * 'a) -> 'b1 -> 'b2 -> 'c
  (** [acc2 acc f b1 b2] calls [f b1 b2] to obtain [(c, a)].
    Now, [a] is consed to [acc] as a side effect and the result [c] 
    is returned. *)

val accb : 'a list ref -> ('b -> 'a option) -> 'b -> bool
  (** [accb acc p b] calls [p b]. If the result of evaluating
    [p b] is of the form [Some(a)], then [a] is consed to [acc] 
    as a side effect and the result [true] is returned. Otherwise,
    if [p b] is [None], there is no side effect, and [false] is returned. *)
