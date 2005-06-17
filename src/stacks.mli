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

(** {i Datatype of stacks.}

  @author Harald Ruess
*)


type 'a t
  (** The type of stacks containing elements of type ['a]. *)

exception Empty
  (** Raised when {!Stacks.pop} or {!Stacks.top} is applied to an empty stack. *)

val create : unit -> 'a t
  (** Return a new stack, initially empty. *)

val top : 'a t -> 'a 
  (** [top s] returns the top of the stack for nonempty stack [s]. *)

val push : 'a -> 'a t -> unit
  (** [push x s] adds the element [x] at the top of stack [s]. *)

val pop : 'a t -> 'a
  (** [pop s] removes and returns the topmost element in stack [s],
    or raises [Empty] if the stack is empty. *)

val clear : 'a t -> unit
  (** Discard all elements from a stack. *)

val is_empty : 'a t -> bool
  (** Return [true] if the given stack is empty, [false] otherwise. *)
  
val length : 'a t -> int
  (** Return the number of elements in a stack. *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s],
    from the element at the top of the stack to the element at the
    bottom of the stack. The stack itself is unchanged. *)

val map : ('a -> 'a) -> 'a t -> unit
  (** [map f s] applies [f] to all elements [a] in [s] and replaces [a] with [f a]. *)

val to_list : 'a t -> 'a list
  (** [to_list s] collects elements of stack in a list. *)

val mem : ('a -> 'a -> bool) -> 'a -> 'a t -> bool
  (** [mem eq a s] tests if there is [b] in [s] such that [eq a b]. *)

val for_all : ('a -> bool) -> 'a t -> bool
  (** [for_all p s] tests if [p a] holds for each element [a] on stack [s]. *)

val sort : ('a -> 'a -> int) -> 'a t -> unit
  (** [Stacks.sort cmp s] sorts the elements of the stack [s] with [top s] the smallest 
    element according to the comparison function [cmp]. *)
