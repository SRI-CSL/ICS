(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
