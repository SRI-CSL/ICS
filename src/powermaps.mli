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

(** {i Exponential maps}

    This module implements finite maps with bindings of the form
    [x |-> {x{1},...,x{n}}]. All update operations over maps are destructive
    (side-effects).

    The implementation uses {i Splay trees} for dynamic balancing of binary
    tree representations. Therefore, insertion of a new element, for
    example, is of {i amortized} logarithmic cost. *)

(** Output signature of the functor {!Powermaps.Make}. *)
module type S = sig
  type key

  module Values : Sets.S with type elt = key

  type t

  val empty : unit -> t
  val is_empty : t -> bool
  val singleton : key -> Values.t -> t
  val is_singleton : t -> bool
  val find : key -> t -> Values.t
  val set : key -> Values.t -> t -> unit
  val remove : key -> t -> unit
  val copy : t -> t
  val mem : key -> t -> bool
  val iter : (key -> Values.t -> unit) -> t -> unit
  val fold : (key -> Values.t -> 'b -> 'b) -> t -> 'b -> 'b
  val cardinal : t -> int
  val map : (Values.t -> Values.t) -> t -> t
  val replace : t -> key -> key -> t
  val for_all : (key -> Values.t -> bool) -> t -> bool
  val exists : (key -> Values.t -> bool) -> t -> bool
  val choose : (key -> Values.t -> bool) -> t -> key * Values.t
  val destruct : (key -> Values.t -> bool) -> t -> key * Values.t * t
  val to_list : t -> (key * Values.t) list
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(** {i Exponential maps}. Functor building an implementation of maps with
    bindings of the form [x |-> {x{1},...,x{n}}] given a totally ordered
    type [T]. *)
module Make (T : Maps.OrderedType) : S with type key = T.t
