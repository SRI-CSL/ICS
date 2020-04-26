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

(** Theory names.

    @author Harald Ruess *)

type t = Top | U | A | P | F

val equal : t -> t -> bool
val sub : t -> t -> bool
val pp : Format.formatter -> t -> unit

val to_string : t -> string
(** [to_string i] returns a name for theory [i]. *)

val description : t -> string
(** Return a description of the theory. *)

(** Finite sets of theories. *)
module Set : Set.S with type elt = t

(** Finite maps with theories as domain. *)
module Map : Map.S with type key = t

(** Hash table with theories as keys. *)
module Hash : Hashtbl.S with type key = t

module type T = sig
  val theory : t
end

module Top : T
module U : T
module A : T
module P : T
module F : T
