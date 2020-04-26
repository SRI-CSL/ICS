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

(** {i Datatype of names}

    A {i name} is a string with constant time equality test. This is
    achieved by associating a unique integer with every string.

    @author Harald Ruess *)

(** Representation of strings with constant time equality. *)
type t

val to_string : t -> string
(** [to_string n] returns the string associated with [n]. *)

val of_string : string -> t
(** [of_string s] constructs a name with associated string [s]. *)

val of_int : int -> t
(** [of_int i] is the same as [of_string (Pervasives.string_of_int i)]. *)

val fresh : unit -> t
(** [fresh s] return a name [n] with [to_string n] of the form [s!k] with
    [k] an integer. *)

val hash : t -> int
(** Returns a hash value for a name [n]; that is,
    [to_string n = to_string m] implies [hash n = hash m]. *)

val equal : t -> t -> bool
(** [equal n m] holds iff [to_string n] equals [to_string m]. This equality
    test has constant runtime. *)

val compare : t -> t -> int
(** If [s] ([t]) is the string associated to [n] ([m]), then [compare n m]
    equals [0] iff [equal n m]. Furthermore, if [compare n m] equals [i],
    then [compare m n] equals [-i]. *)

val length : t -> int
(** Length of a name. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printing of names. *)

(** Sets of names. *)
module Set : Set.S with type elt = t

(** Maps with names in the domain. *)
module Map : Map.S with type key = t

(** Hash table with names as keys. *)
module Hash : Hashtbl.S with type key = t
