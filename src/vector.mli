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

(** {i Datatype of vectors}

  @author Harald Ruess
*)

module type ELT = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val zero : t
  val one : t
  val add : t -> t -> t
  val mult : t -> t -> t
end

module type S = sig
  type elt
  type t
  val dim : t -> int
  val get : t -> int -> elt 
  val sub : t -> int -> int -> t
  val postfix : t -> int -> t
  val const : int -> elt -> t
  val add : t -> t -> unit
  val map : (elt -> elt) -> t -> unit
  val iter : (elt -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit
  val ( ** ) : t -> t -> elt
end

module Make(E: ELT): (S with type elt = E.t)
