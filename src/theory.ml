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

type t = Top | U | A | P | F

let to_string = function
  | Top -> "top"
  | U -> "u"
  | A -> "a"
  | P -> "p"
  | F -> "f"

let index = function U -> 0 | A -> 1 | P -> 2 | F -> 3 | Top -> 4
let pp fmt t = Format.fprintf fmt "%s" (to_string t)
let equal = ( == )

let compare t1 t2 =
  let i1 = index t1 and i2 = index t2 in
  if i1 == i2 then 0 else if i1 > i2 then 1 else -1

let sub t1 = function Top -> true | t2 -> t1 == t2

module T = struct
  type theory = t (* avoid name clash. *)

  type t = theory

  let equal = equal
  let compare = compare
  let hash = index
end

module Map = Map.Make (T)
module Set = Set.Make (T)
module Hash = Hashtbl.Make (T)

let description = function
  | Top -> "The (disjoint) union of the theories U, A, P, F."
  | U -> "Equality theory of uninterpreted functions."
  | A -> "Theory of linear arithmetic."
  | P -> "Equality theory of pairs."
  | F -> "Equality theory of functional arrays."

module type T = sig
  val theory : t
end

module Top : T = struct
  let theory = Top
end

module U : T = struct
  let theory = U
end

module A : T = struct
  let theory = A
end

module P : T = struct
  let theory = P
end

module F : T = struct
  let theory = F
end
