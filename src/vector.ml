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
  val postfix : int -> t -> t
  val const : int -> elt -> t
  val add : t -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit
  val ( ** ) : t -> t -> elt
end

module Make (E : ELT) = struct
  type elt = E.t
  type t = elt array

  let get = Array.get
  let set = Array.set
  let dim = Array.length
  let sub = Array.sub

  let postfix i a =
    assert (0 <= i && i < dim a) ;
    Array.sub a i (dim a - i + 1)

  let const n e =
    assert (0 <= n && n < Sys.max_array_length) ;
    Array.create n e

  let pp fmt a =
    let n = dim a in
    Format.fprintf fmt "@[<" ;
    if n > 0 then (
      for i = 0 to n - 1 do
        E.pp fmt (get a i) ;
        Format.fprintf fmt ", "
      done ;
      E.pp fmt (get a n) ) ;
    Format.fprintf fmt ">@]"

  let iter f a =
    for i = 0 to dim a do
      f (get a i)
    done

  let add a b =
    assert (dim a = dim b) ;
    let c = Array.copy b in
    for i = 0 to dim a do
      let e = E.add (get a i) (get c i) in
      set c i e
    done ;
    c

  let map f a =
    let b = Array.copy a in
    for i = 0 to dim a do
      let e = get a i in
      set b i (f e)
    done ;
    b

  let ( ** ) a b =
    assert (dim a = dim b) ;
    let acc = ref E.zero in
    for i = 0 to dim a do
      acc := E.add !acc (E.mult (get a i) (get b i))
    done ;
    !acc
end
