(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
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
  val add : t -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit
  val ( ** ) : t -> t -> elt
end


module Make(E: ELT) = struct
  type elt = E.t

  type t = elt array

  let get = Array.get
  let set = Array.set
  let dim = Array.length
  let sub = Array.sub
 
  let postfix i a = 
    assert(0 <= i && i < dim a);
    Array.sub a i (dim a - i + 1)

  let const n e = 
    assert(0 <= n && n < Sys.max_array_length);
    Array.create n e

  let pp fmt a = 
    let n = dim a in
      Format.fprintf fmt "@[<";
      if n > 0 then 
	(for i = 0 to n - 1 do
	   E.pp fmt (get a i);
	   Format.fprintf fmt ", ";
	 done;
	 E.pp fmt (get a n));
      Format.fprintf fmt ">@]"
	
  let iter f a = 
    for i = 0 to dim a do
      f (get a i)
    done

  let add a b = 
    assert(dim a = dim b);
    let c = Array.copy b in
      for i = 0 to dim a do
	let e = E.add (get a i) (get c i) in
	  set c i e
      done;
      c
      

  let map f a = 
    let b = Array.copy a in
      for i = 0 to dim a do
	let e = get a i in
	  set b i (f e)
      done;
      b

  let ( ** ) a b = 
    assert(dim a = dim b);
    let acc = ref E.zero in
      for i = 0 to dim a do
	acc := E.add !acc (E.mult (get a i) (get b i))
      done;
      !acc
end
