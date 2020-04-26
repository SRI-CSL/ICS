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

module type EQUAL = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type ORDERED = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type PRODUCT = sig
  type elt1
  type elt2
  type t

  val make : elt1 -> elt2 -> t
  val fill : t -> elt1 -> elt2 -> unit
  val lhs : t -> elt1
  val rhs : t -> elt2
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module Product (Ordered1 : ORDERED) (Ordered2 : ORDERED) = struct
  type elt1 = Ordered1.t
  type elt2 = Ordered2.t
  type t = {mutable lhs: elt1; mutable rhs: elt2}

  let make a b = {lhs= a; rhs= b}

  let fill p a b =
    p.lhs <- a ;
    p.rhs <- b

  let lhs p = p.lhs
  let rhs p = p.rhs
  let equal p q = Ordered1.equal p.lhs q.lhs && Ordered2.equal p.rhs q.rhs

  let compare p q =
    let c1 = Ordered1.compare p.lhs q.lhs in
    if c1 <> 0 then c1 else Ordered2.compare p.rhs q.rhs

  let hash p = (Ordered1.hash p.lhs + Ordered2.hash p.rhs) land 0x3FFFFFFF

  let pp fmt p =
    Format.fprintf fmt "@[<" ;
    Ordered1.pp fmt p.lhs ;
    Format.fprintf fmt ", " ;
    Ordered2.pp fmt p.rhs ;
    Format.fprintf fmt ">@]@?"
end

module Triple (Ordered1 : ORDERED) (Ordered2 : ORDERED) (Ordered3 : ORDERED) =
struct
  type elt1 = Ordered1.t
  type elt2 = Ordered2.t
  type elt3 = Ordered3.t

  type t =
    { mutable arg1: elt1
    ; mutable arg2: elt2
    ; mutable arg3: elt3
    ; mutable hash: int }

  let make a b c = {arg1= a; arg2= b; arg3= c; hash= -1}

  let dummy () =
    let arb1 = Obj.magic None
    and arb2 = Obj.magic None
    and arb3 = Obj.magic None in
    make arb1 arb2 arb3

  let fill p a b c =
    p.arg1 <- a ;
    p.arg2 <- b ;
    p.arg3 <- c ;
    p.hash <- -1

  let arg1 p = p.arg1
  let arg2 p = p.arg2
  let arg3 p = p.arg3

  let equal p q =
    Ordered1.equal p.arg1 q.arg1
    && Ordered2.equal p.arg2 q.arg2
    && Ordered3.equal p.arg3 q.arg3

  let compare p q =
    let c1 = Ordered1.compare p.arg1 q.arg1 in
    if c1 <> 0 then c1
    else
      let c2 = Ordered2.compare p.arg2 q.arg2 in
      if c2 <> 0 then c2 else Ordered3.compare p.arg3 q.arg3

  let hash p =
    if p.hash >= 0 then p.hash
    else
      let h =
        (Ordered1.hash p.arg1 + Ordered2.hash p.arg2 + Ordered3.hash p.arg3)
        land 0x3FFFFFFF
      in
      p.hash <- h ;
      h

  let pp fmt p =
    Format.fprintf fmt "@[<" ;
    Ordered1.pp fmt p.arg1 ;
    Format.fprintf fmt ", " ;
    Ordered2.pp fmt p.arg2 ;
    Format.fprintf fmt ", " ;
    Ordered3.pp fmt p.arg3 ;
    Format.fprintf fmt ">@]@?"
end
