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

module type MEMO = sig
  type d
  type r
  val eq : d -> d -> bool
  val hash : d -> int
  val f : d -> r
end

module Make(Memo: MEMO) = struct

  module Memoize = Weakhash.Make(
    struct
      type t = Memo.d
      let equal = Memo.eq
      let hash = Memo.hash
    end)

 let table = Memoize.create 17
     
 let memoize x =
   try Memoize.find table x with Not_found ->
     let y = Memo.f x in
       Memoize.add table x y;
       y

 let stats () = Memoize.stats table
 let iter f = Memoize.iter f table
 let fold f = Memoize.fold f table
 let to_list () = Memoize.to_list table

end

module M2 = struct
  type d = {mutable arg1: int; mutable arg2: int}
  let make x y = 
    let f = fun d -> Format.eprintf "Gc: {arg1 = %d; arg2 = %d}@." d.arg1 d.arg2 in
    let d = {arg1 = x; arg2 = y} in
      Gc.finalise f d;
      d
    type r = int
    let eq x y = (x.arg1 = y.arg1 && x.arg2 = y.arg2)
    let hash x = x.arg1 + x.arg2
    let f x = 
      Format.eprintf "\nCompute %d*%d@." x.arg1 x.arg2;
      x.arg1 * x.arg2
end

module F = Make(M2)

open M2

let f = F.memoize
let stats = F.stats

let x  = (make 2 3)
let y  = (make 3 4)

let _ = f x
let _ = f y
let _ = stats()
let _ = f (make 5 4)
let _ = f x
let _ = stats()
let _ = Gc.major()
let _ = stats()
let _ = f x
let _ = f (make 5 4)

