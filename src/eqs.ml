(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Deprecated. *)

module P = Partition


(** {6 Equality sets} *)

(** Abstract interface of an equality set. *)
module type SET = sig
  type t
  val theories : Th.t list
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val find : Th.t -> P.t * t -> Jst.Eqtrans.t
  val inv : Th.t -> P.t * t -> Jst.Eqtrans.t 
  val mem : Term.t -> t -> bool
  val fold : (Fact.Equal.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Fact.Equal.t -> unit) -> t -> unit
end


module Make(T: Solution.TH): SET = struct
  module S = Solution.Make0(T)
  type t = S.t
  let theories = [T.th]
  let eq = S.eq
  let pp = S.pp
  let empty = S.empty
  let is_empty = S.is_empty
  let find i (p, s) a =
    if i = T.th then S.find s a else raise Not_found
  let inv i (p, s) a =
    if i = T.th then S.inv s a else raise Not_found
  let mem x s =
    S.is_dependent s x || S.is_independent s x
  let fold = S.fold
  let iter = S.iter
end



(** Cross product of two equality sets. *)
module Cross(Left: SET) (Right: SET): (SET with type t = Left.t * Right.t) = struct
  
  type t = Left.t * Right.t

  let theories  =
    Left.theories @ Right.theories

  let eq (l1, r1) (l2, r2) =
    Left.eq l1 l2 && Right.eq r1 r2

  let pp fmt (l, r) =
    Left.pp fmt l;
    Right.pp fmt r

  let fold  f (l, r) acc =
    Left.fold f l (Right.fold f r acc)

  let iter f (l, r) =
    Left.iter f l;
    Right.iter f r

  let empty =
    (Left.empty, Right.empty)

  let is_empty (l, r) =
    Left.is_empty l && Right.is_empty r

  let mem a (l, r) =
    Left.mem a l || Right.mem a r

  let find i (p, (l, r)) a =
    if Th.mem i Left.theories then 
      Left.find i (p, l) a
    else if Th.mem i Right.theories then
      Right.find i (p, r) a
    else 
      raise Not_found

  let inv i (p, (l, r)) a = 
    if Th.mem i Left.theories then
      Left.inv i (p, l) a
    else if Th.mem i Right.theories then
      Right.inv i (p, r) a
    else 
      raise Not_found

end

