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

(** Memoization

  @author Harald Ruess

  Memoization of functions. Uses weak arrays for hash tables to avoid
  memory leaks.
*)

module type MEMO = sig
  type d
  type r
  val eq : d -> d -> bool
  val hash : d -> int
  val f : d -> r
end

module Make(Memo: MEMO) : sig 
  val memoize : Memo.d -> Memo.r
  val fold : (Memo.d -> Memo.r -> 'a -> 'a) -> 'a -> 'a
  val iter : (Memo.d -> Memo.r -> unit) -> unit
end
