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


(** Classification of function symbols
  
  @author Harald Ruess

  Function symbols are classified according to which theory they
  belong to.
 *)

type t =
  | Shostak of shostak
  | Can of can
  | Uninterpreted

and shostak =  A | P | BV | COP

and can = NL | APP | ARR

val a : t
val p : t
val bv : t
val cop : t

val nl : t
val app : t
val arr : t

val u : t

val to_string : t -> string
val of_string : string -> t
  
val fold : (t -> 'a -> 'a) -> 'a -> 'a
val iter : (t -> unit) -> unit
  
val for_all : (t -> bool) -> bool
val exists : (t -> bool) -> bool

val for_all_but : t -> (t -> bool) -> bool
val exists_but : t -> (t -> bool) -> bool

 
val is_shostak : t -> bool 
val is_can : t -> bool 
val is_uninterpreted : t -> bool 

val inj : t -> t option

val pp : t option Pretty.printer
