
(*i
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
 * 
 * Author: Jean-Christophe Filliatre
 i*)

(*s Module [Ptmap]: finite maps implemented as Patricia trees. *)

(*i*)
open Hashcons
(*i*)

type ('a,'b) t

(*s The empty map. *)

val empty : ('a,'b) t

(*s Check whether the argument map is undefined everywhere. *)

val is_empty : ('a,'b) t -> bool

(*s [add x y m] returns a map containing the same bindings as
 [m], plus a binding of [x] to [y]. If [x] was already bound
 in [m], its previous binding disappears. *)

val add : 'a hashed -> 'b -> ('a,'b) t -> ('a,'b) t

(*s [find x m] returns the current binding of [x] in [m],
 or raises [Not_found] if no such binding exists. *)

val find : 'a hashed -> ('a,'b) t -> 'b

(*s [remove x m] returns a map containing the same bindings as
  [m], except for [x] which is unbound in the returned map. *)

val remove : 'a hashed -> ('a,'b) t -> ('a,'b) t

val mem :  'a hashed -> ('a,'b) t -> bool

(*s [iter f m] applies [f] to all bindings in map [m].
 [f] receives the key as first argument, and the associated value
 as second argument. The order in which the bindings are passed to
 [f] is unspecified. Only current bindings are presented to [f]:
 bindings hidden by more recent bindings are not passed to [f]. *)

val iter : ('a hashed -> 'b -> unit) -> ('a,'b) t -> unit

(*s [map f m] returns a map with same domain as [m], where the
 associated value [a] of all bindings of [m] has been
 replaced by the result of the application of [f] to [a].
 The order in which the associated values are passed to [f]
 is unspecified. *) 

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t

(*s [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
 where [k1 ... kN] are the keys of all bindings in [m],
 and [d1 ... dN] are the associated data.
 The order in which the bindings are presented to [f] is
 unspecified. *)

val fold : ('a hashed -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c

(*s [to_list m] enumerates the associations [(x,y)] for all [x]
 in the domain of [m] such that [y] equals [find x m]. The
 order of bindings in the result is undefined. *)
  
val to_list : ('a,'b) t -> ('a hashed * 'b) list 

(*s [of_list l] generates a corresponding map. *)

val of_list :  ('a hashed * 'b) list -> ('a,'b) t
