
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
 * Author: Harald Ruess
i*)


type t

val v : t -> V.focus
val d : t -> D.focus

val make : V.focus -> D.focus -> t

val empty : t

val add_v : V.focus -> t -> t
val add_d : D.focus -> t -> t

val union : t -> t -> t

val is_empty : t -> bool

val fold_v : (Term.t -> 'a -> 'a) -> t -> 'a -> 'a

val fold_d : (Term.t * Term.t -> 'a -> 'a) -> t -> 'a -> 'a
