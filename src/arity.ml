
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

(*i*)
open Hashcons
(*i*)

type arity =
  | Constant of Type.t
  | Functorial of Type.t list * Type.t

and t = arity Hashcons.hashed

let eq = (===)

let destruct a = a.node

(*s Hashconsing type. *)

module HashArity = Hashcons.Make(      
  struct 
    type t = arity

    let equal a b =
      match a, b with
	| Constant(c), Constant(d) ->
	    Type.eq c d
	| Functorial(dl1,r1), Functorial(dl2,r2) ->
	    Type.eq r1 r2
	    && (try List.for_all2 Type.eq dl1 dl2 
		with Invalid_argument _ -> false)
	| _ -> false

    let hash = Hashtbl.hash
  end)

let hashcons : arity -> t =
  let ht = HashArity.create 17 in
  Tools.add_at_reset (fun () -> HashArity.clear ht);
  HashArity.hashcons ht

let mk_constant t =
  hashcons(Constant(t))

let mk_functorial dl r =
  if dl = [] then
    mk_constant r
  else
    hashcons(Functorial(dl,r))

let pp fmt a =
  match a.node with
    | Constant(c) -> 
	Type.pp fmt c
    | Functorial(dl,r) ->
	Format.fprintf fmt "@[[";
	Tools.ppl ("",",","") Type.pp fmt dl;
	Format.fprintf fmt " -> ";
	Type.pp fmt r;
	Format.fprintf fmt "]@]"
