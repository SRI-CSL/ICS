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

open Term
open Th

type t = (Term.t * Fact.justification option) Map.t

let empty = Map.empty
 
let is_empty s = (s == Map.empty)

let eq s t = (s == t)


(** Fold over the [find] structure. *)
let fold = Term.Map.fold


(** Solution set *)
let to_list s =
  fold (fun x (b,_) acc -> (x, b) :: acc) s []

(** Pretty-printer. *)

let pp fmt s =
  Pretty.string fmt "\nsl:";
  Pretty.list (Pretty.eqn Term.pp) fmt (to_list s) 

let apply s x =
  match x with
    | App _ -> raise Not_found (* Invariant: only vars in domain of [s]. *)
    | _ -> fst(Map.find x s)

let find s x = 
  match x with
    | App _ -> x
    | _ -> (try fst(Map.find x s) with Not_found -> x)

    
let mem s x = Map.mem x s

let update e s =
  let (x, a, rho) = Fact.d_equal e in
    Map.add x (a, rho) s

let restrict = Map.remove
