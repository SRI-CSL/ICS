
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


(*s Set of variable equalities encoded as a list. *)

type t = Veq.t list

let empty = []

let is_empty el = (el = [])

let destruct = function
  | e :: el -> (e,el)
  | _ -> assert false

let singleton e = [e]

let add x y el = 
  assert(Term.is_var x && Term.is_var y);
  Veq.make x y :: el

let union = (@)

let fold f = 
  List.fold_right
    (fun e ->
       let (x,y) = Veq.destruct e in
       f x y)

let to_list =
  List.map Veq.destruct

let pp fmt =
  Pretty.list Veq.pp fmt
