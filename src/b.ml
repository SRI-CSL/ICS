
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
open Term
(*i*)

type t = {
  valid : Term.t option;
  invalid : Term.t option
}

(*s Empty context. *)

let empty = {
  valid = None;
  invalid = None
}
 
(*s Accessors. *)

let solution_of s =
  match s.valid, s.invalid with
    | None, None -> []
    | Some(x), None -> [(x, Boolean.mk_true)]
    | None, Some(y) -> [(y, Boolean.mk_false)]
    | Some(x), Some(y) -> [(x, Boolean.mk_true); (y, Boolean.mk_false)]


let apply s a =
  match s.valid, s.invalid with
    | Some(x), _ when eq x a -> Boolean.mk_true
    | _, Some(y) when eq y a -> Boolean.mk_false
    | _ -> raise Not_found

let find s a =
  try apply s a with Not_found -> a

let inv s a =
  if Boolean.is_true a then
    match s.valid with Some(x) -> x | None -> raise Not_found
  else if Boolean.is_false a then
    match s.invalid with Some(x) -> x | None -> raise Not_found
  else 
    raise Not_found

let mem s a =
  match s.valid, s.invalid with
    | Some(x), _ -> eq x a 
    | _, Some(y) -> eq y a
    | None, None -> false

let use s _ = Term.Set.empty



(*s Pretty-printing. *)

let pp fmt s =
  let m = solution_of s in
  if m <> [] then
    begin
      Pretty.string fmt "b:";
      Pretty.map Term.pp Term.pp fmt m;
      Pretty.string fmt "\n"
    end


(*s Add equations between slack variables. *)

let merge e s =
  let (x,y) = Veq.destruct e in
  match s.valid, s.invalid with
    | Some(t), None when eq x t ->
	({s with valid = Some(y)}, Veqs.empty)
    | None, Some(f) when eq x f ->
	({s with invalid = Some(y)}, Veqs.empty)
    | Some(t), Some(f) ->
	if eq t x then
	  if eq f y then
	    raise Exc.Inconsistent
	  else 
	    ({s with valid = Some(y)}, Veqs.empty)
	else if eq f x then
	  if eq t y then
	    raise Exc.Inconsistent
	  else 
	    ({s with invalid = Some(y)}, Veqs.empty)
	else 
	  (s, Veqs.empty)  
    | _ ->
	(s, Veqs.empty)


(*s Extend. *)

let extend b s =
  assert(Boolean.is_interp b);
  let n = Name.of_string (if Boolean.is_true b then "t" else "f") in
  let x = Term.mk_fresh_var n None in
  if Boolean.is_true b then
    (x, {s with valid = Some(x)})
  else 
    (x, {s with invalid = Some(x)})
