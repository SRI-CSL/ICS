
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
 * Author: Harald Ruess, N. Shankar
i*)


(*s Recognizer for variables. *)

let is a =
  (Term.args_of a = []) &&
  (match Sym.destruct (Term.sym_of a) with
     | Sym.Uninterp _ -> true
     | _ -> false)


(*s Ordering on variables. *)

let (<<<) a b =
  match Term.is_label a, Term.is_label b with
    | true, false -> false
    | false, true -> true
    | _ -> Term.(<<<) a b

(* Orientation of variables. *)

let orient ((a,b) as e) =
  if a <<< b then (b,a) else e


(*s Check for inconsistency of variable equality. *)

let inconsistent x y = 
  (Term.is_tt x && Term.is_ff y) || 
  (Term.is_ff x && Term.is_tt y)


(*s Equalities between variables. *)

type eqs = (Term.t * Term.t) list

let empty = []

let add x y el = (x,y) :: el

let union = (@)

let pp = Pretty.list Pretty.eqn
