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

type t = {
  mutable facts : Fact.t list;
  mutable clauses : Clause.t list
}

let empty = { facts = []; clauses = [] }

let is_empty g =
  match g.facts, g.clauses with
    | [], [] -> true
    | _ -> false

let eq g1 g2 = 
  g1.facts == g2.facts &&
  g1.clauses == g2.clauses

let pp fmt g =
  Pretty.set Fact.pp fmt g.facts;
  if not(g.clauses = []) then
    begin
      Format.fprintf fmt "\nDisjunctions: \n";
      Pretty.infixl Clause.pp "\n AND " fmt g.clauses
    end 

let get g =
  match g.facts with
    | [] -> raise Not_found
    | fct :: fcts' ->
	g.facts <- fcts'; fct
      

let mem fct g =
  let eq_fct fct' = Fact.eq fct fct' in
    List.exists eq_fct g.facts

let put fct g = 
  if not(mem fct g) then
    g.facts <- fct :: g.facts

let get_clause g =
  match g.clauses with
    | [] -> raise Not_found 
    | cl :: cls ->
	g.clauses <- cls; cl

let put_clause cl g =
(*
  try
    let fct = Clause.d_singleton cl in
      put fct g.facts
  with
      Not_found -> 
*)
	g.clauses <- cl :: g.clauses

(* don't replace in clauses. *)
let replace e g =
  g.facts <- List.map (Fact.replace e) g.facts

let copy g = { facts = g.facts; clauses = g.clauses}
