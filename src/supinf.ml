
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

type t = {
  sup : Term.Set.t;
  inf : Term.Set.t;
  diseqs : Term.Set.t;
  cnstrnt : Cnstrnt.t
}

let of_cnstrnt c = {
  sup = Term.Set.empty;
  inf = Term.Set.empty;
  diseqs = Term.Set.empty;
  cnstrnt = c
}

let is_empty c =
  Cnstrnt.is_empty c.cnstrnt

let inter c d = {
  sup = Term.Set.union c.sup d.sup;
  inf = Term.Set.union c.inf d.inf;
  diseqs = Term.Set.union c.inf d.inf;
  cnstrnt = Cnstrnt.inter c.cnstrnt d.cnstrnt
}
    
let inst x y c = 
  let repl s = 
    Term.Set.fold 
      (fun z -> 
	 Term.Set.add (Arith.replace x y z)) 
      s Term.Set.empty
  in
  {c with 
     sup = repl c.sup; 
     inf = repl c.inf;
     diseqs = repl c.diseqs}

let pp fmt c = 
  Cnstrnt.pp fmt c.cnstrnt;
  if not(Term.Set.is_empty c.sup) then
    begin
      Pretty.string fmt "sup: "; 
      Pretty.set Term.pp fmt (Term.Set.elements c.sup)
    end;
  if not(Term.Set.is_empty c.inf) then
    begin
      Pretty.string fmt "inf: "; 
      Pretty.set Term.pp fmt (Term.Set.elements c.inf)
    end;
  if not(Term.Set.is_empty c.diseqs) then
    begin
      Pretty.string fmt "diseqs: "; 
      Pretty.set Term.pp fmt (Term.Set.elements c.diseqs)
    end
