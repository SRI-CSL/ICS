(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
*)

module Entry = struct

  type  t = 
    | Def of args * body
    | Spec of Spec.t
    | Context of Context.t

  and body = 
    | Term of  Term.t
    | Prop of Prop.t 

  and args = Name.t list

  let pp fmt = function
    | Def(args, Term(a)) -> 
	Format.fprintf fmt "@[def("; 
	Term.pp fmt a;
	Format.fprintf fmt ")@]@;"
    | Def(args, Prop(a)) -> 
	Format.fprintf fmt "@[def("; 
	Prop.pp fmt a;
	Format.fprintf fmt ")@]@;"
    | Spec(sp) -> 
	Spec.pp fmt sp;
    | Context(ctxt) -> 
	Context.pp fmt ctxt

end

type key = Name.t

let table = Name.Hash.create 17

let reset () = 
  Name.Hash.clear table

let _ = Tools.add_at_reset reset

let to_list () =
  let l = ref [] in
    Name.Hash.iter (fun n e -> l := (n, e) :: !l) table;
    !l

let pp fmt =
  Pretty.map Name.pp Entry.pp fmt (to_list ())

let lookup n = Name.Hash.find table n
	
let in_dom n = Name.Hash.mem table n

module Get = struct

  let term n len =
    match Name.Hash.find table n with
      | Entry.Def(xl, Entry.Term(a)) when List.length xl = len -> xl, a
      | _ -> raise Not_found

  let prop n len =
    match Name.Hash.find table n with
      | Entry.Def(xl, Entry.Prop(p)) when List.length xl = len -> xl, p
      | _ -> raise Not_found

  let spec n =
    match Name.Hash.find table n with
      | Entry.Spec(sp) -> sp
      | _ -> raise Not_found

  let context n =
    match Name.Hash.find table n with
      | Entry.Context(ctxt) -> ctxt
      | _ -> raise Not_found
end

(** Extending symbol table. *)
module Put = struct

  let error n = 
    let msg = "Name " ^ Pretty.to_string Name.pp n ^ " already in table" in 
      raise (Invalid_argument msg)

  let term n xl a =
    if in_dom n then error n else
      Name.Hash.add table n (Entry.Def(xl, Entry.Term(a)))

  let prop n xl p =
    if in_dom n then error n else
      Name.Hash.add table n (Entry.Def(xl, Entry.Prop(p)))

  let spec n sp =
    if in_dom n then error n else
      Name.Hash.add table n (Entry.Spec(sp))

  let context n ctxt =
    if in_dom n then error n else
      Name.Hash.add table n (Entry.Context(ctxt))

end

let restrict n =
  Name.Hash.remove table n




