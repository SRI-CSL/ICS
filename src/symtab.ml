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

type entry = 
  | Def of defn
  | Arity of int
  | Type of Dom.t
  | State of Context.t

and defn = 
  | Term of Term.t
  | Prop of Prop.t

and t = entry Name.Map.t

let lookup = Name.Map.find

let empty_name = Name.of_string "empty"

let empty = 
  Name.Map.add
    empty_name
    (State (Context.empty))
    Name.Map.empty

let add n e s =
  if Name.Map.mem n s then
    let msg = "Name " ^ Pretty.to_string Name.pp n ^ " already in table" in 
    raise (Invalid_argument msg)
  else 
    Name.Map.add n e s

let remove n s = 
  if Name.eq n empty_name then s else Name.Map.remove n s

let filter p s = 
  Name.Map.fold 
    (fun n e acc -> 
       if p n e then Name.Map.add n e acc else acc)
    s
    Name.Map.empty

let state = filter (fun _ e -> match e with State _ -> true | _ -> false)
let def   = filter (fun _ e -> match e with Def _ -> true | _ -> false)
let arity = filter (fun _ e -> match e with Arity  _ -> true | _ -> false)
let typ   = filter (fun _ e -> match e with Type  _ -> true | _ -> false)

let rec pp fmt s =
  let ml = Name.Map.fold (fun n e acc -> (n, e) :: acc) s [] in
    Pretty.map Name.pp pp_entry fmt ml 

and pp_entry fmt e =
  let pr a = 
    if !pretty then () else Format.fprintf fmt a
  in
    match e with
      | Def(Term(x)) -> pr "@[def("; Term.pp fmt x; pr ")@]"
      | Def(Prop(x)) -> pr "@[def("; Prop.pp fmt x; pr ")@]"
      | Arity(a) -> pr "@[sig("; Format.fprintf fmt "%d" a; pr ")@]"
      | Type(c) -> pr "@[type("; Dom.pp fmt c; pr ")@]"
      | State(s) -> 
	  pr "@[state(";
	  Pretty.list Atom.pp fmt (Context.ctxt_of s);
	  pr ")@]"

and pretty = ref true
