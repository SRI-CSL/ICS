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


type obj = {
  set : value -> unit;
  get : unit -> value;
  reset : unit -> unit;
  description : string;
}

and value = string

let references = Name.Hash.create 7

let no_such_reference_error n =
  let msg = Format.sprintf "Parameter %s unknown." (Name.to_string n) in
    raise (Invalid_argument msg)
      
let get n = 
  try
    let r = Name.Hash.find references n in
      r.get ()  
  with
      Not_found -> no_such_reference_error n
	  
let set n v =
  try
    let r = Name.Hash.find references n in
      r.set v
  with
      Not_found -> no_such_reference_error n
	
let reset () = 
  Name.Hash.iter (fun _ r -> r.reset ()) references
    
let description n =
  try
    let r = Name.Hash.find references n in
      r.description
  with
	Not_found -> no_such_reference_error n

let iter f =
  Name.Hash.iter (fun n _ -> f n) references


module type KIND = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

module type DESCRIPTION = sig
  type t
  val name : string
  val default : t
  val description : string
end

module type REF = sig
  type t 
  val set : t -> unit
  val get : unit -> t
  val reset : unit -> unit
end

module Make(Kind: KIND)(Ref: (DESCRIPTION with type t = Kind.t)) = struct

  type t = Kind.t

  let reference = ref Ref.default

  let set (v: Kind.t) = reference := v

  let get () = !reference

  let reset () = reference := Ref.default
  
  let _ = Tools.add_at_reset reset

  let _ = 
    let name = Name.of_string Ref.name in
      if Name.Hash.mem references name then
	invalid_arg ("Parameter already registered: " ^ Ref.name)
      else 
	Name.Hash.add references name {
	  set = (fun str -> reference := Kind.of_string str);
	  get = (fun () ->  Kind.to_string !reference);
	  reset = (fun () -> reference := Ref.default);
	  description = Ref.description
	}

end

module type BOOLEAN = (REF with type t = bool)

module Boolean = 
  Make(
    struct
      type t = bool
      let to_string = function
	| true -> "true"
	| false -> "false"
      let of_string = function
	| "true" -> true
	| "false" -> false
	| str -> invalid_arg ("Not a Boolean value: " ^ str) 
    end)

module type STRING = (REF with type t = string)

module String = 
  Make(
    struct
      type t = string
      let to_string str = str
      let of_string str = str 
    end)



