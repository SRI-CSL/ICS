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


let enabled = ref true


(** Registration of nonterminals for help command. *)
module Nonterminal = struct

  let registration = ref []

  let is_defined nt =
    List.mem_assoc nt !registration

  let register nt (alternatives: string list) description =
      begin
	assert(not(is_defined nt));
	registration := (nt, (alternatives, description)) :: !registration
      end 
      
  let definition_of nt =
    try
      List.assoc nt !registration
    with
	Not_found ->
	  raise(Invalid_argument
		  (Format.sprintf "No help available for nonterminal <%s>" nt))


  let rec pp fmt (nt, defs, descr) =
    let out = Format.std_formatter in
    Format.fprintf out "DEFINITION";
    List.iter
      (fun def -> 
	 Format.fprintf out "\n     <%s> ::= %s" nt def)
      defs;
    if descr <> "" then
      begin
	Format.fprintf out "\nDESCRIPTION";
	Format.fprintf out "\n%s" descr
      end
	
end

type command = {
  args : string;
  short : string;
  description : string;
  examples : (string * string) list;
  seealso : string
}


(** Commands are registered just for the purpose of producing
  consistent help texts for commands. *)
module Command = struct

  module Description = struct


    let mk_short args short = {
      args = args;
      short = short;
      description = "";
      examples = [];
      seealso = ""
    }

    let pp name fmt d =
      let out = Format.std_formatter in
      Format.fprintf out "NAME";
      Format.fprintf out "\n     %s - %s " name d.short;
      Format.fprintf out "\nSYNOPSIS";
      Format.fprintf out "\n     %s %s" name d.args;
      if d.description <> "" then
	Format.fprintf out "\nDESCRIPTION\n %s" d.description;
      if d.examples <> [] then
	 begin
	   Format.fprintf out "\n%s"
	     (if List.length d.examples <> 1 then "EXAMPLES" else "EXAMPLE");
	   List.iter
	     (fun (example, description) ->
		if description = "" then
		  Format.fprintf out "\n     %s" example
		else
		  Format.fprintf out "\n     %s - %s" example description)
	     d.examples
	 end;
      if d.seealso <> "" then
	Format.fprintf out "\nSEE ALSO\n %s" d.seealso;

  end

  open Description

  let descriptions = ref []

  let description_of name = 
    try
      List.assoc name !descriptions
    with
	Not_found -> raise (Invalid_argument (name ^ ": no such command@?"))

  let register name proc descr =
    if !enabled then
      begin
	(* assert(not(List.mem_assoc name !descriptions)); *)
	descriptions := (name, descr) :: !descriptions;
      end;
    proc


  let help1 name =
    let out = stdout in
      Description.pp name out (description_of name)
    

  let helpall () =
    let out = Format.std_formatter in
    let max = ref 10 in
      List.iter
	(fun (name, _) ->
	   let n = String.length name in
	     if n > !max then max := n)
	!descriptions;
      List.iter
	(fun (name, d) ->
	   let offset = ref (!max - String.length name) in
	     Format.fprintf out "\n%s" name;
	     while !offset >= 0 do 
	       offset := !offset - 1; Format.fprintf out " " 
	     done;
	     Format.fprintf out "%s" d.args)
	!descriptions
	
  let syntax nt = 
    let out = stdout in
    let defn, descr = Nonterminal.definition_of nt in
      Nonterminal.pp out (nt, defn, descr)
	
end

let on_help () = ()

let syntax = Command.syntax

let command = Command.help1

let commands = Command.helpall


(** Registration of nonterminals and commands for help. *)
module Register = struct

  let nonterminal = Nonterminal.register

  let command = Command.register

end
