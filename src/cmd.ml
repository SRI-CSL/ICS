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


module Prompt = 
  Ref.String
    (struct
       type t = string
       let name = "prompt"
       let default = "ics> "
       let description =  "Current value of prompt for read-eval-print loop"
     end)


let lexing () = 
  Lexing.from_channel (Istate.Inchannel.get())

let prompt () =
  let out = Istate.Outchannel.get() in
    Format.fprintf out "\n%s " (Prompt.get());
    Format.pp_print_flush out ()



(** Read-Eval-Print loop. *)
let rec rep () =
  Istate.batch := false;
  try
    while true do
      Tools.linenumber := 0;    (* for error message *)
      try
	prompt ();
	Parser.commands Lexer.token (lexing())
      with
	  Parsing.Parse_error -> parse_error ()
    done 
  with
    | End_of_file -> 
	eof ();
	Istate.do_quit 0
    | Sys.Break -> 
	break();
	if !Istate.batch then Istate.do_quit 1 else rep ()
    | (Failure("drop") as drop) -> 
	raise drop
    | exc -> 
	error exc; rep ()

and parse_error () = 
  Format.fprintf (Istate.Outchannel.get())
    ":parse_error (on line %d)@." !Tools.linenumber

and error exc = 
  Format.fprintf (Istate.Outchannel.get())
    ":error %s@." (Printexc.to_string exc)

and break () = 
  Format.eprintf "\n:%s@." (Printexc.to_string Sys.Break)

and eof () = 
  Format.eprintf "\n:%s@." (Printexc.to_string End_of_file)

(** Batch processor. *)
and batch (inch) =
  Istate.batch := true;
  Tools.linenumber := 1;
  Istate.Inchannel.set inch;
  try
    while true do
      Parser.commandsequence Lexer.token (lexing())
    done;
    failwith "unreachable"
  with
    | Parsing.Parse_error -> parse_error(); 2
    | End_of_file -> 0
    | Sys.Break -> break(); 1
    | exc -> error exc; (-2)

let _ = Istate.cmdBatch := batch
