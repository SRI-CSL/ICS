
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

(*s Module [Main]: Toplevel of ICS command line interpreter. *)

let _ = Sys.catch_break true
 
(*s Options. *)

let stat_flag = ref false
let timing_flag = ref false
let disable_prompt_flag = ref false
     
(*s Interactive toplevel. Read commands from standard input and evaluate them. *)

let rec repl inch =
  usage ();
  let outch = Ics.stdout () in
  Ics.istate_set_in_channel inch;
  Ics.istate_set_out_channel outch;
  try
    while true do
      prompt outch;
      Ics.istate_eval ();
      Ics.istate_flush ();
    done
  with 
    | End_of_file -> exiting 0
    | Sys.Break -> exiting 1
    | Failure "drop" -> ()

and prompt outch =
  if not(!disable_prompt_flag) then Format.printf "\nics> @?"

and usage () =
  Format.eprintf "ICS: Integrated Canonizer and Solver.";
  Format.eprintf "\nCopyright (c) 2001,2002 SRI International.";
  Format.eprintf "\nType 'help.' for help about help, and 'Ctrl-d' to exit.@."
 
and exiting n = (*s Exiting the system (on Ctrl-C or Ctrl-D) *)
  if !stat_flag then 
    Ics.do_at_exit ();
  Ics.istate_flush ();
  exit n

let args () =
  let files = ref [] in
  Arg.parse
      [ "-s", Arg.Set stat_flag,           "  Print statistics";
	"-t", Arg.Set timing_flag,         "  Print timings";
	"-v", Arg.Int Ics.set_verbose,     "  Verbose levels 0,1,2,...";
	"-p", Arg.Set disable_prompt_flag, "  Disable printing of prompt"
      ]
      (fun f -> files := f :: !files)
      "usage: ics [-stvph] [files]";
  List.rev !files

let rec main () =
  match args () with
    | [] -> repl (Ics.stdin ())
    | l -> batch l

and batch l =
  disable_prompt_flag := true;
  List.iter 
    (fun x ->
       try
	 repl (Ics.in_of_string x)
       with
	 | Sys_error str -> Format.eprintf "\nSys_error(%s)@?" str)
    l

let _ = Printexc.catch main ()






