
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
open Ics
(*i*)

(*s Module [Main]: Toplevel of ICS command line interpreter. *)

let _ = Sys.catch_break true
 
(*s Options. *)

let stat_flag = ref false
let timing_flag = ref false
let disable_prompt_flag = ref false
let disable_usage_flag = ref false
let disable_pretty_print_flag = ref false
let end_of_transmission = ref ""
let disable_compactify_flag = ref false
let disable_nonlin_flag = ref false
let maxloops_flag = ref !Rule.maxclose
let portnum_flag = ref None
     
(*s Interactive toplevel. Read commands from standard input and evaluate them. *)

let rec repl inch =
  try
    usage ();
    Ics.init (1, 
              not !disable_pretty_print_flag,
              !end_of_transmission,
              inch,
              Ics.channel_stdout());
    Tools.linenumber := 1;
    while true do
      prompt ();
      Ics.cmd_rep ()
    done
  with
    | Failure "drop" -> ()

and prompt () =
  if not(!disable_prompt_flag) then 
    Format.eprintf "\n%d ics> @?" !Tools.linenumber

and usage () =
  if not(!disable_usage_flag) then
    begin
      Format.eprintf "ICS: Integrated Canonizer and Solver.";
      Format.eprintf "\nCopyright (c) 2001,2002 SRI International.";
      Format.eprintf "\nType 'help.' for help about help, and 'Ctrl-d' to exit.@."
    end

and batch l =
  usage();
  disable_prompt_flag := true;
  List.iter 
    (fun x -> 
       let inch = Ics.inchannel_of_string x in
	 Ics.init (1, 
		   not !disable_pretty_print_flag,
		   !end_of_transmission,
		   inch,
		   Ics.channel_stdout());
	 Tools.linenumber := 1;
	 while true do
	   prompt ();
	   Ics.cmd_batch ()
	 done)
    l;
  Ics.flush();


and server portnum = 
  let addr = Unix.inet_addr_any in
  let sockaddr = Unix.ADDR_INET (addr, portnum) in
  Unix.establish_server 
    (fun inch outch ->
       let formatter = Format.formatter_of_out_channel outch in
       Ics.init (0, false, !end_of_transmission, inch, formatter);
       while true do
	 Ics.cmd_rep ()
       done)
    sockaddr

let args () =
  let files = ref [] in
  Arg.parse
      [ "-timings", Arg.Set timing_flag,          "Print timings";
	"-prompt", Arg.Set disable_prompt_flag,   "Disable printing of prompt";
        "-pp", Arg.Set disable_pretty_print_flag, "Disable Pretty-Printing of terms";
        "-usage", Arg.Set disable_usage_flag,     "Disable printing of usage message";
        "-compactify", Arg.Set disable_compactify_flag, "Disable compactification";
        "-groebner", Arg.Set Rule.groebner_completion, "Enable Groebner completion";
	"-nonlin", Arg.Set disable_nonlin_flag, "Disable Interpretation of nonlinear arithmetic";
        "-eot", Arg.String (fun str -> end_of_transmission := str), "Print string argument after each transmission";
	"-maxloops", Arg.Int (fun n -> maxloops_flag := n), "Run in server mode";
        "-server", Arg.Int (fun portnum -> portnum_flag := Some(portnum)), "Run in server mode";
      ]
      (fun f -> files := f :: !files)
      "usage: ics [-h] [-timings] [-prompt] [-pp] [-usage] [-eot <string>] [-server <portnum>] [files]";
  List.rev !files

let rec main () =
  try
    let l = args () in
      Rule.maxclose := !maxloops_flag;
      (match !portnum_flag with
	| None ->   
	    (match l with
	       | [] -> repl (Ics.channel_stdin ())
	       | l -> batch l)
	| Some(portnum) ->
	    server portnum);
      Ics.do_at_exit()
  with
      exc ->
	Ics.do_at_exit();
	raise exc

let _ = Printexc.catch main ()




