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

open Ics

(** ICS command line interpreter. *)

let _ = Sys.catch_break true
 
(** {6 Options} *)

let stat_flag = ref false
let timing_flag = ref false
let disable_prompt_flag = ref false
let disable_usage_flag = ref false
let disable_pretty_print_flag = ref false
let end_of_transmission = ref ""
let disable_compactify_flag = ref false
let portnum_flag = ref None


(** {6 Interactive toplevel} *)

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
    Format.eprintf "\nics> @?"

and usage () =
  if not(!disable_usage_flag) then
    begin
      Format.eprintf "ICS: Integrated Canonizer and Solver.";
      Format.eprintf "\nCopyright (c) 2003 SRI International.";
      Format.eprintf "\nType 'help.' for help about help, and 'Ctrl-d' to exit.@."
    end


(** {6 Batch level processing} *)

and batch l =
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
      [ "-timings", Arg.Set timing_flag,          
	"Print timings";
	"-prompt", Arg.Set disable_prompt_flag,   
	"Disable printing of prompt";
        "-pp", Arg.Set disable_pretty_print_flag, 
	"Disable Pretty-Printing of terms";
	"-trace", Arg.String Ics.trace_add,
	"Trace level";
        "-usage", Arg.Set disable_usage_flag,     
	"Disable printing of usage message";
        "-compactify", Arg.Set disable_compactify_flag,
	"Use integer solver";
        "-integer-solve", Arg.Set(Context.integer_solve),  
	"Disable compactification";
        "-eot", Arg.String (fun str -> end_of_transmission := str), 
	"Print string argument after each transmission";
        "-server", Arg.Int (fun portnum -> portnum_flag := Some(portnum)), 
	"Run in server mode";
	"-verbose", Arg.Unit (fun () -> Prop.set_verbose true),
        "Verbose flag for SAT solver";
	"-remove_subsumed_clauses", Arg.Unit(fun () -> Prop.set_remove_subsumed_clauses true),
        "Removing subsumed clauses in SAT solver";
	"-validate_counter_example", Arg.Unit(fun () -> Prop.set_validate_counter_example true),
        "Check Boolean part of counterexample (for debugging)";
	"-polarity_optimization", Arg.Unit(fun () -> Prop.set_polarity_optimization true),
        "Optimizations for SAT solver based on polarities";
	"-clause_relevance", Arg.Int(Prop.set_clause_relevance),
        "Deletion of conflict clauses (default 50) in SAT";
        "-cleanup_period", Arg.Int(Prop.set_cleanup_period),
        "Garbage collection for SAT after number of conflicts (default 2000)";
        "-refinements", Arg.Int(Prop.set_num_refinements),
        "Number of refinement steps in SAT solver";
         "-statistics", Arg.Set(Prop.statistics),
        "Print statistics for SAT solver"
      ]
      (fun f -> files := f :: !files)
      "usage: ics [-h] [-timings] [-prompt] [-pp] [-usage] [-eot <string>] [-server <portnum>] [files]";
  List.rev !files

let rec main () =
  try
    let l = args () in
      (match !portnum_flag with
	| None ->   
	    (match l with
	       | [] -> repl (Ics.channel_stdin ())
	       | l -> batch l)
	| Some(portnum) ->
	    server portnum);
      Ics.do_at_exit();
      exit 0
  with
      exc ->
	Ics.do_at_exit();
	Format.eprintf "%s@." (Printexc.to_string exc);
	exit (-1)
	  
let _ = Printexc.catch main ()




