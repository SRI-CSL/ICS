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

(** ICS command line interpreter. *)

let _ = Sys.catch_break true

(** {6 Arguments} *)

let timing_flag = ref false
let portnum_flag = ref None


let args () =
  let files = ref [] in
  let set_true set = Arg.Unit (fun () -> set (Ics.tt()))
  and set_false set = Arg.Unit (fun () -> set (Ics.ff())) in
  let arg_string f =
    let f' str = f (Ics.intern str) in
      Arg.String f'
  in
    Arg.parse
      [ "-timings", Arg.Set timing_flag,          
	"Print timings";
	"-profiles", Arg.Set Tools.profiling,          
	"Print profiles";
(*
	"-prompt", arg_string Ics.set_prompt,
	"Set prompt";
        "-pp", arg_string Ics.set_pretty,
	"Pretty-printing mode ([mixfix | prefix | sexpr]) ";	
	"-nohelp", Arg.Clear Help.enabled,
	"Disable help feature";
	"-show_explanations", set_true Ics.set_show_explanations,
	"Display explanations generated for SAT solver on stderr";
        "-version", Arg.Unit (fun () -> Version.print(); exit 0),
        "Display version number";
        "-compactify",  set_true Ics.set_compactify,
	"Disable compactification in SAT solver";
	"-proofmode", arg_string Ics.set_proofmode,
	"Set proofmode to [No | Dep]";
        "-eot", arg_string Ics.set_eot, 
	"Print string argument after each transmission";
        "-server", Arg.Int (fun portnum -> portnum_flag := Some(portnum)), 
	"Run in server mode";
	"-verbose", set_true Ics.set_verbose,
        "Verbose flag for SAT solver";
	"-progress", Arg.Set Istate.progress,
        "Print progress in batch mode";
	"-remove_subsumed_clauses", set_true Ics.set_remove_subsumed_clauses,
        "Removing subsumed clauses in SAT solver";
	"-validate", set_true Ics.set_validate,
        "Validate SAT assignments and explanations (for debugging)";
	"-polarity_optimization", set_true Ics.set_polarity_optimization,
        "Optimizations for SAT solver based on polarities";
	"-clause_relevance", Arg.Int(Ics.set_clause_relevance),
        "Deletion of conflict clauses (default 50) in SAT";
        "-cleanup_period", Arg.Int(Ics.set_cleanup_period),
        "Garbage collection for SAT after number of conflicts (default 2000)";
	"-frequency", Arg.Int(Ics.set_assertion_frequency),
        "Minimal frequency of asserting ground atoms in SAT solver";
        "-statistics", set_true Ics.set_statistic,
        "Print statistics for SAT solver";
	"-integersolve", set_false Ics.set_integer_solve,
        "Disables Solving for the integers (incomplete)";
	"-gc_space_overhead",  Arg.Int(Ics.set_gc_space_overhead),
        "GC will work more if [space_overhead] is smaller (default 80)";
	"-gc_max_overhead", Arg.Int(Ics.set_gc_max_overhead),
        "Controlling heap compaction (default 500), [gc_max_overhead >= 1000000] disables compaction";
	"-gc", arg_string (Ics.set_gc_mode),
	"Coarse-grained control over GC (lazy, eager)"
*)
      ]
      (fun f -> files := f :: !files)
      "Usage: ics [args] <file> ... <file>";
    List.rev !files


(** {6 Interactive Mode} *)

let rec repl () =
  usage ();
  try
    Ics.cmd_rep ()
  with
    | Failure "drop" ->
	()
    | exc -> 
	Format.eprintf "%s@." (Printexc.to_string exc);
	Ics.cmd_rep ()

and usage () =
  begin
    Version.eprint (); 
    Format.eprintf "\nType 'help help.' for help about help, and 'Ctrl-d' to exit.@."
  end


(** {6 Batch Mode} *)

and batch names =
  let empty = Ics.intern "" in
    Ics.set (Ics.intern "prompt") empty;
    List.iter batch1 names

and batch1 name =
  Ics.set (Ics.intern "inchannel") (Ics.intern name);
  let exit_code = 
    if !timing_flag then
      let start = (Unix.times()).Unix.tms_utime in
      let code = Ics.cmd_batch () in
      let time = (Unix.times()).Unix.tms_utime -. start in
	Format.eprintf "\n%s processed in %f seconds.@." name time;
	code
    else 
      Ics.cmd_batch ()
  in
    if exit_code <> 0 then
      begin
	Tools.do_at_exit();
	exit exit_code
      end 
    

(** {6 Server Mode} *)

and server portnum = 
  let addr = Unix.inet_addr_any in
  let sockaddr = Unix.ADDR_INET (addr, portnum) in
  Unix.establish_server 
    (fun inch outch ->
       let formatter = Format.formatter_of_out_channel outch in
(* to do
	 Ics.set_inchannel inch;
	 Ics.set_outchannel formatter;
*)
	 Ics.cmd_rep ())
    sockaddr

let rec main () =
  Ics.init(0);
  try
    let l = args () in
      (match !portnum_flag with
	 | None ->   
	     (match l with
		| [] -> repl ()
		| l -> batch l)
	 | Some(portnum) ->
	     server portnum);
      if !Tools.profiling then                  (* print gc statistics *)
	begin
	  Format.eprintf "\nGC statistics:\n";
	  Gc.print_stat stderr
	end;
      Format.eprintf "@.";
      Tools.do_at_exit();
      exit 0
  with
      exc ->
	Format.eprintf "%s@." (Printexc.to_string exc);
	Tools.do_at_exit();
	exit (-1)
	  
let _ = Printexc.catch main ()






