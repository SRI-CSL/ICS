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

let version = "ICS 2.0 (Experimental, Wed Sep 17 13:54:25 PDT 2003)" 

(** ICS command line interpreter. *)

let _ = Sys.catch_break true
 

(** {6 Arguments} *)

let timing_flag = ref false
let portnum_flag = ref None

let args () =
  let files = ref [] in
  let set_true set = Arg.Unit (fun () -> set true)
  and set_false set = Arg.Unit (fun () -> set false) in
    Arg.parse
      [ "-timings", Arg.Set timing_flag,          
	"Print timings";
	"-profiles", Arg.Set Tools.profiling,          
	"Print profiles";
	"-index", Arg.Set Eqs.pp_index,          
	"Print internal indices";
	"-prompt", Arg.String Ics.set_prompt,
	"Set prompt";
        "-pp", Arg.String Ics.set_pretty,
	"Pretty-printing mode ([mixfix | prefix | sexpr]) ";	
	"-nohelp", Arg.Clear Istate.help_enabled,
	"Disable help feature";
	"-trace", Arg.String Ics.trace_add,
	"Enable tracing";
        "-version", Arg.Unit (fun () -> Format.eprintf "%s@." version; exit 0),
        "Display version number";
        "-compactify",  set_true Ics.set_compactify,
	"Disable compactification in SAT solver";
        "-expensive_simplify", Arg.Clear Combine.cheap,
	"Expensive but more complete simplification";
	"-dependencies", Arg.Unit(fun () -> Ics.set_proofmode "dep"),
	"Enable dependency generation";
	"-proofmode", Arg.String(Ics.set_proofmode),
	"Set proofmode to [No | Dep]";
        "-eot", Arg.String Ics.set_eot, 
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
        "-refinements", Arg.Int(Ics.set_num_refinements),
        "Number of refinement steps in SAT solver";
        "-statistics", set_true Ics.set_statistic,
        "Print statistics for SAT solver";
	"-reduce_explanation", set_true Ics.set_reduce_explanation,
        "Try to further reduce explanations in SAT solving";
	"-integersolve", set_false Ics.set_integer_solve,
        "Disables Solving for the integers";
	"-crossmultiply", set_true Ics.set_crossmultiply,
        "Enables crossmultiplication";
	"-gc_space_overhead",  Arg.Int(Ics.set_gc_space_overhead),
        "GC will work more if [space_overhead] is smaller (default 80)";
	"-gc_max_overhead", Arg.Int(Ics.set_gc_max_overhead),
        "Controlling heap compaction (default 500), [gc_max_overhead >= 1000000] disables compaction";
	"-gc", Arg.String (Ics.set_gc_mode),
	"Coarse-grained control over GC (lazy, eager)"
      ]
      (fun f -> files := f :: !files)
      "Usage: ics [args] <file> ... <file>";
    List.rev !files


(** {6 Interactive Mode} *)

let rec repl () =
  usage ();
  Ics.cmd_rep ()

and usage () =
  begin
    Format.eprintf "%s: Integrated Canonizer and Solver." version;
    Format.eprintf "\nCopyright (c) 2003 SRI International.";
    Format.eprintf "\nType 'help help.' for help about help, and 'Ctrl-d' to exit.@."
  end


(** {6 Batch Mode} *)

and batch names =
  Ics.set_prompt "";
  List.iter batch1 names

and batch1 name =
  let inch = Ics.inchannel_of_string name in
  let exit_code = 
    if !timing_flag then
      let start = (Unix.times()).Unix.tms_utime in
      let code = Ics.cmd_batch (inch) in
      let time = (Unix.times()).Unix.tms_utime -. start in
	Format.eprintf "\n%s processed in %f seconds.@." name time;
	code
    else 
      Ics.cmd_batch (inch)
  in
    if exit_code <> 0 then
      exit exit_code
    

(** {6 Server Mode} *)

and server portnum = 
  let addr = Unix.inet_addr_any in
  let sockaddr = Unix.ADDR_INET (addr, portnum) in
  Unix.establish_server 
    (fun inch outch ->
       let formatter = Format.formatter_of_out_channel outch in
	 Ics.set_inchannel inch;
	 Ics.set_outchannel formatter;
	 Ics.cmd_rep ())
    sockaddr


let rec main () =
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
      exit 0
  with
      exc ->
	Format.eprintf "%s@." (Printexc.to_string exc);
	exit (-1)
	  
let _ = Printexc.catch main ()




