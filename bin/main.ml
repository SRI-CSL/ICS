(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

module Ics = Ics_

let _ = Sys.catch_break true

let usage () =
  Format.eprintf "\n%s" Version.version ;
  Format.eprintf
    "\nType 'help help.' for help about help, and 'Ctrl-d' to exit.@."

let set_gc_alarm () =
  let _ =
    Gc.create_alarm (fun () ->
        Format.eprintf "\nICS: major collection...@." )
  in
  ()

let set_gc_mode str =
  let control = Gc.get () in
  match str with
  | "lazy" ->
      Gc.set
        {control with Gc.space_overhead= 10000; Gc.max_overhead= 1000000}
  | "eager" ->
      Gc.set {control with Gc.space_overhead= 10; Gc.max_overhead= 100}
  | str -> raise (Invalid_argument ("no such GC option " ^ str))

let set_gc_space_overhead overhead =
  let control = Gc.get () in
  Gc.set {control with Gc.space_overhead= overhead}

let set_gc_max_overhead overhead =
  let control = Gc.get () in
  Gc.set {control with Gc.max_overhead= overhead}

(** {6 Arguments} *)

let timing_flag = ref false
let smt_flag = ref false
let smt_incomplete_flag = ref false
let config_flag = ref false
let prompt = ref "ics>"

let args () =
  let files = ref [] in
  Arg.parse
    [ ( "-version"
      , Arg.Unit
          (fun () ->
            Format.eprintf "%s@." Version.version ;
            exit 0 )
      , "Display version number." )
    ; ( "-build-info"
      , Arg.Unit
          (fun () ->
            Format.eprintf "%s@." Version.build_info ;
            exit 0 )
      , "Display build info." )
    ; ( "-footprint"
      , Arg.Unit (fun () -> Ics.footprint := true)
      , "Print progress in debugging mode(Default: false)." )
    ; ( "-explain"
      , Arg.Unit (fun () -> Ics.unsat_cores := true)
      , "Compute unsatisfiable cores." )
    ; ( "-prompt"
      , Arg.String (fun s -> prompt := s)
      , "Set interactive prompt (Default: ics>)." )
    ; ( "-gc_alarm"
      , Arg.Unit set_gc_alarm
      , "Output message when garbage collecting." )
    ; ( "-timing"
      , Arg.Unit (fun () -> timing_flag := true)
      , "Output timings (Default: false)" )
    ; ("-smt", Arg.Unit (fun () -> smt_flag := true), "Process SMT format")
    ; ( "-smtIncomplete"
      , Arg.Unit
          (fun () ->
            smt_flag := true ;
            smt_incomplete_flag := true )
      , "Process SMT format but no implicant computation" )
    ; ( "-config"
      , Arg.Unit (fun () -> config_flag := true)
      , "Print final configuration in batch mode (Default: false)." )
    ; ( "-doMinimize"
      , Arg.Unit (fun () -> Ics.do_minimize := true)
      , "Enable minimization when constructing arithmetic inequality \
         constraints (Default: false)." )
    ; ( "-gc_space_overhead"
      , Arg.Int set_gc_space_overhead
      , "GC will work more if [space_overhead] is smaller (default 80)" )
    ; ( "-gc_max_overhead"
      , Arg.Int set_gc_max_overhead
      , "Controlling heap compaction (default 500), [gc_max_overhead >= \
         1000000] disables compaction" )
    ; ( "-gc"
      , Arg.String set_gc_mode
      , "Coarse-grained control over GC (lazy, eager)" ) ]
    (fun f -> files := f :: !files)
    "Usage: ics [args] <file> ... <file>" ;
  List.rev !files

let eval_cmd inch =
  try Parser.command Lexer.token (Lexing.from_channel inch) with
  | Parsing.Parse_error ->
      Format.fprintf Format.std_formatter ":parse_error"
  | End_of_file -> exit 0
  | Sys.Break -> exit 1

let repl inch =
  usage () ;
  while true do
    Format.fprintf Format.std_formatter "\n%s @?" !prompt ;
    try eval_cmd inch
    with exc ->
      Format.fprintf Format.std_formatter ":exception %s@."
        (Printexc.to_string exc)
  done

let process_batch inch =
  let start = (Unix.times ()).Unix.tms_utime in
  repl inch ;
  (Unix.times ()).Unix.tms_utime -. start

let batch name =
  Format.printf "\nBatch Input: %s@?" name ;
  try
    let inch = Stdlib.open_in name in
    let time = process_batch inch in
    Format.printf "\n Status: @?" ;
    if !timing_flag then
      Format.printf "\n%s processed in %f seconds.@?" name time ;
    if !config_flag then (
      Format.printf "\nFinal configuration: \n" ;
      Ics.pp_current Format.std_formatter () )
  with exc ->
    let msg = Printexc.to_string exc in
    Format.printf ":error %s@." msg

let smt_process inch =
  let start = (Unix.times ()).Unix.tms_utime in
  SmtBench.Fill.reset () ;
  SmtLexer.linenumber := 0 ;
  SmtParser.benchmark SmtLexer.token (Lexing.from_channel inch) ;
  let b = SmtBench.Fill.finalize () in
  let status = SmtBench.decide !smt_incomplete_flag b in
  let time = (Unix.times ()).Unix.tms_utime -. start in
  (status, time)

let smt name =
  Format.eprintf "\nSMT Batch Input: %s@?" name ;
  try
    let inch = Stdlib.open_in name in
    let status, time = smt_process inch in
    Format.printf "\nStatus: " ;
    SmtBench.Ast.pp_status Format.std_formatter status ;
    if !timing_flag then
      Format.printf "\n%s processed in %f seconds.@?" name time ;
    if !config_flag then (
      Format.printf "\nFinal configuration: \n" ;
      Ics.pp_current Format.std_formatter () ) ;
    Format.eprintf "\n@?"
  with
  | Parsing.Parse_error ->
      Format.fprintf Format.std_formatter ":parserError on linenumber %d@?"
        !SmtLexer.linenumber
  | exc ->
      let msg = Printexc.to_string exc in
      Format.fprintf Format.std_formatter ":error %s@?" msg

let main () =
  match args () with
  | [] -> repl stdin
  | l -> if !smt_flag then List.iter smt l else List.iter batch l

let _ = Printexc.catch main ()
