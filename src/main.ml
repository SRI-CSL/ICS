
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*s Module [Main]: Toplevel of ICS command line interpreter. *)

(*i*)
open Ics
open Lexer
open Parser
open Tools
open Format
(*i*)
 
(*s Options. *)

let stat_flag = ref false
let timing_flag = ref false
let disable_prompt_flag = ref false
			    
(*s Exiting the system (on Ctrl-C or Ctrl-D) *)

let exit n =
  if !stat_flag then Tools.do_at_exit ();
  print_flush ();
  exit n

(*s Display usage and errors *)

let inconsistent () = printf "F\n@?"

let parse_error () = eprintf "Syntax Error\n@?"

let usage () =
  eprintf "ICS: Integrated Canonizer and Solver. Copyright (c) 2001 SRI International.\n";
  eprintf "\nType 'help.' for help about help, and 'Ctrl-d' to exit.\n\n@?"
    
(*s Batch mode. Read commands from files. *)

let process_channel c =
  let lb = Lexing.from_channel c in
  try
    while true do
      Parser.command Lexer.token lb
    done
  with
      End_of_file -> ()

let process_file f =
  let c = open_in f in
  try
    process_channel c; close_in c
  with
      e -> close_in c; raise e
    
let batch files =
  try
    List.iter process_file files; 
    exit 0
  with 
    | Exc.Inconsistent _   -> inconsistent (); exit 3
    | Parsing.Parse_error -> parse_error  (); exit 4

	
(*s Interactive toplevel. Read commands from standard input
     and evaluate them. *)

let prompt () =
  if not(!disable_prompt_flag) then printf "> @?"

let repl () =
  usage ();
  let lb = Lexing.from_channel stdin in
  try
    while true do
      prompt ();
      begin try
	Parser.command Lexer.token lb
      with 
	| Parsing.Parse_error -> parse_error  ()
	| Exc.Inconsistent _   -> inconsistent ()
      end;
      print_newline(); print_flush ()
    done
  with 
    | End_of_file    -> printf "\n@?"; exit 0
    | Failure "drop" ->	()

let args () =
  let files = ref [] in
  Arg.parse
      [ "-s", Arg.Set stat_flag,           "  Print statistics";
	"-t", Arg.Set timing_flag,         "  Print timings";
	"-v", Arg.Int set_verbose,         "  Verbose levels 0,1,2,...";
	"-p", Arg.Set disable_prompt_flag, "  Disable printing of prompt"
      ]
      (fun f -> files := f :: !files)
      "usage: ics [-stvp] [files]";
  List.rev !files

let main () =
  match args () with
    | [] -> repl ()
    | l -> batch l

let _ = Printexc.catch main ()











