
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
 * Author: Jean-Christophe Filliatre
 i*)

(*s {\bf chameleon.} Automatic C stub code generation for an ocaml library. *)

open Filename
open Printf
open Str
open Mli_types
open Mli_lexer

(*s Globals. *)

let base = ref ""
let cout = ref stdout
let hout = ref stdout
let lout = ref stdout

let cprintf x = fprintf !cout x
let hprintf x = fprintf !hout x
let lprintf x = fprintf !lout x

(*s [iter_lines] iters the function [f] over all the lines of file [file]. *)

let iter_lines f file =
  let c = open_in file in
  try
    while true do f (input_line c) done
  with End_of_file ->
    close_in c

(*s The callbacks already contained in the C file are stored in the following
    table. *)

module Sset = Set.Make(struct type t = string let compare = compare end)

let ctable = ref Sset.empty

let scan_c_file file =
  ctable := Sset.empty;
  let cfile = file ^ ".c" in
  if Sys.file_exists cfile then begin
    let already = regexp ("value[ \t\*]" ^ !base ^ "_\([^ ]\).*") in
    iter_lines 
      (fun s -> 
	 if string_match already s 0 then 
	   let f = matched_group 1 s in
	   ctable := Sset.add f !ctable) 
      cfile
  end

(*s Types are read from the \texttt{.mli} file and stored in the following
    table. *)

module Smap = Map.Make(struct type t = string let compare = compare end)

let ttable = ref Smap.empty

let declaration = regexp "val[ \t]+\([^ ]*\)[ \t]*:[ \t]*\(.*\)"

let scan_mli_file file =
  iter_lines
    (fun s -> 
       if string_match declaration s 0 then
	 ttable := Smap.add (matched_group 1 s) (matched_group 2 s) !ttable)
    (file ^ ".mli")

(*s Types conversions. *)

let output_c_type c abs = function
  | Int -> fprintf c "int"
  | Bool -> fprintf c "int"
  | Unit -> fprintf c "void"
  | String -> fprintf c "char*"
  | Abstract -> fprintf c abs

let iter_args f sep l =
  let rec iter i = function
    | [] -> ()
    | [a] -> f i a
    | a::args -> f i a; sep(); iter (succ i) args
  in
  iter 1 l

let output_c_dargs c abs = function
  | [] | [Unit] -> ()
  | args -> 
      iter_args 
	(fun i a -> output_c_type c abs a; fprintf c " x%d" i)
	(fun () -> fprintf c ", ")
	args

(*s Output the C prototype of the function. *)

let output_proto ch indirect f typ mltyp =
  if indirect then begin
    output_c_type ch "value*" mltyp.result;
    fprintf ch " %s_%s(" !base f;
    output_c_dargs ch "value*" mltyp.args
  end else begin
    output_c_type ch "value" mltyp.result;
    fprintf ch " %s_direct_%s(" !base f;
    output_c_dargs ch "value" mltyp.args
  end;
  fprintf ch ")"

(*s Output a fake return at the end of the stub code (after [ics_error]) *)

let output_fake_return ch indirect mltyp =
  fprintf ch "  return (";
  if indirect then 
    output_c_type ch "value*" mltyp.result 
  else 
    output_c_type ch "value" mltyp.result;
  fprintf ch ")0;\n"

(*s Output the header file. *)

let output_header f typ mltyp =
  output_proto !hout false f typ mltyp;
  hprintf ";\n";
  output_proto !hout true f typ mltyp;
  hprintf ";\n"

(*s Output the C stub code. *)

let arity f mltyp = match List.length mltyp.args with
  | 0 -> ""
  | 1 -> ""
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | _ -> printf "%s has too many arguments\n" f; exit 1

let c_to_ml x = function
  | Int -> cprintf "Val_int(%s)" x
  | Unit -> cprintf "Val_unit"
  | Bool -> cprintf "Val_bool(%s)" x
  | String -> cprintf "copy_string(%s)" x
  | Abstract -> output_string !cout x

let ml_to_c k = function
  | Int -> cprintf "Int_val("; k(); cprintf ")"
  | Unit -> assert false
  | Bool -> cprintf "Bool_val("; k(); cprintf ")"
  | String -> cprintf "strdup(String_val("; k(); cprintf "))"
  | Abstract -> k()

let output_c_args indirect = function
  | [] -> cprintf "Val_unit"
  | args -> 
      iter_args 
	(fun i a -> 
	   if indirect && a = Abstract then cprintf "*";
	   c_to_ml ("x" ^ string_of_int i) a) 
	(fun () -> cprintf ",")
	args

let gc_params args =
  let rec filter i = function
    | [] -> []
    | Abstract :: l -> i :: (filter (succ i) l)
    | _ :: l -> filter (succ i) l
  in
  let args' = filter 1 args in
  let n = List.length args' in
  cprintf "  CAMLparam%d(" n;
  if n > 0 then
    iter_args (fun _ i -> cprintf "x%d" i) (fun () -> cprintf ",") args';
  cprintf ");\n"

let output_c_stub f typ mltyp = 
  output_proto !cout false f typ mltyp;
  cprintf " {\n";
  gc_params mltyp.args;
  cprintf "  CAMLlocal1(r);\n";                       (* return value *)
(*  cprintf "  extern int new_handler();\n";            (* declaration of handler for breaks *) *)
(* cprintf "  if ((r = setjmp(catch)) != 0) return (value*) r;\n"; *)
(* cprintf "  old_handler = signal(SIGINT, new_handler);\n"; (* install new handler *)   *)  
  cprintf "  r = callback%s_exn(*%s_%s_rv," (arity f mltyp) !base f;
  (* "" *)
  output_c_args false mltyp.args;
  cprintf ");\n";
(*  cprintf "  signal(SIGINT, old_handler);\n"; *)
  cprintf "  if (!Is_exception_result(r)) { ";
  if mltyp.result <> Unit then begin
    cprintf "CAMLreturn(";
    ml_to_c (fun () -> cprintf "r") mltyp.result;
    cprintf ")"
  end else
    cprintf "CAMLreturn0";
  cprintf "; };\n";
  cprintf 
    "  ics_error(\"%s_%s\",format_caml_exception(Extract_exception(r)));\n" !base f;
  output_fake_return !cout false mltyp;
  cprintf "}\n"

let output_c_indirect_stub f typ mltyp = 
  output_proto !cout true f typ mltyp;
  cprintf " {\n";
  cprintf "  value* r = malloc(sizeof(value));\n";
  cprintf "  register_global_root(r);\n";
  cprintf "  *r = 1;\n";
  cprintf "  *r = callback%s_exn(*%s_%s_rv," (arity f mltyp) !base f;
  (* "" *)
  output_c_args true mltyp.args;
  cprintf ");\n";
  (****TODO: RETABLIR???
  cprintf "  if (((*r) & 1) == 1) remove_global_root(r);\n";
  *****)
  cprintf "  if (!Is_exception_result(*r)) {";
  (* "" *)
  begin match mltyp.result with
    | Unit -> 
	cprintf " return; "
    | Abstract -> 
	cprintf " return r; "
    | t -> 
	cprintf " return ";
	ml_to_c (fun () -> cprintf "*r") t; 
	cprintf "; "
  end;
  cprintf "};\n";
  cprintf 
    "  ics_error(\"%s_%s\",format_caml_exception(Extract_exception(*r)));\n"
    !base f;
  output_fake_return !cout true mltyp;
  cprintf "}\n"

(*s Constructors. *)

let constructors () =
  hprintf "\nvalue %s_direct_void();" !base;
  hprintf "\nvalue* %s_void();\n" !base;
  hprintf "\nvalue %s_direct_false();" !base;
  hprintf "\nvalue* %s_false();\n" !base;
  hprintf "\nvalue %s_direct_true();" !base;
  hprintf "\nvalue* %s_true();\n" !base;
  hprintf "\nvalue %s_direct_nil();" !base;
  hprintf "\nvalue* %s_nil();\n" !base;

  cprintf "\nvalue %s_direct_void()  { return Val_int(0); }" !base;
  cprintf 
    "\nvalue* %s_void()  { static value r = Val_int(0); return &r; }\n" !base;
  cprintf "\nvalue %s_direct_false() { return Val_int(0); }" !base;
  cprintf 
    "\nvalue* %s_false() { static value r = Val_int(0); return &r; }\n" !base;
  cprintf "\nvalue %s_direct_true()  { return Val_int(1); }" !base;
  cprintf 
    "\nvalue* %s_true()  { static value r = Val_int(1); return &r; }\n" !base;
  cprintf "\nvalue %s_direct_nil()   { return Val_int(0); }" !base;
  cprintf 
    "\nvalue* %s_nil()   { static value r = Val_int(0); return &r; }\n" !base;

(*  cprintf 
    "\nint new_handler() { signal(SIGINT,old_handler); longjmp(catch,-1); }\n"; *)

  cprintf "\nvoid %s_deregister(value* r) {\n" !base;
  cprintf "  if (((*r) & 1) == 0) { remove_global_root(r); }\n}\n";
  (* "" *)

  lprintf "\n(ff:def-foreign-call %s_sleep_wrapper  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_process_wrapper  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_void  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_false ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_true  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_nil   ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_deregister (v) :returning :void)\n" !base

(*s Outout the Lisp stub code. *)

let lisp_typed_arg = function
  | Abstract | Int | Bool -> false
  | _ -> true

let lisp_typed_result = function
  | Unit -> true
  | _ -> false

let lisp_type = function
  | Int      -> ":int"
  | Bool     -> ":bool"
  | String   -> "(* :char)" (* "" *)
  | Unit     -> ":void"
  | Abstract -> ":int fixnum"

let output_lisp_stub f typ mltyp =
  lprintf "\n; %s : %s\n" f typ;
  lprintf "(ff:def-foreign-call %s_%s (" !base f;
  begin match mltyp.args with
    | [] | [Unit] -> lprintf ") "
    | args -> 
	let n = List.length args in
	iter_args
	  (fun i a -> 
	     if lisp_typed_arg a then
	       lprintf "(x%d %s)" i (lisp_type a)
	     else
	       lprintf "x%d" i) 
	  (fun () -> lprintf " ")
	  args;
	lprintf ")"
  end;
  if lisp_typed_result mltyp.result then
    lprintf " :returning %s" (lisp_type mltyp.result);
  lprintf ")\n"

(*s Process the callback corresponding to line [s]. *)

let output_code l =
  cprintf "/* THIS FILE IS AUTOMATICALLY GENERATED */\n";
  cprintf "\n#include <stdio.h>\n#include <string.h>\n";
  cprintf "#include <caml/mlvalues.h>\n#include <caml/alloc.h>\n";
  cprintf "#include <caml/callback.h>\n#include <caml/memory.h>\n";
  cprintf "#include <setjmp.h>\n";
  cprintf "#include <signal.h>\n";
  cprintf "extern void ics_error(char *, char *);";
  cprintf "jmp_buf catch;\n";
  cprintf "void (*old_handler)(int);\n";
  cprintf "\n/* Values registered from Caml. */\n";
  hprintf "/* THIS FILE IS AUTOMATICALLY GENERATED */\n";
  hprintf "\ntypedef long value;\n";
  lprintf ";;; THIS FILE IS AUTOMATICALLY GENERATED\n";
  lprintf "\n(in-package :pvs)\n";
  lprintf 
    "\n(ff:def-foreign-call %s_caml_startup ()) \n" 
    !base;
  List.iter 
    (fun (rname,_,_,_) -> cprintf "value* %s_%s_rv;\n" !base rname)
    l;
  constructors();
  List.iter 
    (fun (rname,name,typ,mltyp) ->
       hprintf "\n/* %s : %s */\n" rname typ;
       output_header rname typ mltyp;
       cprintf "\n/* %s : %s */\n" rname typ;
       output_c_stub rname typ mltyp;
       output_c_indirect_stub rname typ mltyp;
       output_lisp_stub rname typ mltyp;
       printf "%s added as %s with type %s\n" name rname typ)
    l;
  cprintf "\n/* Caml startup. */\n\n";
  cprintf "void %s_caml_startup() {\n" !base;
  cprintf " char ** a;\n";
  cprintf " a = malloc(2*sizeof(char **));\n";
  cprintf " a[0] = malloc(4);\n";
  cprintf " a[0][0] = 'i';\n";
  cprintf " a[0][1] = 'c';\n";
  cprintf " a[0][2] = 's';\n";
  cprintf " a[0][3] = NULL;\n";
  cprintf " a[1] = NULL;\n";
  cprintf "  fprintf(stdout, \"### Caml startup\\n\");\n";
  cprintf "  fflush(stdout);\n";
  cprintf "  caml_startup(a);\n";
  cprintf "  fprintf(stdout, \"### successful\\n\");\n";
  cprintf "  fflush(stdout);\n";
  List.iter
    (fun (rname,_,_,_) -> 
       cprintf "  %s_%s_rv = caml_named_value(\"%s\");\n" 
	 !base rname rname)
    l;
  cprintf "}\n";
  cprintf "/* New interrupt handler. */\n";
  cprintf "void new_handler (int foo) {\n";
  cprintf "  signal(SIGINT, old_handler);";
  cprintf "  longjmp(catch, -1);\n";
  cprintf "}\n\n";
  cprintf "/* Wrappers for interrupt handlers. */\n";
  cprintf "value* ics_process_wrapper (value* x1, value* x2) {\n";
  cprintf "   auto value* return_value;\n";
  cprintf "   if ((setjmp(catch)) != 0) return (value*) NULL;\n";
  cprintf "   old_handler = signal(SIGINT,new_handler);\n"; 
  cprintf "   return_value = ics_process(x1,x2);\n";
  cprintf "   signal(SIGINT,old_handler);\n";
  cprintf "   return return_value;\n";
  cprintf "}\n\n";
  cprintf "/* Test function for interrupts. */\n";
  cprintf "void ics_sleep_wrapper (value* n) {\n";
  cprintf "   auto value* return_value;\n";
  cprintf "   if ((setjmp(catch)) == 0) { \n";
  cprintf "      old_handler = signal(SIGINT,new_handler);\n";
  cprintf "      ics_sleep(n);\n";
  cprintf "      signal(SIGINT,old_handler);\n";
  cprintf "}}\n\n"


  

(*s Process a file: for each line, we call [process_callback] as soon as the
    line looks like "let _ = Callback.register ..." *)

let register = 
  regexp "let _ = Callback\.register \"\([^\"]*\)\"[ \t]*\([^ ]*\).*"

let scan_ml_file file =
  let l = ref [] in
  let add s = 
    let rname = matched_group 1 s in
    let name = matched_group 2 s in
    let typ = 
      try  Smap.find name !ttable 
      with Not_found -> printf "%s not declared in interface\n" name; exit 1
    in
    let mltyp = 
      try  mltyp_of_string typ
      with Parsing.Parse_error -> printf "parse error for %s\n" name; exit 1
    in
    l := (rname,name,typ,mltyp) :: !l
  in
  iter_lines 
    (fun s -> if string_match register s 0 then add s)
    (file ^ ".ml");
  List.rev !l

let main () =
  let file = chop_extension Sys.argv.(1) in
  scan_mli_file file;
  let l = scan_ml_file file in
  base := basename file;
  cout := open_out (file ^ "_stub.c");
  hout := open_out (file ^ ".h");
  lout := open_out (file ^ ".lisp");
  output_code l;
  close_out !cout;
  close_out !hout;
  close_out !lout

let _ = Printexc.catch main ()
