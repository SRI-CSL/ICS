
(** Automatic C stub code generation for an ocaml library. *)

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

let printf x = fprintf !cout x

(** [iter_lines] iters the function [f] over all the lines of file [file]. *)
let iter_lines f file =
  let c = open_in file in
  try
    while true do f (input_line c) done
  with 
      End_of_file -> close_in c


(** Types are read from the \texttt{.mli} file and stored in the following table. *)
module Smap = Map.Make(struct type t = string let compare = compare end)

let ttable = ref Smap.empty

let declaration = regexp "val[ \t]+\([^ ]*\)[ \t]*:[ \t]*\(.*\)"

let scan_mli_file file =
  iter_lines
    (fun s -> 
       if string_match declaration s 0 then
	 ttable := Smap.add (matched_group 1 s) (matched_group 2 s) !ttable)
    (file ^ ".mli")

(** Types conversions. *)

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
  | [] -> ()
  | [Unit] -> ()
  | args -> 
      iter_args 
      (fun i a -> output_c_type c abs a; fprintf c " x%d" i)
      (fun () -> fprintf c ", ")
      args

(** Output the C prototype of the function. *)
let output_proto ch f typ mltyp =
  output_c_type ch "value*" mltyp.result;
  fprintf ch " %s_%s(" !base f;
  output_c_dargs ch "value*" mltyp.args;
  fprintf ch ")"

(** Output a fake return at the end of the stub code (after [ics_error]) *)
let output_fake_return ch indirect mltyp =
  fprintf ch "  return (";
  if indirect then 
    output_c_type ch "value*" mltyp.result 
  else 
    ();
  fprintf ch ")0;\n"

(** Output the header file. *)
let output_header f typ mltyp =
  output_proto !hout f typ mltyp;
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

let output_c_stub f typ mltyp = 
  output_proto !cout f typ mltyp;
  cprintf " {\n";
  cprintf "  value* r = malloc(sizeof(value));\n";
  cprintf "  register_global_root(r);\n";
  cprintf "  *r = 1;\n";
  cprintf "  set_ocaml_handlers();\n";
  cprintf "  *r = callback%s_exn(*%s_%s_rv," (arity f mltyp) !base f;
  output_c_args true mltyp.args;
  cprintf ");\n"; 
  cprintf "  restore_lisp_handlers();\n";
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

(** Constructors. *)
let constructors () =
  hprintf "\nvalue* %s_void();\n" !base;
  hprintf "\nvalue* %s_false();\n" !base;
  hprintf "\nvalue* %s_true();\n" !base;
  hprintf "\nvalue* %s_nil();\n" !base;

  cprintf 
    "\nvalue* %s_void()  { value* r = malloc(sizeof(value)); register_global_root(r); *r = Val_int(0); return r; }\n" !base;
  cprintf 
    "\nvalue* %s_false() { value* r = malloc(sizeof(value)); register_global_root(r); *r = Val_int(0); return r; }\n" !base;
  cprintf 
    "\nvalue* %s_true()  { value* r = malloc(sizeof(value)); register_global_root(r); *r = Val_int(1); return r; }\n" !base;
  cprintf "\nvoid %s_deregister(value* r) {\n" !base;
  cprintf "  if (((*r) & 1) == 0) { remove_global_root(r); }\n}\n";
  
  cprintf "\n/* The following is extracted from register_global_root in ocaml/byterun/globroots.c */\n";
  cprintf "\nint %s_registered_value_p (value *r)\n" !base;
  cprintf "{\n";
  cprintf "  struct global_root * e, * f;\n";
  cprintf "  int i;\n\n";
  cprintf "  /* Init \"cursor\" to list head */\n";
  cprintf "  e = (struct global_root *) &caml_global_roots;\n";
  cprintf "  /* Find value */\n";
  cprintf "  for (i = caml_global_roots.level; i >= 0; i--) {\n";
  cprintf "    while (1) {\n";
  cprintf "      f = e->forward[i];\n";
  cprintf "      if (f == NULL || f->root >= r) break;\n";
  cprintf "      e = f;\n";
  cprintf "    }\n";
  cprintf "  }\n";
  cprintf "  e = e->forward[0];\n";
  cprintf "  /* If already present, don't do anything */\n";
  cprintf "  return (e != NULL && e->root == r) ? 1 : 0;\n";
  cprintf "}\n";
  
  (* "" *)

  lprintf "\n(ff:def-foreign-call %s_void  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_false ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_true  ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_nil   ())\n" !base;
  lprintf "\n(ff:def-foreign-call %s_deregister (v) :returning :void)\n" !base;
  lprintf "\n(ff:def-foreign-call %s_registered_value_p (v))\n" !base




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
  cprintf "/* THIS FILE IS AUTOMATICALLY GENERATED FROM $ICSHOME/src/ics.mli */\n";
  cprintf "\n#include <stdio.h>\n#include <string.h>\n";
  cprintf "#include <caml/mlvalues.h>\n#include <caml/alloc.h>\n";
  cprintf "#include <caml/callback.h>\n#include <caml/memory.h>\n";
  cprintf "\n/* The following is from globroots.h which is not put into the ocaml lib dir */\n\n";
  cprintf "struct global_root {\n";
  cprintf "  value * root;                    /* the address of the root */\n";
  cprintf "  struct global_root * forward[1]; /* variable-length array */\n";
  cprintf "};\n\n";
  cprintf "#define MAX_LEVEL 15\n\n";
  cprintf "struct global_root_list {\n";
  cprintf "  value * root;                 /* dummy value for layout compatibility */\n";
  cprintf "  struct global_root * forward[MAX_LEVEL]; /* forward chaining */\n";
  cprintf "  int level;                    /* max used level */\n";
  cprintf "};\n\n";
  cprintf "extern struct global_root_list caml_global_roots;\n\n";
  cprintf "/* End of globroots.h */\n\n";
  cprintf "extern void set_ocaml_handlers(void);\n";
  cprintf "extern void restore_lisp_handlers(void);\n";
  cprintf "extern void ics_error(char *, char *);\n";
  cprintf "\n/* Values registered from Caml. */\n";
  lprintf ";;; ICS Interface description for C/C++\n";
  hprintf "/* THIS FILE IS AUTOMATICALLY GENERATED FROM ics.mli */\n";
  hprintf "\ntypedef long value;\n";
  lprintf ";;; Allegro Common Lisp Foreign Function Interface for ICS\n";
  lprintf ";;; THIS FILE IS AUTOMATICALLY GENERATED FROM ics.mli\n";
  lprintf "\n(make-package :ics)\n";
  lprintf "\n(in-package :ics)\n";
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
       output_lisp_stub rname typ mltyp;
       printf "%s added as %s with type %s\n" name rname typ)
    l;
  cprintf "\n/* Caml startup. */\n\n";
  cprintf "int caml_started_p = 0;\n";
  cprintf "void %s_caml_startup() {\n" !base;
  cprintf " char ** a;\n";
  cprintf " if (caml_started_p) {\n";
  cprintf "  fprintf(stdout, \"### Caml already started\\n\");\n";
  cprintf "  return;\n";
  cprintf " }\n";
  cprintf " a = malloc(2*sizeof(char **));\n";
  cprintf " a[0] = malloc(4);\n";
  cprintf " a[0][0] = 'i';\n";
  cprintf " a[0][1] = 'c';\n";
  cprintf " a[0][2] = 's';\n";
  cprintf " a[0][3] = NULL;\n";
  cprintf " a[1] = NULL;\n"; 
  cprintf " set_ocaml_handlers();\n";
  cprintf " caml_started_p = 1;\n";
  cprintf " fprintf(stderr, \"### Caml startup\\n\");\n";
  cprintf " fflush(stderr);\n";
  cprintf " caml_startup(a);\n";
  cprintf " fprintf(stderr, \"### successful\\n\");\n";
  cprintf " fflush(stderr);\n";
  List.iter
    (fun (rname,_,_,_) -> 
       cprintf "  %s_%s_rv = caml_named_value(\"%s\");\n" 
	 !base rname rname)
    l;
  cprintf " restore_lisp_handlers();\n";
  cprintf "}\n"
  

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
