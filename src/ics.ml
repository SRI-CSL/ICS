
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
open Format
open Hashcons
open Sym
open Term
(*i*)

let init n =
  Trace.set_verbose n;
  Sys.catch_break true                 (*s raise [Sys.Break] exception upon *)
                                       (*s user interrupt. *)

let do_at_exit () = Tools.do_at_exit ()
let _ = Callback.register "do_at_exit" do_at_exit

let _ = Callback.register "init" init

(*s Channels. *)

type inchannel = in_channel
type outchannel = Format.formatter

let stdin () = Pervasives.stdin
let _ = Callback.register "stdin" stdin

let stdout () = Format.std_formatter
let _ = Callback.register "stdout" stdout

let stderr () = Format.err_formatter
let _ = Callback.register "stderr" stderr

let in_of_string = Pervasives.open_in
(* let _ = Callback.register "in_of_string" in_of_string *)

let out_of_string str = 
  Format.formatter_of_out_channel (Pervasives.open_out str)
let _ = Callback.register "out_of_string" out_of_string


(* Names. *)

type name = Name.t

let name_of_string = Name.of_string
let _ = Callback.register "name_of_string" name_of_string

let name_to_string = Name.to_string
let _ = Callback.register "name_to_string" name_to_string

let name_eq = Name.eq
let _ = Callback.register "name_eq" name_eq


(*s Constrains. *)

type cnstrnt = Cnstrnt.t

let cnstrnt_of_string s = 
  let lb = Lexing.from_string s in 
  Parser.cnstrnteof Lexer.token lb
let _ = Callback.register "cnstrnt_of_string" cnstrnt_of_string

let cnstrnt_input ch =
 let lb = Lexing.from_channel ch in 
  Parser.cnstrnteof Lexer.token lb
let _ = Callback.register "cnstrnt_input" cnstrnt_input

let cnstrnt_output = Cnstrnt.pp
let _ = Callback.register "cnstrnt_output" cnstrnt_output

let cnstrnt_pp = Cnstrnt.pp Format.std_formatter
let _ = Callback.register "cnstrnt_pp" cnstrnt_pp


let cnstrnt_mk_int () = Cnstrnt.mk_int
let _ = Callback.register "cnstrnt_mk_int" cnstrnt_mk_int

let cnstrnt_mk_nat () = Cnstrnt.mk_nat
let _ = Callback.register "cnstrnt_mk_nat" cnstrnt_mk_nat

let cnstrnt_mk_singleton = Cnstrnt.mk_singleton
let _ = Callback.register "cnstrnt_mk_singleton" cnstrnt_mk_singleton

let cnstrnt_mk_diseq = Cnstrnt.mk_diseq
let _ = Callback.register "cnstrnt_mk_diseq" cnstrnt_mk_diseq

let cnstrnt_mk_oo = Cnstrnt.mk_oo Dom.Real
let _ = Callback.register "cnstrnt_mk_oo" cnstrnt_mk_oo

let cnstrnt_mk_oc = Cnstrnt.mk_oc Dom.Real
let _ = Callback.register "cnstrnt_mk_oc" cnstrnt_mk_oc

let cnstrnt_mk_co = Cnstrnt.mk_co Dom.Real
let _ = Callback.register "cnstrnt_mk_co" cnstrnt_mk_co

let cnstrnt_mk_cc = Cnstrnt.mk_cc Dom.Real
let _ = Callback.register "cnstrnt_mk_cc" cnstrnt_mk_cc

let cnstrnt_mk_lt = Cnstrnt.mk_lt Dom.Real
let _ = Callback.register "cnstrnt_mk_lt" cnstrnt_mk_lt

let cnstrnt_mk_le = Cnstrnt.mk_le Dom.Real
let _ = Callback.register "cnstrnt_mk_le" cnstrnt_mk_le


let cnstrnt_inter = Cnstrnt.inter
let _ = Callback.register "cnstrnt_inter" cnstrnt_inter

let cnstrnt_add = Cnstrnt.add
let _ = Callback.register "cnstrnt_add" cnstrnt_add

let cnstrnt_multq = Cnstrnt.multq
let _ = Callback.register "cnstrnt_multq" cnstrnt_multq

let cnstrnt_mult = Cnstrnt.mult
let _ = Callback.register "cnstrnt_mult" cnstrnt_mult

let cnstrnt_div = Cnstrnt.div
let _ = Callback.register "cnstrnt_div" cnstrnt_div



(*s Terms ar either variables, uninterpreted applications,
  or interpreted applications including boolean terms. *)
  
type term = Term.t

let term_of_string s =
  let lb = Lexing.from_string s in 
  Parser.termeof Lexer.token lb
let _ = Callback.register "term_of_string" term_of_string

let term_input ch =
  let lb = Lexing.from_channel ch in 
  Parser.termeof Lexer.token lb
let _ = Callback.register "term_input" term_input

let term_output = Term.pp
let _ = Callback.register "term_output" term_output

let term_pp a = Term.pp Format.std_formatter a; Format.print_flush ()
let _ = Callback.register "term_pp" term_pp


(*s Construct a variable. *)

let term_mk_var str =
  let x = Name.of_string str in
  Term.mk_var x
let _ = Callback.register "term_mk_var" term_mk_var


(*s Uninterpred function application and function update. *)
         
let term_mk_uninterp x =
  let str = Name.of_string x in
  App.sigma (Sym.mk_uninterp str)
let _ = Callback.register "term_mk_uninterp" term_mk_uninterp

  
(*s Constructing arithmetic expressions. *)

let term_mk_num = Arith.mk_num
let _ = Callback.register "term_mk_num" term_mk_num

let term_mk_multq q = Arith.mk_multq q
let _ = Callback.register "term_mk_multq" term_mk_multq

let term_mk_add = Arith.mk_add
let _ = Callback.register "term_mk_add" term_mk_add

let term_mk_addl = Arith.mk_addl
let _ = Callback.register "term_mk_addl" term_mk_addl

let term_mk_sub  = Arith.mk_sub
let _ = Callback.register "term_mk_sub" term_mk_sub

let term_mk_unary_minus  = Arith.mk_neg
let _ = Callback.register "term_mk_unary_minus" term_mk_unary_minus

let term_is_arith = Arith.is_interp
let _ = Callback.register "iterm_s_arith" term_is_arith




(*s Constructing tuples and projections. *)

let term_mk_tuple = Tuple.mk_tuple
let _ = Callback.register "term_mk_tuple" term_mk_tuple

let term_mk_proj i j = Tuple.mk_proj i j
let _ = Callback.register "term_mk_proj" term_mk_proj


(*s Bitvector terms. *)

let term_mk_bvconst s = Bitvector.mk_const (Bitv.from_string s)
let _ = Callback.register "term_mk_bvconst" term_mk_bvconst

let term_mk_bvsub (n,i,j) = Bitvector.mk_sub n i j
let _ = Callback.register "term_mk_bvsub" term_mk_bvsub

let term_mk_bvconc (n,m) = Bitvector.mk_conc n m
let _ = Callback.register "term_mk_bvconc" term_mk_bvconc

let term_mk_bwite n (a,b,c) = Bitvector.mk_bitwise n a b c
let _ = Callback.register "term_mk_bwite" term_mk_bwite

let term_mk_bwand n a b =
  Bitvector.mk_bitwise n a b (Bitvector.mk_zero n)
let _ = Callback.register "term_mk_bwand" term_mk_bwand

let term_mk_bwor n a b =
  Bitvector.mk_bitwise n a (Bitvector.mk_one n) b
let _ = Callback.register "term_mk_bwor" term_mk_bwor

let term_mk_bwnot n a =
  Bitvector.mk_bitwise n a (Bitvector.mk_zero n) (Bitvector.mk_one n)
let _ = Callback.register "term_mk_bwnot" term_mk_bwnot

	  
(*s Boolean terms. *)

let term_mk_true () = Boolean.mk_true
let _ = Callback.register "term_mk_true" term_mk_true

let term_mk_false () = Boolean.mk_false
let _ = Callback.register "term_mk_false" term_mk_false

type atom = Atom.t
type atoms = Atom.Set.t

let atom_pp a = 
  Atom.pp Format.std_formatter a;
  Format.print_flush ()
let _ = Callback.register "atom_pp" atom_pp


let atom_mk_equal = Atom.mk_equal
let _ = Callback.register "atom_mk_equal" atom_mk_equal  

let atom_mk_diseq = Atom.mk_diseq
let _ = Callback.register "atom_mk_diseq" atom_mk_diseq

let atom_mk_in = Atom.mk_in 
let _ = Callback.register "atom_mk_in" atom_mk_in

let atom_mk_true () = Atom.mk_true
let _ = Callback.register "atom_mk_true" atom_mk_true

let atom_mk_false () = Atom.mk_false
let _ = Callback.register "atom_mk_false" atom_mk_false

let atom_mk_real = Atom.mk_in Cnstrnt.mk_real
let _ = Callback.register "atom_mk_real" atom_mk_real

let atom_mk_int = Atom.mk_in Cnstrnt.mk_int
let _ = Callback.register "atom_mk_int" atom_mk_int

let atom_mk_lt = Atom.mk_lt
let _ = Callback.register "atom_mk_lt"  atom_mk_lt

let atom_mk_le = Atom.mk_le
let _ = Callback.register "atom_mk_le"  atom_mk_le

let atom_mk_gt a b = Atom.mk_lt b a
let _ = Callback.register "atom_mk_gt" atom_mk_gt

let atom_mk_ge a b = Atom.mk_le b a
let _ = Callback.register "atom_mk_ge" atom_mk_ge

let term_is_true = Boolean.is_true
let _ = Callback.register "term_is_true" term_is_true

let term_is_false = Boolean.is_false
let _ = Callback.register "term_is_false" term_is_false



(*s Nonlinear terms. *)


let term_mk_mult = Arith.mk_mult
let _ = Callback.register "term_mk_mult" term_mk_mult

let term_mk_multl = Arith.mk_multl 
let _ = Callback.register "term_mk_multl" term_mk_multl

let term_mk_expt = Arith.mk_expt
let _ = Callback.register "term_mk_expt" term_mk_expt


(*s Builtin applications. *)

let term_mk_unsigned = Builtin.mk_unsigned
let _ = Callback.register "term_mk_unsigned" term_mk_unsigned

let term_mk_update = Builtin.mk_update
let _ = Callback.register "term_mk_update" term_mk_update

let term_mk_select = Builtin.mk_select
let _ = Callback.register "term_mk_select" term_mk_select



(*s Set of terms. *)

type terms = Term.Set.t

(* Term map. *)

type 'a map = 'a Term.Map.t
	  

(*s Equalities. *)

let term_eq = Term.eq
let _ = Callback.register "term_eq" term_eq

let term_cmp = Term.cmp
let _ = Callback.register "term_cmp" term_cmp

(*s Verbose level. *)

let set_verbose = Trace.set_verbose
let _ = Callback.register "set_verbose" set_verbose


(*s States. *)

open Shostak

type state = Shostak.t

let state_eq = (==)
let _ = Callback.register "state_eq" state_eq  

let state_empty () = Shostak.empty
let _ = Callback.register "state_empty" state_empty

let state_ctxt_of s = s.Shostak.ctxt
let _ = Callback.register "state_ctxt_of" state_ctxt_of

let state_diseqs_of s = D.deq_of s.Shostak.d
let _ = Callback.register "state_diseqs_of" state_diseqs_of

let state_cnstrnts_of s = failwith "to do"
let _ = Callback.register "state_cnstrnts_of" state_cnstrnts_of

let state_pp s = Shostak.pp Format.std_formatter s; Format.print_flush()
let _ = Callback.register "state_pp" state_pp
	  

(*s Processing of new equalities. *)

type status = Shostak.t Shostak.status

let is_consistent = function Shostak.Satisfiable _ -> true | _ -> false
let _ = Callback.register "is_consistent" is_consistent

let is_redundant r = (r = Shostak.Valid)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent r =
   (r = Shostak.Inconsistent)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent = function
  | Shostak.Satisfiable s -> s
  | _ -> failwith "Ics.d_consistent: fatal error"
	
let _ = Callback.register "d_consistent" d_consistent  

let process = Shostak.process 
let _ = Callback.register "process" process   



(*s Normalization functions *)

let can = Shostak.can
let _ = Callback.register "can" can

(*s Command interface. *)

type istate = Istate.t

let istate_current = Istate.current
let _ = Callback.register "istate_current" istate_current

let istate_symtab = Istate.symtab
(* let _ = Callback.register "istate_symtab" istate_symtab *)

let istate_def = Istate.def
let _ = Callback.register "istate_def" istate_def

let istate_sig = Istate.sgn
let _ = Callback.register "istate_sig" istate_sig

let istate_set_in_channel = Istate.set_inchannel 
let _ = Callback.register "istate_set_in_channel" istate_set_in_channel

let istate_set_out_channel = Istate.set_outchannel 
let _ = Callback.register "istate_set_out_channel" istate_set_out_channel

let istate_type = Istate.typ
let _ = Callback.register "istate_type" istate_type

let istate_flush = Istate.flush
let _ = Callback.register "istate_flush" istate_flush

let istate_nl = Istate.nl
let _ = Callback.register "istate_nl" istate_nl

let istate_can  = Istate.can
let _ = Callback.register "istate_can" istate_can

let istate_process = Istate.process
let _ = Callback.register "istate_process" istate_process

let istate_reset = Istate.reset
let _ = Callback.register "istate_reset" istate_reset

let istate_save = Istate.save
let _ = Callback.register "istate_save" istate_save

let istate_restore = Istate.restore
let _ = Callback.register "istate_restore" istate_restore

let istate_remove = Istate.remove
let _ = Callback.register "istate_remove" istate_remove

let istate_forget = Istate.forget
let _ = Callback.register "istate_forget" istate_forget

let istate_eval () =
  try 
    Parser.commands Lexer.token (Lexing.from_channel (Istate.inchannel()))
  with 
    | Invalid_argument str -> Format.eprintf "Error(%s).@." str
    | Parsing.Parse_error -> Format.eprintf "Error(Syntax).@."
    | End_of_file -> raise End_of_file
    | Sys.Break -> raise Sys.Break
    | exc -> Format.eprintf "Internal error.@."; raise exc

let _ = Callback.register "istate_eval" istate_eval


(*s Abstract sign interpretation. *)

let cnstrnt s a = Shostak.cnstrnt s a
 

(*s Tools *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush


(*s Lists. *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "is_nil" is_nil

let cons x l = x :: l
let _ = Callback.register "cons" cons

let head = List.hd
let _ = Callback.register "head" head

let tail = List.tl
let _ = Callback.register "tail" tail


(*s Pairs. *)

let pair x y = (x,y)
let _ = Callback.register "pair" pair

let fst = fst
let _ = Callback.register "fst" fst

let snd = snd
let _ = Callback.register "snd" snd


(*s Triples. *)

let triple x y z = (x,y,z)
let _ = Callback.register "triple" triple

let fst_of_triple = function (x,_,_) -> x
let _ = Callback.register "fst_of_triple" fst_of_triple

let snd_of_triple = function (_,y,_) -> y
let _ = Callback.register "snd_of_triple" snd_of_triple

let third_of_triple = function (_,_,z) -> z
let _ = Callback.register "third_of_triple" third_of_triple
  

(*s Quadruples. *)
   
let fst_of_quadruple  = function (x1,_,_,_) -> x1
let _ = Callback.register "fst_of_quadruple" fst_of_quadruple

let snd_of_quadruple = function (_,x2,_,_) -> x2
let _ = Callback.register "snd_of_quadruple" snd_of_quadruple

let third_of_quadruple = function (_,_,x3,_) -> x3
let _ = Callback.register "third_of_quadruple" third_of_quadruple

let fourth_of_quadruple = function (_,_,_,x4) -> x4
let _ = Callback.register "fourth_of_quadruple" fourth_of_quadruple
    

(*s Options. *)

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | None -> true
  | _ -> false

let value_of = function
  | Some(x) -> x
  | _ -> assert false
	
let _ = Callback.register "is_some" is_some
let _ = Callback.register "is_none" is_none
let _ = Callback.register "value_of" value_of
	  
	 
(*s Multi-precision arithmetic.*)

open Mpa

type q = Q.t

let ints_of_num q = (Z.to_string (Q.numerator q), Z.to_string (Q.denominator q))
let _ = Callback.register "ints_of_num" ints_of_num

let num_of_int = Q.of_int
let _ = Callback.register "num_of_int" num_of_int
		   
let num_of_ints i1 i2 = Q.div (num_of_int i1) (num_of_int i2)
let _ = Callback.register "num_of_ints" num_of_ints

let string_of_num = Q.to_string
let _ = Callback.register "string_of_num" string_of_num

let num_of_string = Q.of_string
let _ = Callback.register "num_of_string" num_of_string

