
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

let init (n) =
  Trace.set_verbose n;
  Sys.catch_break true                 (*s raise [Sys.Break] exception upon *)
                                       (*s user interrupt. *)

let do_at_exit () = Tools.do_at_exit ()
let _ = Callback.register "do_at_exit" do_at_exit


let _ = Callback.register "init" init

(* Three-valued logic. *)

type three = Three.t

let three_yes = Three.Yes
let three_no = Three.No
let three_x = Three.X

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

(*s Interval. *)

type dom = Dom.t

let dom_int = Dom.Int
let _ = Callback.register "dom_int" dom_int

let dom_real = Dom.Real
let _ = Callback.register "dom_real" dom_real

type endpoint = Interval.endpoint

let endpoint_posinf = Interval.posinf
let _ = Callback.register "endpoint_posinf" endpoint_posinf

let endpoint_neginf = Interval.neginf
let _ = Callback.register "endpoint_neginf" endpoint_neginf

let endpoint_open = Interval.strict
let _ = Callback.register "endpoint_open" endpoint_open

let endpoint_closed = Interval.nonstrict
let _ = Callback.register "endpoint_closed" endpoint_closed

type interval = Interval.t
type intervals = Intervals.t

let interval_make = Interval.make Dom.Real
let _ = Callback.register "interval_make" interval_make

(* Names. *)

type name = Name.t

let name_of_string = Name.of_string
let _ = Callback.register "name_of_string" name_of_string

let name_to_string = Name.to_string
let _ = Callback.register "name_to_string" name_to_string

type names = Name.Set.t

let names_singleton s =  Name.Set.singleton (Name.of_string s)
let _ = Callback.register "names_singleton" names_singleton

let names_add s =  Name.Set.add (Name.of_string s)
let _ = Callback.register "names_add" names_add

type 'a namemap = 'a Name.Map.t


(*s Constrains. *)

type cnstrnt = Number.t


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

let term_output = Pretty.term
let _ = Callback.register "term_output" term_output

(*s Uninterpred function application and function update. *)
         
let mk_uninterp x =
  let str = Name.of_string x in
  Uninterp.mk_uninterp str
let _ = Callback.register "mk_uninterp" mk_uninterp

  
(*s Constructing arithmetic expressions. *)

let mk_num = Linarith.mk_num
let _ = Callback.register "mk_num" mk_num

let mk_multq q = Linarith.mk_multq q
let _ = Callback.register "mk_multq" mk_multq

let mk_add = Linarith.mk_add
let _ = Callback.register "mk_add" mk_add

let mk_sub  = Linarith.mk_sub
let _ = Callback.register "mk_sub" mk_sub

let mk_unary_minus  = Linarith.mk_neg
let _ = Callback.register "mk_unary_minus" mk_unary_minus

let is_arith = Linarith.is_interp
let _ = Callback.register "is_arith" is_arith




(*s Constructing tuples and projections. *)

let mk_tuple = Tuple.mk_tuple
let _ = Callback.register "mk_tuple" mk_tuple

let mk_proj i j = Tuple.mk_proj i j
let _ = Callback.register "mk_proj" mk_proj


(*s Bitvector terms. *)

let mk_bvconst s = Bv.mk_const (Bitv.from_string s)
let _ = Callback.register "mk_bvconst" mk_bvconst

let mk_bvsub = Bv.mk_sub
let _ = Callback.register "mk_bvsub" mk_bvsub

let mk_bvconc = Bv.mk_conc
let _ = Callback.register "mk_bvconc" mk_bvconc

let mk_bwite = Bv.mk_bitwise
let _ = Callback.register "mk_bwite" mk_bwite

let mk_bwand n a b =
  Bv.mk_bitwise n a b (Bv.mk_zero n)
let _ = Callback.register "mk_bwand" mk_bwand

let mk_bwor n a b =
  Bv.mk_bitwise n a (Bv.mk_one n) b
let _ = Callback.register "mk_bwor" mk_bwor

let mk_bwxor n a b = failwith "mk_bwxor: to do"
let _ = Callback.register "mk_bwxor" mk_bwxor

let mk_bwnot n a =
  Bv.mk_bitwise n a (Bv.mk_zero n) (Bv.mk_one n)
let _ = Callback.register "mk_bwnot" mk_bwnot

	  
(*s Boolean terms. *)

let mk_true () = Bool.mk_tt
let _ = Callback.register "mk_true" mk_true

let mk_false () = Bool.mk_ff
let _ = Callback.register "mk_false" mk_false

type atom = Atom.t
type atoms = Atom.Set.t

let mk_equal = Atom.mk_equal
let _ = Callback.register "mk_equal" mk_equal  

let mk_diseq = Atom.mk_diseq
let _ = Callback.register "mk_diseq" mk_diseq

let mk_in c a = failwith "mk_in: to do"
let _ = Callback.register "mk_in" mk_in

let mk_real = Atom.mk_in Number.mk_real
let _ = Callback.register "mk_real" mk_real

let mk_lt = Atom.mk_lt
let _ = Callback.register "mk_lt" mk_lt

let mk_le = Atom.mk_le
let _ = Callback.register "mk_le" mk_le

let mk_gt a b = Atom.mk_lt b a
let _ = Callback.register "mk_gt" mk_gt

let mk_ge a b = Atom.mk_le b a
let _ = Callback.register "mk_ge" mk_ge
 
let is_true a =
  match Term.destruct a with 
    | f, [] -> Sym.eq f Sym.mk_tt 
    | _ -> false
let _ = Callback.register "is_true" is_true

let is_false a =
  match Term.destruct a with 
    | f, [] -> Sym.eq f Sym.mk_ff
    | _ -> false
let _ = Callback.register "is_false" is_false


(*s Propositions. *)

type prop = Prop.t

(*s Builtin applications. *)

let mk_unsigned = Builtin.mk_unsigned
let _ = Callback.register "mk_unsigned" mk_unsigned

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

open Dp

type state = Dp.t

let state_eq = (==)
let _ = Callback.register "state_eq" state_eq  

let state_init () = Dp.empty
let _ = Callback.register "state_init" state_init

let state_ctxt_of s = failwith "tstate_ctxt_of o do" (* s.Dp.ctxt *)
let _ = Callback.register "state_ctxt_of" state_ctxt_of


let state_diseqs_of s = failwith "state_diseqs_of: to do"
let _ = Callback.register "state_diseqs_of" state_diseqs_of


let state_cnstrnts_of s = failwith "state_cnstrnts_of: to do"
let _ = Callback.register "state_cnstrnts_of" state_cnstrnts_of

let state_diseqs s = failwith "state_diseqs: to do"
let _ = Callback.register "state_diseqs" state_diseqs
	  
(*s Processing of new equalities. *)

type status = Dp.t Dp.status

let is_consistent = function Dp.Satisfiable _ -> true | _ -> false
let _ = Callback.register "is_consistent" is_consistent

let is_redundant r = 
  failwith "is_redundant: to do"
   (* (r = Process.Valid) *)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent r =
  failwith "is_inconsistent: to do"
   (* (r = Process.Inconsistent) *)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent foo = failwith "d_consistent: to do"
(* function
  | Process.Satisfiable s -> s
  | _ -> failwith "Ics.d_consistent: fatal error"
 *)
	
let _ = Callback.register "d_consistent" d_consistent  

let process s a = failwith "process: to do"
let _ = Callback.register "process" process   



(*s Normalization functions *)

let can = Dp.can_a
let _ = Callback.register "can" can

(*s Command interface. *)

type istate = State.t

let istate_current = State.current
let _ = Callback.register "istate_current" istate_current
let istate_symtab = State.symtab
(* let _ = Callback.register "istate_symtab" istate_symtab *)
let istate_def = State.def
let _ = Callback.register "istate_def" istate_def
let istate_sig = State.sgn
let _ = Callback.register "istate_sig" istate_sig
let istate_set_in_channel = State.set_inchannel 
let _ = Callback.register "istate_set_in_channel" istate_set_in_channel
let istate_set_out_channel = State.set_outchannel 
let _ = Callback.register "istate_set_out_channel" istate_set_out_channel
let istate_type = State.typ
let _ = Callback.register "istate_type" istate_type
let istate_flush = State.flush
let _ = Callback.register "istate_flush" istate_flush
let istate_nl = State.nl
let _ = Callback.register "istate_nl" istate_nl
let istate_can  _ = failwith "istate_can: to do"
let _ = Callback.register "istate_can" istate_can
let istate_process _ = failwith "istate_process: to do" (* State.process *)
let _ = Callback.register "istate_process" istate_process
let istate_check ns a = failwith "istate_check: to do"
(* let _ = Callback.register "istate_check" istate_check *)
let istate_reset = State.reset
let _ = Callback.register "istate_reset" istate_reset
let istate_save = State.save
let _ = Callback.register "istate_save" istate_save
let istate_restore = State.restore
let _ = Callback.register "istate_restore" istate_restore
let istate_remove = State.remove
let _ = Callback.register "istate_remove" istate_remove
let istate_forget = State.forget
let _ = Callback.register "istate_forget" istate_forget

let istate_eval () =
  try 
    Parser.commands Lexer.token (Lexing.from_channel (State.inchannel()))
  with 
    | Invalid_argument str -> Format.eprintf "Error(%s).@." str
    | Parsing.Parse_error -> Format.eprintf "Error(Syntax).@."
    | End_of_file -> raise End_of_file
    | Sys.Break -> raise Sys.Break
  (*  | _ -> Format.eprintf "Internal error.@." *)

let _ = Callback.register "istate_eval" istate_eval

(*s Abstract sign interpretation. *)

let cnstrnt s a = 
  failwith "cnstrnt: to do"
(*
  Dp.cnstrnt s a
 *)

(*s Tools *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush


(*s Callbacks for some basic Caml data structures. *)

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

let ints_of_num q =
  (Z.to_string (Q.numerator q), Z.to_string (Q.denominator q))
let _ = Callback.register "ints_of_num" ints_of_num

let num_of_int = Q.of_int
let _ = Callback.register "num_of_int" num_of_int
		   
let num_of_ints i1 i2 = Q.div (num_of_int i1) (num_of_int i2)
let _ = Callback.register "num_of_ints" num_of_ints

let string_of_num = Q.to_string
let _ = Callback.register "string_of_num" string_of_num

let num_of_string = Q.of_string
let _ = Callback.register "num_of_string" num_of_string

let mk_string s = s


