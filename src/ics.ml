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
 * 
 * Author: Harald Ruess
 *)

open Format
open Sym
open Term


let init (n, pp, eot, inch, outch) =
  Istate.initialize pp eot inch outch;
  if n = 0 then
    Sys.catch_break true                 (** raise [Sys.Break] exception upon *)
                                         (** user interrupt. *)

let set_maxloops n =
  Context.maxclose := n
let _ = Callback.register "set_maxloops" set_maxloops


let do_at_exit () = Tools.do_at_exit ()
let _ = Callback.register "do_at_exit" do_at_exit

let _ = Callback.register "init" init

(** Channels. *)

type inchannel = in_channel
type outchannel = Format.formatter

let channel_stdin () = Pervasives.stdin
let _ = Callback.register "channel_stdin" channel_stdin

let channel_stdout () = Format.std_formatter
let _ = Callback.register "channel_stdout"  channel_stdout

let  channel_stderr () = Format.err_formatter
let _ = Callback.register "channel_stderr"  channel_stderr

let inchannel_of_string = Pervasives.open_in
let _ = Callback.register "inchannel_of_string" inchannel_of_string

let outchannel_of_string str = 
  Format.formatter_of_out_channel (Pervasives.open_out str)
let _ = Callback.register "outchannel_of_string" outchannel_of_string


(* Names. *)

type name = Name.t

let name_of_string = Name.of_string
let _ = Callback.register "name_of_string" name_of_string

let name_to_string = Name.to_string
let _ = Callback.register "name_to_string" name_to_string

let name_eq = Name.eq
let _ = Callback.register "name_eq" name_eq


(** Constrains. *)

type cnstrnt = Cnstrnt.t

(** Theories. *)

type th = int

let th_to_string n = Th.to_string (Th.of_int n)
let _ = Callback.register "th_to_string" th_to_string

let th_of_string s = Th.to_int (Th.of_string s)
let _ = Callback.register "th_of_string" th_of_string


(** Function symbols. These are partitioned into uninterpreted
 function symbols and function symbols interpreted in one of the
 builtin theories. For each interpreted function symbol there is
 a recognizer function [is_xxx].  Some values of type [sym] represent 
 families of function symbols. The corresponding indices can be obtained
 using the destructor [d_xxx] functions (only after checking that [is_xxx]
 holds. *)

open Sym

type sym = Sym.t

let sym_eq = Sym.eq
let _ = Callback.register "sym_eq" sym_eq

let sym_cmp = Sym.cmp
let _ = Callback.register "sym_cmp" sym_cmp

let sym_theory_of f = Th.to_int (Th.of_sym f)
let _ = Callback.register "sym_theory_of" sym_theory_of

let sym_is_uninterp = function Uninterp _ -> true | _ -> false
let _ = Callback.register "sym_is_uninterp" sym_is_uninterp

let sym_d_uninterp = function 
  | Uninterp(n) -> n 
  | _ -> assert false
let _ = Callback.register "sym_d_uninterp" sym_d_uninterp


let sym_mk_num = Arith.num
let _ = Callback.register "sym_mk_num" sym_mk_num

let sym_is_num = function Arith(Num _) -> true | _ -> false
let _ = Callback.register "sym_is_num" sym_is_num

let sym_d_num = function Arith(Num(q)) -> q | _ -> assert false
let _ = Callback.register "sym_d_num" sym_d_num

let sym_mk_multq = Arith.multq
let _ = Callback.register "sym_mk_multq" sym_mk_multq

let sym_is_multq = function Arith(Multq _) -> true | _ -> false
let _ = Callback.register "sym_is_multq" sym_is_multq

let sym_d_multq = function Arith(Multq(q)) -> q | _ -> assert false
let _ = Callback.register "sym_d_multq" sym_d_multq

let sym_mk_add () = Arith.add
let _ = Callback.register "sym_mk_add" sym_mk_add

let sym_is_add = function Arith(Add) -> true | _ -> false
let _ = Callback.register "sym_is_add" sym_is_add

let sym_mk_tuple () = Tuple.product
let _ = Callback.register "sym_mk_product" sym_mk_tuple

let sym_is_tuple = function Product(Tuple) -> true | _ -> false
let _ = Callback.register "sym_is_tuple" sym_is_tuple

let sym_mk_proj = Tuple.proj
let _ = Callback.register "sym_mk_proj" sym_mk_proj

let sym_is_proj = function Product(Proj _) -> true | _ -> false
let _ = Callback.register "sym_is_proj" sym_is_proj

let sym_d_proj = function Product(Proj(i,n)) -> (i,n) | _ -> assert false
let _ = Callback.register "sym_d_proj" sym_d_proj

let sym_mk_inl () = Coproduct.inl
let _ = Callback.register "sym_mk_inl" sym_mk_inl

let sym_is_inl = function Coproduct(InL) -> true | _ -> false
let _ = Callback.register "sym_is_inl" sym_is_inl

let sym_mk_inr () = Coproduct.inr
let _ = Callback.register "sym_mk_inr" sym_mk_inr

let sym_is_inr = function Coproduct(InR) -> true | _ -> false
let _ = Callback.register "sym_is_inr" sym_is_inr

let sym_mk_outl () = Coproduct.outl
let _ = Callback.register "sym_mk_outl" sym_mk_outl

let sym_is_outl = function Coproduct(OutL) -> true | _ -> false
let _ = Callback.register "sym_is_outl" sym_is_outl

let sym_mk_outr () = Coproduct.outr
let _ = Callback.register "sym_mk_outr" sym_mk_outr

let sym_is_outr = function Coproduct(OutR) -> true | _ -> false
let _ = Callback.register "sym_is_outr" sym_is_outr

let sym_mk_bv_const s = Bitvector.const(Bitv.from_string s)
let _ = Callback.register "sym_mk_bv_const" sym_mk_bv_const

let sym_is_bv_const = function Bv(Const _) -> true | _ -> false
let _ = Callback.register "sym_is_bv_const" sym_is_bv_const

let sym_mk_bv_conc  = Bitvector.conc
let _ = Callback.register "sym_mk_bv_conc" sym_mk_bv_conc

let sym_is_bv_conc = function Bv(Conc _) -> true | _ -> false
let _ = Callback.register "sym_is_bv_conc" sym_is_bv_conc

let sym_d_bv_conc = function Bv(Conc(n, m)) -> (n, m) | _ -> assert false
let _ = Callback.register "sym_d_bv_conc" sym_d_bv_conc

let sym_mk_bv_sub  = Bitvector.sub
let _ = Callback.register "sym_mk_bv_sub" sym_mk_bv_sub

let sym_is_bv_sub = function Bv(Sub _) -> true | _ -> false
let _ = Callback.register "sym_is_bv_sub" sym_is_bv_sub

let sym_d_bv_sub = function Bv(Sub(n, i, j)) -> (n, i, j) | _ -> assert false
let _ = Callback.register "sym_d_bv_sub" sym_d_bv_sub

let sym_mk_bv_bitwise  = Bitvector.bitwise
let _ = Callback.register "sym_mk_bv_bitwise" sym_mk_bv_bitwise

let sym_is_bv_bitwise = function Bv(Bitwise _) -> true | _ -> false
let _ = Callback.register "sym_is_bv_bitwise" sym_is_bv_bitwise

let sym_d_bv_bitwise = function Bv(Bitwise(n)) -> n | _ -> assert false
let _ = Callback.register "sym_d_bv_bitwise" sym_d_bv_bitwise

let sym_mk_mult () = Pp.mult
let _ = Callback.register "sym_mk_mult" sym_mk_mult

let sym_is_mult = function Pp(Mult) -> true | _ -> false
let _ = Callback.register "sym_is_mult" sym_is_mult

let sym_mk_expt = Pp.expt
let _ = Callback.register "sym_mk_expt" sym_mk_expt

let sym_is_expt = function Pp(Mult) -> true | _ -> false
let _ = Callback.register "sym_is_expt" sym_is_expt

let sym_d_expt = function Pp(Expt(n)) -> n | _ -> assert false
let _ = Callback.register "sym_d_expt" sym_d_expt

let sym_mk_apply _ = Apply.apply None
let _ = Callback.register "sym_mk_apply" sym_mk_apply


let sym_is_apply = function Fun(Apply _) -> true | _ -> false
let _ = Callback.register "sym_is_apply" sym_is_apply

let sym_d_apply = function Fun(Apply(i)) -> None | _ -> assert false
let _ = Callback.register "sym_d_apply" sym_d_apply

let sym_mk_abs () = Apply.abs
let _ = Callback.register "sym_mk_abs" sym_mk_abs

let sym_is_abs = function Fun(Abs) -> true | _ -> false
let _ = Callback.register "sym_is_abs" sym_is_abs

let sym_mk_update () = Arr.update
let _ = Callback.register "sym_mk_update" sym_mk_update

let sym_is_select = function Arrays(Select) -> true | _ -> false
let _ = Callback.register "sym_is_select" sym_is_select

let sym_mk_select () = Arr.select
let _ = Callback.register "sym_mk_select" sym_mk_select

let sym_is_update = function Arrays(Update) -> true | _ -> false
let _ = Callback.register "sym_is_update" sym_is_update

let sym_mk_unsigned () = Bvarith.unsigned
let _ = Callback.register "sym_mk_unsigned" sym_mk_unsigned

let sym_is_unsigned = function Bvarith(Unsigned) -> true | _ -> false
let _ = Callback.register "sym_is_unsigned" sym_is_unsigned


(** Variables. *)

type var = Var.t

let var_name_of = Var.name_of
let _ = Callback.register "var_name_of" var_name_of
 
let var_eq = Var.eq
let _ = Callback.register "var_eq" var_eq
 
let var_cmp = Var.cmp
let _ = Callback.register "var_cmp" var_cmp

let var_mk_external = Var.mk_var
let _ = Callback.register "var_mk_external" var_mk_external

let var_mk_fresh = Var.mk_fresh
let _ = Callback.register "var_mk_fresh" var_mk_fresh

let var_mk_bound = Var.mk_free
let _ = Callback.register "var_mk_bound" var_mk_bound

let var_is_external = Var.is_var
let _ = Callback.register "var_is_external" var_is_external

let var_is_fresh = Var.is_fresh
let _ = Callback.register "var_is_fresh" var_is_fresh

let var_is_bound = Var.is_free
let _ = Callback.register "var_is_bound" var_is_bound

let var_d_bound = Var.d_free
let _ = Callback.register "var_d_bound" var_d_bound


(** Terms ar either variables, uninterpreted applications,
  or interpreted applications including boolean terms. *)
  
type term = Term.t

let term_of_string s =
  let lb = Lexing.from_string s in 
  Parser.termeof Lexer.token lb
let _ = Callback.register "term_of_string" term_of_string

let term_to_string = Pretty.to_string Term.pp 
let _ = Callback.register "term_to_string" term_to_string

let term_input ch =
  let lb = Lexing.from_channel ch in 
  Parser.termeof Lexer.token lb
let _ = Callback.register "term_input" term_input

let term_output = Term.pp
let _ = Callback.register "term_output" term_output

let term_pp a = Term.pp Format.std_formatter a; Format.print_flush ()
let _ = Callback.register "term_pp" term_pp


(** Construct a variable. *)

let term_mk_var str =
  let x = Name.of_string str in
  Term.mk_var x
let _ = Callback.register "term_mk_var" term_mk_var


(** Uninterpred function application and function update. *)
         
let term_mk_uninterp x l =
  let f = Sym.Uninterp(Name.of_string x) in
  App.sigma f l
let _ = Callback.register "term_mk_uninterp" term_mk_uninterp

  
(** Constructing arithmetic expressions. *)

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




(** Constructing tuples and projections. *)

let term_mk_tuple = Tuple.mk_tuple
let _ = Callback.register "term_mk_tuple" term_mk_tuple

let term_mk_proj i j = Tuple.mk_proj i j
let _ = Callback.register "term_mk_proj" term_mk_proj


(** Bitvector terms. *)

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

	  
(** Boolean terms. *)

let term_mk_true () = Boolean.mk_true
let _ = Callback.register "term_mk_true" term_mk_true

let term_mk_false () = Boolean.mk_false
let _ = Callback.register "term_mk_false" term_mk_false

(** Coproducts. *)

let term_mk_inj = Coproduct.mk_inj
let _ = Callback.register "term_mk_inj" term_mk_inj

let term_mk_out = Coproduct.mk_out
let _ = Callback.register "term_mk_out" term_mk_out


(** Interpretation Domains *)

type dom = Dom.t

(** Atoms. *)

type atom = Atom.t

let atom_pp a = 
  Atom.pp Format.std_formatter a;
  Format.print_flush ()
let _ = Callback.register "atom_pp" atom_pp

let atom_of_string s = 
  let lb = Lexing.from_string s in 
  Parser.atomeof Lexer.token lb
let _ = Callback.register "atom_of_string" atom_of_string

let atom_to_string = Pretty.to_string Atom.pp
let _ = Callback.register "atom_to_string" atom_to_string

let atom_mk_equal a b = 
  Atom.mk_equal (a, b)
let _ = Callback.register "atom_mk_equal" atom_mk_equal  

let atom_mk_diseq a b = 
  Atom.mk_diseq (a, b)
let _ = Callback.register "atom_mk_diseq" atom_mk_diseq

let atom_mk_true () = Atom.mk_true
let _ = Callback.register "atom_mk_true" atom_mk_true

let atom_mk_false () = Atom.mk_false
let _ = Callback.register "atom_mk_false" atom_mk_false

let atom_mk_in a d = 
  Atom.mk_in (a, d)
let _ = Callback.register "atom_mk_in" atom_mk_in

let atom_mk_real a = Atom.mk_in (a, Dom.Real)
let _ = Callback.register "atom_mk_real" atom_mk_real

let atom_mk_int a = Atom.mk_in (a, Dom.Int)
let _ = Callback.register "atom_mk_int" atom_mk_int

let atom_mk_nonint a = Atom.mk_in (a, Dom.Nonint)
let _ = Callback.register "atom_mk_nonint" atom_mk_nonint

let atom_mk_lt a b = Atom.mk_less (a, false, b)
let _ = Callback.register "atom_mk_lt"  atom_mk_lt

let atom_mk_le a b = Atom.mk_less (a, true, b)
let _ = Callback.register "atom_mk_le"  atom_mk_le

let atom_mk_gt a b = atom_mk_lt b a
let _ = Callback.register "atom_mk_gt" atom_mk_gt

let atom_mk_ge a b = atom_mk_le b a
let _ = Callback.register "atom_mk_ge" atom_mk_ge

let atom_is_negatable = Atom.is_negatable
let _ = Callback.register "atom_is_negatable" atom_is_negatable

let atom_negate = Atom.negate
let _ = Callback.register "atom_negate" atom_negate


type atoms = Atom.Set.t

let atoms_empty () =   
  Format.eprintf "Atoms_empty@.";
  Atom.Set.empty 
		    
let _ = Callback.register "atoms_empty" atoms_empty

let atoms_add = Atom.Set.add
let _ = Callback.register "atoms_add" atoms_add

let atoms_singleton = Atom.Set.singleton
let _ = Callback.register "atoms_singleton" atoms_singleton

let atoms_to_list atms = 
  Format.eprintf "Atoms_to_list@.";
  Atom.Set.elements atms
let _ = Callback.register "atoms_to_list" atoms_to_list

let term_is_true = Boolean.is_true
let _ = Callback.register "term_is_true" term_is_true

let term_is_false = Boolean.is_false
let _ = Callback.register "term_is_false" term_is_false


(** Propositions *)

type prop = Prop.t
 
let prop_mk_true () = Prop.mk_true
let _ = Callback.register "prop_mk_true" prop_mk_true

let prop_mk_false () = Prop.mk_false
let _ = Callback.register "prop_mk_false" prop_mk_false

let prop_mk_var = Prop.mk_var
let _ = Callback.register "prop_mk_var" prop_mk_var

let prop_mk_poslit = Prop.mk_poslit
let _ = Callback.register "prop_mk_poslit" prop_mk_poslit

let prop_mk_neglit = Prop.mk_neglit
let _ = Callback.register "prop_mk_neglit" prop_mk_neglit

let prop_mk_ite = Prop.mk_ite
let _ = Callback.register "prop_mk_ite" prop_mk_ite

let prop_mk_conj = Prop.mk_conj
let _ = Callback.register "prop_mk_conj" prop_mk_conj

let prop_mk_disj = Prop.mk_disj
let _ = Callback.register "prop_mk_disj" prop_mk_disj

let prop_mk_iff = Prop.mk_iff
let _ = Callback.register "prop_mk_iff" prop_mk_iff

let prop_mk_neg = Prop.mk_neg
let _ = Callback.register "prop_mk_neg" prop_mk_neg

(*
let prop_is_true = Prop.is_true
let _ = Callback.register "prop_is_true" prop_is_true

let prop_is_false = Prop.is_false
let _ = Callback.register "prop_is_false" prop_is_false

let prop_is_var = Prop.is_var
let _ = Callback.register "prop_is_var" prop_is_var

let prop_is_atom = Prop.is_atom
let _ = Callback.register "prop_is_atom" prop_is_atom

let prop_is_ite = Prop.is_ite
let _ = Callback.register "prop_is_ite" prop_is_ite

let prop_is_disj = Prop.is_disj
let _ = Callback.register "prop_is_disj" prop_is_disj

let prop_is_iff = Prop.is_iff
let _ = Callback.register "prop_is_iff" prop_is_iff

let prop_is_neg = Prop.is_neg
let _ = Callback.register "prop_is_neg" prop_is_neg

let prop_d_atom = Prop.d_atom
let _ = Callback.register "prop_d_atom" prop_d_atom

let prop_d_var = Prop.d_var
let _ = Callback.register "prop_d_var" prop_d_var

let prop_d_ite = Prop.d_ite
let _ = Callback.register "prop_d_ite" prop_d_ite

let prop_d_disj = Prop.d_disj
let _ = Callback.register "prop_d_disj" prop_d_disj

let prop_d_iff = Prop.d_iff
let _ = Callback.register "prop_d_iff" prop_d_iff

let prop_d_neg = Prop.d_neg
let _ = Callback.register "prop_d_neg" prop_d_neg
*)

type assignment = Prop.Assignment.t

let sat = Prop.sat
let _ = Callback.register "sat" sat


(** Nonlinear terms. *)


let term_mk_mult = Sig.mk_mult
let _ = Callback.register "term_mk_mult" term_mk_mult

let rec term_mk_multl = function
  | [] -> Arith.mk_one
  | [a] -> a
  | a :: b :: l -> term_mk_multl (term_mk_mult a b :: l)
let _ = Callback.register "term_mk_multl" term_mk_multl


let term_mk_expt = Sig.mk_expt 
let _ = Callback.register "term_mk_expt" term_mk_expt


(** Builtin applications. *)

let term_mk_unsigned = Bvarith.mk_unsigned
let _ = Callback.register "term_mk_unsigned" term_mk_unsigned

let term_mk_update  = Arr.mk_update 
let _ = Callback.register "term_mk_update" term_mk_update

let term_mk_select = Arr.mk_select
let _ = Callback.register "term_mk_select" term_mk_select

let term_mk_div = Sig.mk_div
let _ = Callback.register "term_mk_div" term_mk_div

let term_mk_apply = 
  Apply.mk_apply (Context.sigma Context.empty) None
let _ = Callback.register "term_mk_apply" term_mk_apply

(** Set of terms. *)

type terms = Term.Set.t

(* Term map. *)

type 'a map = 'a Term.Map.t
	  

(** Equalities. *)

let term_eq = Term.eq
let _ = Callback.register "term_eq" term_eq

let term_cmp = Term.cmp
let _ = Callback.register "term_cmp" term_cmp

(** Trace level. *)
    
type trace_level = string

let trace_reset = Trace.reset
let _ = Callback.register "trace_reset" trace_reset

let trace_add = Trace.add
let _ = Callback.register "trace_add" trace_add

let trace_remove = Trace.add
let _ = Callback.register "trace_remove" trace_remove

let trace_get = Trace.get
let _ = Callback.register "trace_get" trace_get


(** Solution sets. *)

type solution = Solution.t

let solution_apply s x = Solution.apply s x

let solution_find s x = Solution.find s x

let solution_inv s b = Solution.inv s b

let solution_mem = Solution.mem

let solution_occurs = Solution.occurs

let solution_use = Solution.use

let solution_is_empty = Solution.is_empty




(** States. *)

type context = Context.t

let context_eq = Context.eq
let _ = Callback.register "context_eq" context_eq  

let context_empty () = Context.empty
let _ = Callback.register "context_empty" context_empty

let context_ctxt_of s = (Atom.Set.elements (Context.ctxt_of s))
let _ = Callback.register "context_ctxt_of" context_ctxt_of

let context_u_of s = Context.eqs_of s Th.u
let _ = Callback.register "context_u_of" context_u_of

let context_a_of s = Context.eqs_of s Th.la
let _ = Callback.register "context_a_of" context_a_of

let context_t_of s = Context.eqs_of s Th.p
let _ = Callback.register "context_t_of" context_t_of

let context_bv_of s =  Context.eqs_of s Th.bv
let _ = Callback.register "context_bv_of" context_bv_of

let context_pp s = Context.pp Format.std_formatter s; Format.print_flush()
let _ = Callback.register "context_pp" context_pp

let context_ctxt_pp s = 
  let al = (Atom.Set.elements (Context.ctxt_of s)) in
  let fmt = Format.std_formatter in
    List.iter
      (fun a ->
	 Pretty.string fmt "\nassert ";
	 Atom.pp fmt a;
	 Pretty.string fmt " .")
      al;
    Format.print_flush()
let _ = Callback.register "context_ctxt_pp" context_ctxt_pp

	  

(** Processing of new equalities. *)

type status = Context.t Context.Status.t

let is_consistent r = 
  (match r with
     | Context.Status.Ok _ -> true
     | _ -> false)
let _ = Callback.register "is_consistent" is_consistent

let is_redundant r = (r = Context.Status.Valid)
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent r =
   (r = Context.Status.Inconsistent)
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent r =
  match r with
    | Context.Status.Ok s -> s
    | _ -> (context_empty())
         (* failwith "Ics.d_consistent: fatal error" *)
	
let _ = Callback.register "d_consistent" d_consistent 

let _ = Trace.add "api"
 
let process = Context.add
let _ = Callback.register "process" process   

let split s = 
  Atom.Set.elements (Context.split s) 
let _ = Callback.register "split" split


(** Normalization functions *)

let can = Context.Can.atom
let _ = Callback.register "can" can

let read_from_channel ch = 
  Parser.commands Lexer.token (Lexing.from_channel ch)

let read_from_string str =  
  Parser.commandseof Lexer.token (Lexing.from_string str)

let rec cmd_rep () =
  let inch = Istate.inchannel() in
  let outch = Istate.outchannel() in
  try
    cmd_output outch (read_from_channel inch);
    Format.fprintf outch "@?"
  with
    | Invalid_argument str -> 
	cmd_error outch str
    | Parsing.Parse_error -> 
	cmd_error outch "Syntax error"
    | End_of_file ->
	 cmd_quit 0 outch;
    | Sys.Break -> 
	 cmd_quit 1 outch;
    | Failure "drop" -> 
	raise (Failure "drop")
    | exc -> 
	(cmd_error outch ("Exception " ^ (Printexc.to_string exc)))

and cmd_batch () =
  let inch = Istate.inchannel() in
  let outch = Istate.outchannel() in
  try
    match Parser.commandsequence Lexer.token (Lexing.from_channel inch) with
      | Result.Process(Context.Status.Inconsistent) ->
	  raise Exc.Inconsistent
      | result ->   (* be quiet (not any more) *)
	  cmd_output outch result
  with
    | Invalid_argument str -> 
	cmd_error outch str;
	cmd_quit 3 outch
    | Parsing.Parse_error -> 
	let number =  string_of_int !Tools.linenumber in
	  cmd_error outch ("Syntax error on line " ^ number);
	  cmd_quit 2 outch
    | End_of_file ->
	cmd_quit 0 outch
    | Sys.Break -> 
	cmd_quit 1 outch
    | Failure "drop" -> 
	raise (Failure "drop")
    | Exc.Inconsistent ->
	Format.fprintf outch ":unsat\n@?";
	cmd_quit (-1) outch
    | exc -> 
	cmd_error outch ("Exception " ^ (Printexc.to_string exc));
	cmd_quit 4 outch



and cmd_output fmt result =
  (match result with
     | Result.Process(status) -> 
	 Context.Status.pp Name.pp fmt status
     | Result.Sat(None) ->
	 Format.fprintf fmt ":unsat@?"
     | Result.Sat(Some(rho)) ->
	 Format.fprintf fmt ":sat "; Prop.Assignment.pp fmt rho
     | Result.Unit() ->
	 Format.fprintf fmt ":unit@?"
     | Result.Bool(true) ->
	 Format.fprintf fmt ":true@?"
     | Result.Bool(false) ->
	 Format.fprintf fmt ":false@?"
     | value -> 
	 Format.fprintf fmt ":val ";
	 Result.output fmt value;
         Format.fprintf fmt "@?");
  cmd_endmarker fmt
	
and cmd_error fmt str =
  Format.fprintf fmt ":error %s" str;
  Format.fprintf fmt "\n%s@?" (Istate.eot())

and cmd_quit n fmt =
  do_at_exit();
  Format.fprintf fmt "\n";
  cmd_endmarker fmt;
  exit n

and cmd_endmarker fmt =
  let eot = Istate.eot () in
  if eot = "" then
    Format.pp_print_flush fmt ()
  else 
    begin
      Format.fprintf fmt "\n%s" eot;
      Format.pp_print_flush fmt ()
    end 

let _ = Callback.register "cmd_rep" cmd_rep
let _ = Callback.register "cmd_batch" cmd_batch


(** Abstract sign interpretation. *)

let cnstrnt = Context.cnstrnt
 
 

(** Tools *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush


(** Lists. *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "is_nil" is_nil

let cons x l = x :: l
let _ = Callback.register "cons" cons

let head = List.hd
let _ = Callback.register "head" head

let tail = List.tl
let _ = Callback.register "tail" tail


(** Pairs. *)

let pair x y = (x,y)
let _ = Callback.register "pair" pair

let fst = fst
let _ = Callback.register "fst" fst

let snd = snd
let _ = Callback.register "snd" snd


(** Triples. *)

let triple x y z = (x,y,z)
let _ = Callback.register "triple" triple

let fst_of_triple = function (x,_,_) -> x
let _ = Callback.register "fst_of_triple" fst_of_triple

let snd_of_triple = function (_,y,_) -> y
let _ = Callback.register "snd_of_triple" snd_of_triple

let third_of_triple = function (_,_,z) -> z
let _ = Callback.register "third_of_triple" third_of_triple
  

(** Quadruples. *)
   
let fst_of_quadruple  = function (x1,_,_,_) -> x1
let _ = Callback.register "fst_of_quadruple" fst_of_quadruple

let snd_of_quadruple = function (_,x2,_,_) -> x2
let _ = Callback.register "snd_of_quadruple" snd_of_quadruple

let third_of_quadruple = function (_,_,x3,_) -> x3
let _ = Callback.register "third_of_quadruple" third_of_quadruple

let fourth_of_quadruple = function (_,_,_,x4) -> x4
let _ = Callback.register "fourth_of_quadruple" fourth_of_quadruple
    

(** Options. *)

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

(** Sleeping. *)

let sleep = Unix.sleep
let _ = Callback.register "sleep" sleep
	 
(** Multi-precision arithmetic.*)

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

