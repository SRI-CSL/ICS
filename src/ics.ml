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
 * 
 * Author: Harald Ruess
 *)

(** Application programming interface. *)

open Format

external ics_is_licensed : unit -> bool = "ics_is_licensed"

let is_licensed = ref false

let license_check () = 
  if !is_licensed then
    ()
  else if ics_is_licensed() then
    begin
      is_licensed := true;
    end 
  else 
    begin
      Format.eprintf "\nExit...";
      exit(-1)
    end

let init (n) =
  license_check ();
  if n = 0 then
    Sys.catch_break true                 (** raise [Sys.Break] exception upon *)
                                         (** user interrupt. *)

let _ = Callback.register "init" init

let version () = Version.version
let _ = Callback.register "version" version


(** {6 Parameters} *)

let set_profile b = Tools.profiling := b
let _ = Callback.register "set_profile" set_profile

let set_pretty mode = Pretty.flag := Pretty.Mode.of_string mode
let _ = Callback.register "set_pretty" set_pretty

let set_compactify b =  V.garbage_collection_enabled := b
let _ = Callback.register "set_compactify" set_compactify

let set_verbose b = Prop.set_verbose b; Context.verbose := true
let _ = Callback.register "set_verbose" set_verbose

let set_assertion_frequency = Prop.set_assertion_frequency
let _ = Callback.register "set_assertion_frequency" set_assertion_frequency

let set_remove_subsumed_clauses = Prop.set_remove_subsumed_clauses
let _ = Callback.register "set_remove_subsumed_clauses" set_remove_subsumed_clauses

let set_validate = Prop.set_validate
let _ = Callback.register "set_validate" set_validate

let set_polarity_optimization = Prop.set_polarity_optimization
let _ = Callback.register "set_polarity_optimization" set_polarity_optimization

let set_clause_relevance = Prop.set_clause_relevance
let _ = Callback.register "set_clause_relevance" set_clause_relevance

let set_cleanup_period = Prop.set_cleanup_period
let _ = Callback.register "set_cleanup_period" set_cleanup_period

let set_num_refinements = Prop.set_num_refinements
let _ = Callback.register "set_num_refinements" set_num_refinements

let set_justifications b =  Fact.print_justification := b
let _ = Callback.register "set_justifications" set_justifications

let set_statistic b =  Prop.statistics := b
let _ = Callback.register "set_statistic" set_statistic

let set_show_explanations b =  Prop.show_explanations := b
let _ = Callback.register "set_show_explanations" set_show_explanations

let set_integer_solve b = Arith.integer_solve := b
let _ = Callback.register "set_integer_solve" set_integer_solve

let set_proofmode str = Jst.Mode.set(Jst.Mode.of_string str)
let _ = Callback.register "set_proofmode" set_proofmode

let set_gc_mode str =   
  let control= Gc.get() in
    match str with
      | "lazy" -> Gc.set {control with Gc.space_overhead = 10000; Gc.max_overhead = 1000000}
      | "eager" -> Gc.set {control with Gc.space_overhead = 10; Gc.max_overhead = 100}
      | str -> raise(Invalid_argument "no such GC option")
let _ = Callback.register "set_gc_mode" set_gc_mode

let set_gc_space_overhead overhead =  
  let control = Gc.get () in
    Gc.set {control with Gc.space_overhead = overhead}
let _ = Callback.register "set_gc_space_overhead" set_gc_space_overhead

let set_gc_max_overhead overhead = 
  let control = Gc.get () in
    Gc.set {control with Gc.max_overhead = overhead}
let _ = Callback.register "set_gc_max_overhead" set_gc_max_overhead



(** {6 Channels} *)

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


(** {6 Names} *)

type name = Name.t

let name_of_string = Name.of_string
let _ = Callback.register "name_of_string" name_of_string

let name_to_string = Name.to_string
let _ = Callback.register "name_to_string" name_to_string

let name_eq = Name.eq
let _ = Callback.register "name_eq" name_eq


(** {6 Constrains} *)


type dom = Dom.t

let dom_mk_int () = Dom.Int
let _ = Callback.register "dom_mk_int" dom_mk_int

let dom_mk_real () = Dom.Real
let _ = Callback.register "dom_mk_real" dom_mk_real

let dom_is_int d = (d = Dom.Int)
let _ = Callback.register "dom_is_int" dom_is_int

let dom_is_real d = (d = Dom.Real)
let _ = Callback.register "dom_is_real" dom_is_real


(** {6 Theories} *)

type th = Th.t

let th_to_string = Th.to_string
let _ = Callback.register "th_to_string" th_to_string

let th_of_string = Th.of_string
let _ = Callback.register "th_of_string" th_of_string


(** {6 Function Symbols} *)

type sym = Sym.t
(** Function symbols are partitioned into uninterpreted
 function symbols and function symbols interpreted in one of the
 builtin theories. For each interpreted function symbol there is
 a recognizer function [is_xxx].  Some values of type [sym] represent 
 families of function symbols. The corresponding indices can be obtained
 using the destructor [d_xxx] functions (only after checking that [is_xxx]
 holds. *)

let sym_eq = Sym.eq
let _ = Callback.register "sym_eq" sym_eq

let sym_cmp = Sym.cmp
let _ = Callback.register "sym_cmp" sym_cmp

let sym_theory_of = Sym.theory_of
let _ = Callback.register "sym_theory_of" sym_theory_of

let sym_is_uninterp = Sym.Uninterp.is
let _ = Callback.register "sym_is_uninterp" sym_is_uninterp

let sym_d_uninterp = Sym.Uninterp.get
let _ = Callback.register "sym_d_uninterp" sym_d_uninterp

let sym_mk_num = Sym.Arith.mk_num
let _ = Callback.register "sym_mk_num" sym_mk_num

let sym_is_num = Sym.Arith.is_num
let _ = Callback.register "sym_is_num" sym_is_num

let sym_d_num = Sym.Arith.d_num
let _ = Callback.register "sym_d_num" sym_d_num

let sym_mk_multq = Sym.Arith.mk_multq
let _ = Callback.register "sym_mk_multq" sym_mk_multq

let sym_is_multq = Sym.Arith.is_multq
let _ = Callback.register "sym_is_multq" sym_is_multq

let sym_d_multq = Sym.Arith.d_multq
let _ = Callback.register "sym_d_multq" sym_d_multq

let sym_mk_add () = Sym.Arith.mk_add
let _ = Callback.register "sym_mk_add" sym_mk_add

let sym_is_add = Sym.Arith.is_add
let _ = Callback.register "sym_is_add" sym_is_add

let sym_mk_cons () = Sym.Product.mk_cons
let _ = Callback.register "sym_mk_cons" sym_mk_cons

let sym_is_cons = Sym.Product.is_cons
let _ = Callback.register "sym_is_cons" sym_is_cons

let sym_mk_car () = Sym.Product.mk_car
let _ = Callback.register "sym_mk_car" sym_mk_car

let sym_is_car  = Sym.Product.is_car
let _ = Callback.register "sym_is_car" sym_is_car

let sym_mk_cdr () = Sym.Product.mk_cdr
let _ = Callback.register "sym_mk_cdr" sym_mk_cdr

let sym_is_cdr = Sym.Product.is_cdr
let _ = Callback.register "sym_is_cdr" sym_is_cdr

let sym_mk_inl () = Sym.Coproduct.mk_inl
let _ = Callback.register "sym_mk_inl" sym_mk_inl

let sym_is_inl = Sym.Coproduct.is_inl
let _ = Callback.register "sym_is_inl" sym_is_inl

let sym_mk_inr () = Sym.Coproduct.mk_inr
let _ = Callback.register "sym_mk_inr" sym_mk_inr

let sym_is_inr = Sym.Coproduct.is_inr
let _ = Callback.register "sym_is_inr" sym_is_inr

let sym_mk_outl () = Sym.Coproduct.mk_outl
let _ = Callback.register "sym_mk_outl" sym_mk_outl

let sym_is_outl = Sym.Coproduct.is_outl
let _ = Callback.register "sym_is_outl" sym_is_outl

let sym_mk_outr () = Sym.Coproduct.mk_outr
let _ = Callback.register "sym_mk_outr" sym_mk_outr

let sym_is_outr = Sym.Coproduct.is_outr
let _ = Callback.register "sym_is_outr" sym_is_outr

let sym_mk_mult () = Sym.Pprod.mk_mult
let _ = Callback.register "sym_mk_mult" sym_mk_mult

let sym_is_mult = Sym.Pprod.is_mult
let _ = Callback.register "sym_is_mult" sym_is_mult

let sym_mk_bv_const s = Sym.Bv.mk_const (Bitv.from_string s)
let _ = Callback.register "sym_mk_bv_const" sym_mk_bv_const

let sym_is_bv_const = Sym.Bv.is_const
let _ = Callback.register "sym_is_bv_const" sym_is_bv_const

let sym_mk_bv_conc  = Sym.Bv.mk_conc
let _ = Callback.register "sym_mk_bv_conc" sym_mk_bv_conc

let sym_is_bv_conc = Sym.Bv.is_conc
let _ = Callback.register "sym_is_bv_conc" sym_is_bv_conc

let sym_d_bv_conc = Sym.Bv.d_conc
let _ = Callback.register "sym_d_bv_conc" sym_d_bv_conc

let sym_mk_bv_sub  = Sym.Bv.mk_sub
let _ = Callback.register "sym_mk_bv_sub" sym_mk_bv_sub

let sym_is_bv_sub = Sym.Bv.is_sub
let _ = Callback.register "sym_is_bv_sub" sym_is_bv_sub

let sym_d_bv_sub = Sym.Bv.d_sub
let _ = Callback.register "sym_d_bv_sub" sym_d_bv_sub


let sym_mk_update () = Sym.Array.mk_update
let _ = Callback.register "sym_mk_update" sym_mk_update

let sym_is_select = Sym.Array.is_select
let _ = Callback.register "sym_is_select" sym_is_select

let sym_mk_select () = Sym.Array.mk_select
let _ = Callback.register "sym_mk_select" sym_mk_select

let sym_is_update = Sym.Array.is_update
let _ = Callback.register "sym_is_update" sym_is_update

let sym_mk_apply () = Sym.Cl.apply
let _ = Callback.register "sym_mk_apply" sym_mk_apply

let sym_is_apply = Sym.Cl.is_apply
let _ = Callback.register "sym_is_apply" sym_is_apply

let sym_mk_s () = Sym.Cl.s
let _ = Callback.register "sym_mk_s" sym_mk_s

let sym_is_s = Sym.Cl.is_s
let _ = Callback.register "sym_is_s" sym_is_s

let sym_mk_k () = Sym.Cl.k
let _ = Callback.register "sym_mk_k" sym_mk_k

let sym_is_k = Sym.Cl.is_k
let _ = Callback.register "sym_is_k" sym_is_k

let sym_mk_i () = Sym.Cl.i
let _ = Callback.register "sym_mk_i" sym_mk_i

let sym_is_i = Sym.Cl.is_i
let _ = Callback.register "sym_is_i" sym_is_i


let sym_mk_empty () = Sym.Propset.mk_empty
let _ = Callback.register "sym_mk_empty" sym_mk_empty

let sym_is_empty = Sym.Propset.is_empty
let _ = Callback.register "sym_is_empty" sym_is_empty

let sym_mk_full () = Sym.Propset.mk_full
let _ = Callback.register "sym_mk_full" sym_mk_full

let sym_is_full = Sym.Propset.is_full
let _ = Callback.register "sym_is_full" sym_is_full

let sym_mk_ite () = Sym.Propset.mk_ite
let _ = Callback.register "sym_mk_ite" sym_mk_ite

let sym_is_ite = Sym.Propset.is_ite
let _ = Callback.register "sym_is_ite" sym_is_ite


(** {6 Terms} *)
  
type term = Term.t
(** Terms are either variables, uninterpreted applications,
  or interpreted applications including boolean terms. *)

let term_of_string s =
  let lb = Lexing.from_string s in Parser.termeof Lexer.token lb
let _ = Callback.register "term_of_string" term_of_string

let term_to_string = Pretty.to_string Term.pp 
let _ = Callback.register "term_to_string" term_to_string

let term_input ch =
  let lb = Lexing.from_channel ch in Parser.termeof Lexer.token lb
let _ = Callback.register "term_input" term_input

let term_output = Term.pp
let _ = Callback.register "term_output" term_output

let term_pp a = Term.pp Format.std_formatter a; Format.print_flush ()
let _ = Callback.register "term_pp" term_pp


(** Construct a variable. *)
let term_mk_var str =
  let x = Name.of_string str in
  Term.Var.mk_var x Var.Cnstrnt.Unconstrained
let _ = Callback.register "term_mk_var" term_mk_var


(** Uninterpred function application and function update. *)     
let term_mk_uninterp x l =
  let f = Sym.Uninterp.make (Name.of_string x) in
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
let term_mk_tuple = Product.mk_tuple
let _ = Callback.register "term_mk_tuple" term_mk_tuple

let term_mk_proj i = Product.mk_proj i
let _ = Callback.register "term_mk_proj" term_mk_proj


(** Bitvector terms. *)
let term_mk_bvconst s = Bitvector.mk_const (Bitv.from_string s)
let _ = Callback.register "term_mk_bvconst" term_mk_bvconst

let term_mk_bvsub (n,i,j) = Bitvector.mk_sub n i j
let _ = Callback.register "term_mk_bvsub" term_mk_bvsub

let term_mk_bvconc (n,m) = Bitvector.mk_conc n m
let _ = Callback.register "term_mk_bvconc" term_mk_bvconc

	  
(** Boolean terms. *)
let term_mk_true = Boolean.mk_true
let _ = Callback.register "term_mk_true" term_mk_true

let term_mk_false = Boolean.mk_false
let _ = Callback.register "term_mk_false" term_mk_false

(** Coproducts. *)
let term_mk_inj = Coproduct.mk_inj
let _ = Callback.register "term_mk_inj" term_mk_inj

let term_mk_out = Coproduct.mk_out
let _ = Callback.register "term_mk_out" term_mk_out


(** {6 Atoms} *)

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


let atom_mk_lt a b = Atom.mk_pos (Arith.mk_sub b a)
let _ = Callback.register "atom_mk_lt"  atom_mk_lt

let atom_mk_le a b = Atom.mk_nonneg (Arith.mk_sub b a)
let _ = Callback.register "atom_mk_le"  atom_mk_le

let atom_mk_gt a b =  Atom.mk_pos (Arith.mk_sub a b) 
let _ = Callback.register "atom_mk_gt" atom_mk_gt

let atom_mk_ge a b = Atom.mk_nonneg (Arith.mk_sub a b)
let _ = Callback.register "atom_mk_ge" atom_mk_ge


let atom_negate = Atom.negate Arith.mk_neg
let _ = Callback.register "atom_negate" atom_negate


let term_is_true = Boolean.is_true
let _ = Callback.register "term_is_true" term_is_true

let term_is_false = Boolean.is_false
let _ = Callback.register "term_is_false" term_is_false


(** {6 Propositions} *)

type prop = Prop.t

let prop_of_string s =
  let lb = Lexing.from_string s in 
    Parser.propeof Lexer.token lb
let _ = Callback.register "prop_of_string" prop_of_string

let prop_to_string = Pretty.to_string Prop.pp
let _ = Callback.register "prop_to_string" prop_to_string

let prop_pp = Prop.pp Format.std_formatter
let _ = Callback.register "prop_pp" prop_pp
 
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

type assignment = Prop.Assignment.t

let assignment_pp = Prop.Assignment.pp Format.std_formatter
let _ = Callback.register "assignment_pp" assignment_pp

let assignment_valuation x = x.Prop.Assignment.valuation
let _ = Callback.register "assignment_valuation" assignment_valuation

let assignment_literals x = failwith "to do" (* x.Prop.Assignment.literals *)
let _ = Callback.register "assignment_literals" assignment_literals

let prop_sat s p =
  match Prop.sat s p with
    | None -> None
    | Some(rho, _) -> Some(rho)
let _ = Callback.register "prop_sat" prop_sat

let term_mk_mult = Pprod.mk_mult
let _ = Callback.register "term_mk_mult" term_mk_mult

let rec term_mk_multl = function
  | [] -> Arith.mk_one()
  | [a] -> a
  | a :: b :: l -> term_mk_multl (term_mk_mult a b :: l)
let _ = Callback.register "term_mk_multl" term_mk_multl


let term_mk_create  = Funarr.mk_create
let _ = Callback.register "term_mk_create" term_mk_create

let term_mk_update  = Funarr.mk_update Term.is_equal
let _ = Callback.register "term_mk_update" term_mk_update

let term_mk_select = Funarr.mk_select Term.is_equal
let _ = Callback.register "term_mk_select" term_mk_select

let term_mk_apply = Apply.mk_apply
let _ = Callback.register "term_mk_apply" term_mk_apply


(** {6 Set of terms} *)

type terms = Term.Set.t

let terms_to_list = Term.Set.elements
let _ = Callback.register "terms_to_list" terms_to_list

let terms_of_list = List.fold_left (fun acc a -> Term.Set.add a acc) Term.Set.empty
let _ = Callback.register "terms_of_list" terms_of_list


(* {6 Term maps} *)

type 'a map = 'a Term.Map.t
	  

(** Equalities. *)
let term_eq = Term.eq
let _ = Callback.register "term_eq" term_eq

let term_cmp = Term.cmp
let _ = Callback.register "term_cmp" term_cmp


(** {6 Tracing} *)
    
type trace_level = string

let trace_reset = Trace.reset
let _ = Callback.register "trace_reset" trace_reset

let trace_add = Trace.add
let _ = Callback.register "trace_add" trace_add

let trace_remove = Trace.add
let _ = Callback.register "trace_remove" trace_remove

let trace_get = Trace.get
let _ = Callback.register "trace_get" trace_get



(** {6 Justifications} *)

type justification = Jst.t

let justification_pp = Jst.pp Format.std_formatter
let _ = Callback.register "justification_pp" justification_pp


(** {6 Logical contexts} *)

type context = Context.t

let context_eq = Context.eq
let _ = Callback.register "context_eq" context_eq  

let context_empty () =
  license_check(); Context.empty
let _ = Callback.register "context_empty" context_empty

let context_ctxt_of s = Context.ctxt_of s
let _ = Callback.register "context_ctxt_of" context_ctxt_of

let context_use s = failwith "to do"
let _ = Callback.register "context_use" context_use

let context_inv i s a = failwith "to do"
(*
  let (bs, _) = Context.inv s a in
    bs
*)
let _ = Callback.register "context_inv" context_inv

let context_find s = failwith "to do" (* Context.find *)
let _ = Callback.register "context_find" context_find

let context_apply s = failwith "to do" (* Context.apply *)
let _ = Callback.register "context_apply" context_apply

let context_mem i s = failwith "to do"
let _ = Callback.register "context_mem" context_mem

let context_pp s = Context.pp Format.std_formatter s; Format.print_flush()
let _ = Callback.register "context_pp" context_pp

let context_ctxt_pp s = 
  let al = Context.ctxt_of s in
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

let is_redundant = function
  | Context.Status.Valid _ -> true
  | _ -> false
let _ = Callback.register "is_redundant" is_redundant

let is_inconsistent = function
  | Context.Status.Inconsistent _ -> true
  | _ -> false
let _ = Callback.register "is_inconsistent" is_inconsistent  

let d_consistent r =
  match r with
    | Context.Status.Ok s -> s
    | _ -> (context_empty())
         (* failwith "Ics.d_consistent: fatal error" *)
	
let _ = Callback.register "d_consistent" d_consistent 

let process = Context.add
let _ = Callback.register "process" process   

let split s = failwith "to do"
let _ = Callback.register "split" split


(** Normalization functions *)

let can s = Combine.can (Context.config_of s)
let _ = Callback.register "can" can

let set_outchannel formatter =
  Istate.outchannel := formatter
let _ = Callback.register "set_outchannel" set_outchannel

let set_inchannel inch =
  Istate.inchannel := inch
let _ = Callback.register "set_inchannel" set_inchannel

let set_prompt str = Istate.prompt := str
let _ = Callback.register "set_prompt" set_prompt

let set_eot str = Istate.eot := str
let _ = Callback.register "set_eot" set_eot

(** Read-Eval-Print loop. *)
let rec cmd_rep () =
  Istate.batch := false;
  try
    while true do
      Tools.linenumber := 0;    (* for error message *)
      try
	Istate.print_prompt ();
	Parser.commands Lexer.token (Istate.lexing())
      with
	  Parsing.Parse_error -> Istate.do_parse_error !Tools.linenumber;	  
    done 
  with
    | End_of_file -> 
	Format.eprintf "\n:%s@." (Printexc.to_string End_of_file);
	Istate.do_quit 0
    | Sys.Break -> 
	Format.eprintf "\n:%s@." (Printexc.to_string Sys.Break);
	if !Istate.batch then
	  Istate.do_quit 1
	else 
	  cmd_rep ()
    | Failure("drop") -> 
	()
    | exc -> 
	Istate.do_error ("Exception " ^ (Printexc.to_string exc));
	cmd_rep ()

(** Batch processor. *)
and cmd_batch (inch) =
  Istate.batch := true;
  Tools.linenumber := 1;
  Istate.inchannel := inch;
  try
    while true do
      Parser.commandsequence Lexer.token (Istate.lexing())
    done;
    failwith "unreachable"
  with
    | Parsing.Parse_error ->
	Format.fprintf !Istate.outchannel ":error (Parse error on line %d)@." !Tools.linenumber;
	2
    | End_of_file -> 
	0
    | Sys.Break -> 
	Format.eprintf "\n:%s@." (Printexc.to_string Sys.Break);
	1
    | exc ->   
	Format.fprintf !Istate.outchannel ":error %s@." (Printexc.to_string exc); 
	(-2)

let _ = Callback.register "cmd_rep" cmd_rep
let _ = Callback.register "cmd_batch" cmd_batch


(** Abstract sign interpretation. *)

let dom s = Combine.dom (Context.config_of s)
 

(** {6 Tools} *)

let reset () = Tools.do_at_reset ()
let _ = Callback.register "reset" reset

let gc () = Gc.full_major ()
let _ = Callback.register "gc" gc

let flush = print_flush
let _ = Callback.register "flush" flush

(** Sleeping. *)
let sleep = Unix.sleep
let _ = Callback.register "sleep" sleep



(** {6 Lists} *)

let is_nil = function [] -> true | _ -> false
let _ = Callback.register "is_nil" is_nil

let cons x l = x :: l
let _ = Callback.register "cons" cons

let head = List.hd
let _ = Callback.register "head" head

let tail = List.tl
let _ = Callback.register "tail" tail


(** {6 Pairs} *)

let pair x y = (x,y)
let _ = Callback.register "pair" pair

let fst = fst
let _ = Callback.register "fst" fst

let snd = snd
let _ = Callback.register "snd" snd


(** {6 Triples} *)

let triple x y z = (x,y,z)
let _ = Callback.register "triple" triple

let fst_of_triple = function (x,_,_) -> x
let _ = Callback.register "fst_of_triple" fst_of_triple

let snd_of_triple = function (_,y,_) -> y
let _ = Callback.register "snd_of_triple" snd_of_triple

let third_of_triple = function (_,_,z) -> z
let _ = Callback.register "third_of_triple" third_of_triple
  

(** {6 Quadruples} *)
   
let fst_of_quadruple  = function (x1,_,_,_) -> x1
let _ = Callback.register "fst_of_quadruple" fst_of_quadruple

let snd_of_quadruple = function (_,x2,_,_) -> x2
let _ = Callback.register "snd_of_quadruple" snd_of_quadruple

let third_of_quadruple = function (_,_,x3,_) -> x3
let _ = Callback.register "third_of_quadruple" third_of_quadruple

let fourth_of_quadruple = function (_,_,_,x4) -> x4
let _ = Callback.register "fourth_of_quadruple" fourth_of_quadruple
    

(** {6 Options} *)

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


	 
(** {6 Multi-precision arithmetic} *)

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
