/*
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
 */

#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>


/* Registration and deregistration of values */

void icsat_register(value* r) {
  register_global_root(r);
}

void icsat_deregister(value* r) {
  if (((*r) & 1) == 0) { 
     remove_global_root(r); 
  }
}

/* Lists */

int icsat_is_nil(value x1) {
  CAMLparam1(x1);
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_is_nil"); }
  CAMLreturn(Bool_val(callback(*closure, x1)));
}

int icsat_length(value x1) {
  CAMLparam1(x1);
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_length"); }
  CAMLreturn(Int_val(callback(*closure, x1)));
}

int icsat_head(value x1) {
  CAMLparam1(x1);
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_head"); }
  CAMLreturn(Int_val(callback(*closure, x1)));
}

value icsat_tail(value x1) {
  CAMLparam1(x1);
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_tail"); }
  CAMLreturn((value)(callback(*closure, x1)));
}

/* Explanation function returns a list of atoms. Should only be called immediately after */
/* inconsistent processing. */

value icsat_explain(value x1) {
  CAMLparam1(x1);
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_explain"); }
  CAMLreturn((value)(callback(*closure, x1)));
}


/* Stack manipulations */

void icsat_reset() {
  CAMLparam0();
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_reset"); }
  callback(*closure, Val_unit);
  CAMLreturn0;
}

void icsat_push() {
  CAMLparam0();
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_dup"); }
  callback(*closure, Val_unit);
  CAMLreturn0;
}

void icsat_pop() {
  CAMLparam0();
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_pop"); }
  callback(*closure, Val_unit);
  CAMLreturn0;
}

void icsat_stackpp() {
  CAMLparam0();
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_stackpp"); }
  callback(*closure, Val_unit);
  CAMLreturn0;
}

int icsat_assert(int x1) {
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_add"); }
  return(Int_val(callback(*closure, Val_int(x1))));
}


void icsat_reset_scratch_context() {
  CAMLparam0();
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("reset_scratch_context"); }
  callback(*closure, Val_unit);
  CAMLreturn0;
}

int icsat_add_scratch_context(int x1) {
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("add_scratch_context"); }
  return(Int_val(callback(*closure, Val_int(x1))));
}

/* Atoms. */

int icsat_is_connected(int x1, int x2) {
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("atom_is_connected"); }
  return(Bool_val(callback2(*closure, Val_int(x1), Val_int(x2))));
}

void icsat_atom_pp(int x1) {
  static value * closure = NULL;
  if (closure == NULL) { closure = caml_named_value("prop_atom_pp"); }
  callback(*closure, Val_int(x1));
}


/* Propositional Constructors */
typedef int LPFormulaId;
extern void sat_initialize();
extern void sat_finalize();
extern LPFormulaId sat_mk_true();
extern LPFormulaId sat_mk_false();
extern LPFormulaId sat_mk_or(unsigned int num_args, LPFormulaId * args);
extern LPFormulaId sat_mk_and(unsigned int num_args, LPFormulaId * args);
extern LPFormulaId sat_mk_iff(LPFormulaId lhs, LPFormulaId rhs);
extern LPFormulaId sat_mk_implies(LPFormulaId lhs, LPFormulaId rhs);
extern LPFormulaId sat_mk_xor(LPFormulaId lhs, LPFormulaId rhs);
extern LPFormulaId sat_mk_not(LPFormulaId f);
extern LPFormulaId sat_mk_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e);
extern LPFormulaId sat_mk_atom(value a, value not_a);
extern LPFormulaId sat_mk_var(char * var);
extern int sat_is_true(LPFormulaId f);
extern int sat_is_false(LPFormulaId f);
extern int sat_is_not(LPFormulaId f);
extern int sat_is_or(LPFormulaId f);
extern int sat_is_iff(LPFormulaId f);
extern int sat_is_ite(LPFormulaId f);
extern int sat_is_var(LPFormulaId f);
extern int sat_is_atom(LPFormulaId f);
extern LPFormulaId sat_d_not(LPFormulaId f);
extern char * sat_d_var(LPFormulaId f);
extern value sat_d_atom(LPFormulaId f);
extern int sat_num_arguments(LPFormulaId f);
extern LPFormulaId sat_get_argument(LPFormulaId f, int idx);

void icsat_initialize() {
  CAMLparam0();
  sat_initialize();
  CAMLreturn0;
}

void icsat_finalize() {
  CAMLparam0();
  sat_finalize();
  CAMLreturn0;
}

value icsat_mk_true() {
  CAMLparam0();
  CAMLreturn(Val_int(sat_mk_true()));
}

value icsat_mk_false() {
  CAMLparam0();
  CAMLreturn(Val_int(sat_mk_false()));
}

value icsat_mk_var(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_mk_var(String_val(x1))));
}

value icsat_mk_atom(value x1, value x2) {
  CAMLparam2(x1, x2);
  CAMLreturn(Val_int(sat_mk_atom(Int_val(x1), Int_val(x2))));
}

LPFormulaId * tmp_array = NULL;
int array_size = 0;

void set_array_size(int size) {
  if (size > array_size) {
    if (tmp_array != NULL)
      free(tmp_array);
    tmp_array = (LPFormulaId *) malloc(sizeof(LPFormulaId)*size);
    array_size = size;
  }
}

value icsat_mk_or(value x1) {  /* to do */
  CAMLparam1(x1);
  int len = icsat_length(x1);
  int i;
  set_array_size(len);
  for(i = 0; i < len; i++) {
    tmp_array[i] = icsat_head(x1);
    x1 = icsat_tail(x1);
  }
  CAMLreturn(Val_int(sat_mk_or(len, tmp_array)));
}

value icsat_mk_and(value x1) {
  CAMLparam1(x1);
  int len = icsat_length(x1);
  int i;
  set_array_size(len);
  for(i = 0; i < len; i++) {
    tmp_array[i] = icsat_head(x1);
    x1 = icsat_tail(x1);
  }
  CAMLreturn(Val_int(sat_mk_and(len, tmp_array)));
}

value icsat_mk_iff(value x1, value x2) {
  CAMLparam2(x1, x2);
  CAMLreturn(Val_int(sat_mk_iff(Int_val(x1), Int_val(x2))));
}

value icsat_mk_implies(value x1, value x2) {
  CAMLparam2(x1, x2);
  CAMLreturn(Val_int(sat_mk_implies(Int_val(x1), Int_val(x2))));
}

value icsat_mk_xor(value x1, value x2) {
  CAMLparam2(x1, x2);
  CAMLreturn(Val_int(sat_mk_xor(Int_val(x1), Int_val(x2))));
}

value icsat_mk_not(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_mk_not(Int_val(x1))));
}

value icsat_mk_ite(value x1, value x2, value x3) {
  CAMLparam3(x1, x2, x3);
  CAMLreturn(Val_int(sat_mk_ite(Int_val(x1), Int_val(x2), Int_val(x3))));
}


/* Propositional Recognizers */

int icsat_is_true(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_true(Int_val(x1))));
}

int icsat_is_false(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_false(Int_val(x1))));
}

int icsat_is_not(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_not(Int_val(x1))));
}

int icsat_is_or(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_or(Int_val(x1))));
}

int icsat_is_iff(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_iff(Int_val(x1))));
}

int icsat_is_ite(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_ite(Int_val(x1))));
}

int icsat_is_var(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_var(Int_val(x1))));
}

int icsat_is_atom(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Bool_val(sat_is_atom(Int_val(x1))));
}


/* Propositional Destructors */


value icsat_d_var(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_d_var(Int_val(x1))));
}

value icsat_d_atom(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_d_atom(Int_val(x1))));
}

value icsat_d_not(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_d_not(Int_val(x1))));
}

value icsat_num_arguments(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_num_arguments(Int_val(x1))));
}

value icsat_get_argument(value x1, value x2) {
  CAMLparam2(x1, x2);
  CAMLreturn(Val_int(sat_get_argument(Int_val(x1), Int_val(x2))));
}





/** following has to be filled in with ICSAT */

extern int ics_sat(LPFormulaId);

value icsat_sat (value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_bool(ics_sat(Int_val(x1))));
}


/** Parameter settings for SAT solver */

extern void sat_set_verbose(int);

void icsat_set_verbose(value x1) {
  CAMLparam1(x1);
  sat_set_verbose(Bool_val(x1));
  CAMLreturn0;
}

extern void sat_set_remove_subsumed_clauses(int);

void icsat_set_remove_subsumed_clauses(value x1) {
  CAMLparam1(x1);
  sat_set_remove_subsumed_clauses(Bool_val(x1));
  CAMLreturn0;
}

extern void sat_set_validate_counter_example(int);

void icsat_set_validate_counter_example(value x1) {
  CAMLparam1(x1);
  sat_set_validate_counter_example(Bool_val(x1));
  CAMLreturn0;
}

extern void  sat_set_polarity_optimization(int);

void icsat_set_polarity_optimization(value x1) {
  CAMLparam1(x1);
  sat_set_polarity_optimization(Bool_val(x1));
  CAMLreturn0;
}

extern void  sat_set_clause_relevance(int);

void icsat_set_clause_relevance(value x1) {
  CAMLparam1(x1);
  sat_set_clause_relevance(Int_val(x1));
  CAMLreturn0;
}

extern void sat_set_num_refinements(int);

void icsat_set_num_refinements(value x1) {
  CAMLparam1(x1);
  sat_set_num_refinements(Int_val(x1));
  CAMLreturn0;
}

extern void sat_set_cleanup_period(int);

void icsat_set_cleanup_period(value x1) {
  CAMLparam1(x1);
  sat_set_cleanup_period(Int_val(x1));
  CAMLreturn0;
}

extern int sat_get_assignment(LPFormulaId);

value icsat_get_assignment(value x1) {
  CAMLparam1(x1);
  CAMLreturn(Val_int(sat_get_assignment(Int_val(x1))));
}

extern void sat_print_statistics();

void icsat_print_statistics() {
  CAMLparam0();
  sat_print_statistics();
  CAMLreturn0;
}



