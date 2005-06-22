/*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
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


extern void ics_error(char *, char *);

typedef value* rat;
typedef value* termvar;
typedef value* funsym;
typedef value* term;
typedef value* propvar;
typedef value* predsym;
typedef value* formula;
typedef value* context;
typedef value* state;


int ics_started = 0;
void ics_init() {
  static value* version_closure = NULL;
  char ** a;
    if (ics_started) {
      fprintf(stderr, "Error: ICS runtime already started.\n");
      return;
    }
  a = malloc(2*sizeof(char **));
  a[0] = malloc(4);
  a[0][0] = 'i';
  a[0][1] = 'c';
  a[0][2] = 's';
  a[0][3] = (char) NULL;
  a[1] = NULL;
  ics_started = 1;
  caml_startup(a);
  if (version_closure == NULL) 
      version_closure = caml_named_value("ics_version"); 
  fprintf(stderr, "%s", String_val(callback(*version_closure, Val_unit)));
  fflush(stderr);
  return;
}

#define CHECKERROR(funname, result) \
   if (Is_exception_result(result)) \
       ics_error((funname), \
                 (char*)caml_format_exception(Extract_exception(result))) 

#define CHECKMALLOC(ptr) \
   if((ptr) == NULL) ics_error("ics_malloc", "failed to allocate memory")

#define UNIT_TO_STRING(funname) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = callback_exn(*closure, Val_unit); \
  CHECKERROR((funname), (result)); \
  return(strdup(String_val(result)))

#define UNIT_TO_UNIT(funname) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = callback_exn(*closure, Val_unit); \
  CHECKERROR((funname), (result)); \
  return

#define UNIT_TO_BOOL(funname) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = callback_exn(*closure, Val_unit); \
  CHECKERROR((funname), (result)); \
  return(Bool_val(result))

#define UNIT_TO_INT(funname) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = callback_exn(*closure, Val_unit); \
  CHECKERROR((funname), (result)); \
  return(Int_val(result))


#define STRING_TO_VALUE(funname, stringarg) \
  value result = (value)NULL; \
  value* resptr = (value*)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, caml_copy_string(stringarg)); \
  CHECKERROR((funname), result); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define VALUE_TO_STRING(funname, valptr) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, *(valptr)); \
  CHECKERROR((funname), (result)); \
  return(strdup(String_val(result)))

#define VALUE_TO_INT(funname, valptr) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, *(valptr)); \
  CHECKERROR((funname), (result)); \
  return(Int_val(result))

#define INTxVALUE_TO_INT(funname, n, valptr) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback2_exn(*closure, Val_int(n), *(valptr)); \
  CHECKERROR((funname), (result)); \
  return(Int_val(result))

#define INTxINTxVALUE_TO_VALUE(funname, n, m, valptr) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback3_exn(*closure, Val_int(n), Val_int(m), *(valptr)); \
  CHECKERROR((funname), (result)); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define INTxVALUE_TO_VALUE(funname, n, valptr) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback2_exn(*closure, Val_int(n), *(valptr)); \
  CHECKERROR((funname), (result)); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define VALUE_TO_BOOL(funname, valptr) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, *(valptr)); \
  CHECKERROR((funname), (result)); \
  return(Bool_val(result))

#define INT_TO_VALUE(funname, n) \
 value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, Val_int(n)); \
  CHECKERROR((funname), (result)); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define VALUE2_TO_INT(funname, valptr1, valptr2) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback2_exn(*closure, *(valptr1), *(valptr2)); \
  CHECKERROR((funname), (result)); \
  return(Int_val(result))

#define VALUE_TO_UNIT(funname, valptr) \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, *(valptr)); \
  CHECKERROR((funname), result); \
  return

#define UNIT_TO_VALUE(funname) \
 value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, Val_unit); \
  CHECKERROR((funname), (result)); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define BINREL(funname, valptr1, valptr2) \
  value result = (value)NULL;  \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname);  \
  result = caml_callback2_exn(*closure, *(valptr1), *(valptr2)); \
  CHECKERROR((funname), (result)); \
  return(Bool_val(result))

#define VALUE_TO_VALUE(funname, valptr) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback_exn(*closure, *(valptr)); \
  CHECKERROR((funname), result); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define VALUE2_TO_VALUE(funname, valptr1, valptr2) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback2_exn(*closure, *(valptr1), *(valptr2)); \
  CHECKERROR((funname), result); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define VALUE3_TO_VALUE(funname, valptr1, valptr2, valptr3) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback3_exn(*closure, *(valptr1), *(valptr2), *(valptr3)); \
  CHECKERROR((funname), result); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

#define INT2_TO_VALUE(funname, n, m) \
  value* resptr = (value*)NULL; \
  value result = (value)NULL; \
  static value*closure = NULL; \
  if (closure == NULL) closure = caml_named_value(funname); \
  result = caml_callback2_exn(*closure, Val_int(n), Val_int(m)); \
  CHECKERROR((funname), result); \
  resptr = malloc(sizeof(value*)); \
  CHECKMALLOC(resptr); \
  *resptr = result; \
  register_global_root(resptr); \
  return(resptr)

char* ics_version() { UNIT_TO_STRING("ics_version"); }

void ics_gc() { UNIT_TO_UNIT("ics_gc"); }
void ics_flush() { UNIT_TO_UNIT("ics_flush"); }

void ics_free(value* r) {
  if (r && (((*r) & 1) == 0)) { 
    remove_global_root(r); 
    free(r);
  }
}

rat ics_rat_of_int(int n) { INT_TO_VALUE("ics_q_of_int", n); }
rat ics_rat_make(int n, int d) { INT2_TO_VALUE("ics_q_make", n, d); }
char* ics_rat_to_string(rat q) { VALUE_TO_STRING("ics_q_to_string", q); }
rat ics_rat_add(rat q, rat p) { VALUE2_TO_VALUE("ics_q_add", q, p); }
rat ics_rat_sub(rat q, rat p) { VALUE2_TO_VALUE("ics_q_sub", q, p); }
rat ics_rat_minus(rat q) { VALUE_TO_VALUE("ics_q_minus", q); }
rat ics_rat_mult(rat q, rat p) { VALUE2_TO_VALUE("ics_q_mult", q, p); }
rat ics_rat_div(rat q, rat p) { VALUE2_TO_VALUE("ics_q_div", q, p); }
rat ics_rat_inv(rat q) { VALUE_TO_VALUE("ics_q_inv", q); }
int ics_rat_compare(rat q, rat p) { VALUE2_TO_INT("ics_q_compare", q, p); }
int ics_rat_denominator(rat q) { VALUE_TO_INT("ics_q_denumerator", q); }
int ics_rat_numerator(rat q) { VALUE_TO_INT("ics_q_numerator", q); }



termvar ics_var_of_string(char* name) { STRING_TO_VALUE("ics_var_of_string", name);}
char* ics_var_to_string(termvar x) { VALUE_TO_STRING("ics_var_to_string", x); }
int ics_var_equal(termvar x, termvar y) { BINREL("ics_var_equal", x, y); }
int ics_var_compare(termvar x, termvar y) { VALUE2_TO_INT("ics_var_equal", x, y); }

funsym ics_funsym_of_string(char* name) { STRING_TO_VALUE("ics_funsym_of_string", name); }
int ics_funsym_compare(funsym f, funsym g) { VALUE2_TO_INT("ics_funsym_compare",f,g); }
char* ics_funsym_to_string(funsym f) { VALUE_TO_STRING("ics_funsym_to_string", f); }

#define VAR 0
#define UNINTERP 1
#define POLYNOMIAL 2
#define PROJECTION 3
#define TUPLE 4
#define UPDATE 5
#define LOOKUP 6
#define NUM 7
#define MONOMIAL 8

int ics_term_kind(term t) { 
  VALUE_TO_INT("ics_term_kind", t); }

int ics_term_arity(term t) { 
  VALUE_TO_INT("ics_term_arity", t); }

term ics_term_arg(int pos, term t) {  
  INTxVALUE_TO_VALUE("ics_term_arg", pos, t); }

int ics_term_compare(term s, term t) { 
  BINREL("ics_term_compare", s, t); }

char* ics_term_to_string(term t) { 
  VALUE_TO_STRING("ics_term_to_string", t); }

term ics_term_mk_var(termvar x) { 
  VALUE_TO_VALUE("ics_term_mk_var", x); }

term ics_term_mk_int(int n) { 
  INT_TO_VALUE("ics_term_mk_int", n); }

term ics_term_mk_rat(int n, int d) { 
  INT2_TO_VALUE("ics_term_mk_rat", n, d); }

term ics_term_mk_multz(int n, term t) { 
  INTxVALUE_TO_VALUE("ics_term_mk_multz", n, t); }

term ics_term_mk_multq(rat q, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_multq", q, t); } 

term ics_term_mk_add(term s, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_add", s, t); }

term ics_term_mk_minus(term s) { 
  VALUE_TO_VALUE("ics_term_mk_minus", s); }

term ics_term_mk_sub(term s, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_sub", s, t); }

term ics_term_mk_nil() { UNIT_TO_VALUE("ics_term_mk_nil"); }

term ics_term_mk_pair(term s, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_pair", s, t); }

term ics_term_mk_proj(int i, int n, term t) { 
  INTxINTxVALUE_TO_VALUE("ics_term_mk_proj", i, n, t); }

term ics_term_mk_apply(funsym f, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_apply", f, t); }

term ics_term_mk_lookup(term s, term t) { 
  VALUE2_TO_VALUE("ics_term_mk_lookup", s, t); }

term ics_term_mk_update(term s1, term s2, term s3) { 
  VALUE3_TO_VALUE("ics_term_mk_update", s1, s2, s3); }

propvar ics_propvar_of_string(char* name) { 
  STRING_TO_VALUE("ics_propvar_of_string", name);}

char* ics_propvar_to_string(propvar x) { 
  VALUE_TO_STRING("ics_propvar_to_string", x); }

int ics_propvar_compare(propvar x, propvar y) { 
  VALUE2_TO_INT("ics_propvar_equal", x, y); }


predsym ics_predsym_of_string(char* name) { 
  STRING_TO_VALUE("ics_predsym_of_string", name); }

int ics_predsym_compare(predsym f, predsym g) { 
  VALUE2_TO_INT("ics_predsym_compare",f,g); }

char* ics_predsym_to_string(predsym f) { 
  VALUE_TO_STRING("ics_predsym_to_string",f); }


#define TT 0
#define FF 1
#define POSVAR 2
#define NEGVAR 3
#define EQUAL 4
#define DISEQ 5
#define NONNEG_PRED 6
#define POS_PRED 7
#define REAL_PRED 8
#define ITE 9
#define UNINTERP_PRED 10
#define TUPLE_PRED 11
#define ARRAY_PRED 12
#define NEGLIT

int ics_formula_find(formula p) { 
  VALUE_TO_INT("ics_formula_kind", p);
}


int ics_formula_compare(formula p, formula q) { 
  BINREL("ics_formula_compare", p, q);
}

char* ics_formula_to_string(formula p) {
  VALUE_TO_STRING("ics_formula_to_string", p);
}

formula ics_formula_mk_tt() {
  UNIT_TO_VALUE("ics_formula_mk_tt");
}

formula ics_formula_mk_ff() {
  UNIT_TO_VALUE("ics_formula_mk_ff");
}

formula ics_formula_mk_posvar(propvar p) {
  VALUE_TO_VALUE("ics_formula_mk_posvar", p);
}

formula ics_formula_mk_negvar(propvar p) {
  VALUE_TO_VALUE("ics_formula_mk_negvar", p);
}

formula ics_formula_mk_poslit(predsym p, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_poslit", p, t);
}

formula ics_formula_mk_neglit(predsym p, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_neglit", p, t);
}

formula ics_formula_mk_eq(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_eq", s, t);
}

formula ics_formula_mk_deq(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_deq", s, t);
}

formula ics_formula_mk_nonneg(term t) {
  VALUE_TO_VALUE("ics_formula_mk_nonneg", t);
}

formula ics_formula_mk_pos(term t) {
  VALUE_TO_VALUE("ics_formula_mk_pos", t);
}

formula ics_formula_mk_ge(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_ge", s, t);
}

formula ics_formula_mk_gt(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_gt", s, t);
}

formula ics_formula_mk_lt(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_lt", s, t);
}

formula ics_formula_mk_le(term s, term t) {
  VALUE2_TO_VALUE("ics_formula_mk_le", s, t);
}

formula ics_formula_mk_real(term t) {
  VALUE_TO_VALUE("ics_formula_mk_real", t);
}

formula ics_formula_mk_integer(term t) {
  VALUE_TO_VALUE("ics_formula_mk_integer", t);
}

formula ics_formula_mk_not(formula p) {
  VALUE_TO_VALUE("ics_formula_mk_not", p);
}

formula ics_formula_mk_andthen(formula p, formula q) {
  VALUE2_TO_VALUE("ics_formula_mk_andthen", p, q);
}

formula ics_formula_mk_orelse(formula p, formula q) {
  VALUE2_TO_VALUE("ics_formula_mk_orelse", p, q);
}

formula ics_formula_mk_implies(formula p, formula q) {
  VALUE2_TO_VALUE("ics_formula_mk_implies", p, q);
}

formula ics_formula_mk_equiv(formula p, formula q) {
  VALUE2_TO_VALUE("ics_formula_mk_equiv", p, q);
}

formula ics_formula_mk_ite(formula p, formula q, formula r) {
  VALUE3_TO_VALUE("ics_formula_mk_ite", p, q, r);
}

int ics_context_is_empty(context ctxt) { 
  VALUE_TO_BOOL("ics_context_is_empty", ctxt);
}

formula ics_context_head(context ctxt) { 
  VALUE_TO_VALUE("ics_context_head", ctxt);
}

context ics_context_tail(context ctxt) { 
  VALUE_TO_VALUE("ics_context_tail", ctxt);
}


#define UNKNOWN 0
#define SAT 1
#define UNSAT 2

int ics_status() { UNIT_TO_INT("ics_status"); }
context ics_context() { UNIT_TO_VALUE("ics_context"); }
int ics_is_unchanged() { UNIT_TO_BOOL("ics_is_unchanged"); }
void ics_initialize(state s) { VALUE_TO_UNIT("ics_initialize", s); }
state ics_finalize() { UNIT_TO_VALUE("ics_finalize"); }
void ics_reset() { UNIT_TO_UNIT("ics_reset"); }
void ics_process(formula fml) { VALUE_TO_UNIT("ics_process", fml); }
void ics_resolve() { UNIT_TO_UNIT("ics_resolve"); }
context ics_explain() { UNIT_TO_VALUE("ics_explain"); }
term ics_can(term t) { VALUE_TO_VALUE("ics_can", t); }
rat ics_sup(term t) { VALUE_TO_VALUE("ics_sup", t); }
rat ics_inf(term t) { VALUE_TO_VALUE("ics_inf", t); }
context ics_var_eqs() { UNIT_TO_VALUE("ics_var_eqs"); }
context ics_var_diseqs() { UNIT_TO_VALUE("ics_var_diseqs"); }
context ics_constant_equals() { UNIT_TO_VALUE("ics_constant_equals"); }
context ics_regular_equals() { UNIT_TO_VALUE("ics_regular_equals"); }
context ics_tableau_equals() { UNIT_TO_VALUE("ics_tableau_equals"); }
context ics_tuple_equals() { UNIT_TO_VALUE("ics_tuple_equals"); }
context ics_array_equals() { UNIT_TO_VALUE("ics_array_equals"); }
context ics_uninterp_equals() { UNIT_TO_VALUE("ics_uninterp_equals"); }
context ics_literals() { UNIT_TO_VALUE("ics_literals"); }
context ics_renames() { UNIT_TO_VALUE("ics_renames"); }
formula ics_prop() { UNIT_TO_VALUE("ics_prop"); }
