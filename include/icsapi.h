/* 
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
 */

/* ICS Programming interface.

  Author: Harald Ruess

  The interface is functional in that none of the arguments 
  to function calls is modified.
 */

#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

/* Syntactic entities of ICS:
   rat:     arbitrary precision rationals
   termvar: term variables
   funsym:  uninterpreted function symbols
   term:    injection of term variables or term applications
   propvar: propositional variables
   predsym: monadic predicate symbols
   formula: propositional constraint formulas
   context: logical context
   state:   decision procedure states including logical context */ 
typedef value* rat;
typedef value* termvar;
typedef value* funsym;
typedef value* term;
typedef value* propvar;
typedef value* predsym;
typedef value* formula;
typedef value* context;
typedef value* state;

extern void ics_error(char* funname, char* errormsg);
/* Application-specific error signaling. 
   [ics_error] must never return normally. */

void ics_init();
/* Initialize the ICS runtime system. This function
   must be called before any other function in this API. */

char* ics_version();
/* Return the current ICS version. */

void ics_free(value* v);
/* [ics_free v] schedules an ICS value [v] for garbage collection.
  In particular, the memory location for [v] is not 
  necessarily freed immediately. */

void ics_gc();
/* Explicitly invoke garbage collection. This is usually 
   not needed as garbage collection is invoked implicitly. */


/*** Arbitrary-precision rationals ***/

rat ics_rat_of_int(int n);
/* Construct a rational for the integer [n]. */

rat ics_rat_make(int n, int d);
/* Construct a rational [n/d] with [d <> 0]. */

char* ics_rat_to_string(rat q);
/* Print a rational to a string. */

rat ics_rat_add(rat q, rat p);
/* Return [q + p]. */

rat ics_rat_sub(rat q, rat p);
/* Subtract [p] from [q]. */

rat ics_rat_minus(rat q);
/* Negate rational [q]. */

rat ics_rat_div(rat q, rat p);
/* Return [q/p] for [p /= 0]. Otherwise [ics_error] is called. */

rat ics_rat_inv(rat q);
/* Return [1/q] if [q/=0]. Otherwise [ics_error] is called. */

int ics_rat_compare(rat q, rat p);
/* Result is [0] if [q = p], negative if [q < p], and positive if [q > p]. */


/*** Variables ***/

termvar ics_var_of_string(char* name);
/* Construct an ICS term variable of given name. */

char* ics_var_to_string(termvar x);
/* Returns the name associated with a term variable. */

int ics_var_equal(termvar x, termvar y);
/* Equality test for variables. */

int ics_var_compare(termvar x, termvar y);
/* Total ordering [<] on variables with [x < y] iff
   the [ics_var_compare x y] is negative. Result is [0] iff the 
   argument variables are equal. */ 


/*** Uninterpreted function symbols. */

funsym ics_funsym_of_string(char* name);
/** Construct an uninterpreted function symbol. */

int ics_funsym_compare(funsym f, funsym g);
/* Total ordering [<] on function symbols with [f < g] iff
   the [ics_funsym_compare f g] is negative. 
   Result is [0] iff the argument function symbols are equal. */ 

char* ics_funsym_to_string(funsym f);
/* Returns the name of the function symbol. */


/*** Terms ***/

#define VAR 0
#define UNINTERP 1
#define POLYNOMIAL 2
#define PROJECTION 3
#define TUPLE 4
#define UPDATE 5
#define LOOKUP 6
#define NUM 7
#define MONOMIAL 8

int ics_term_kind(term t);
int ics_term_arity(term t);
int ics_term_arg(int pos, term t);
int ics_term_compare(term s, term t);
char* ics_term_to_string(term s);
term ics_term_mk_var(termvar x);
term ics_term_mk_int(int n);
term ics_term_mk_rat(int n, int d);
term ics_term_mk_multz(int n, term t);
term ics_term_mk_multq(rat q, term t);
term ics_term_mk_add(term s, term t);
term ics_term_mk_sub(term s, term t);
term ics_term_mk_nil();
term ics_term_mk_pair(term s, term t);
term ics_term_mk_proj(int i, int n, term t);
term ics_term_mk_apply(funsym f, term t);
term ics_term_mk_lookup(term s, term t);
term ics_term_mk_update(term s1, term s2, term t);


/*** Propositional variables */

propvar ics_propvar_of_string(char* name);
/* Construct a proposition variable with associated name [name]. */

char* ics_propvar_to_string(propvar x);
/* Return the name associated with a propositional variable. */

int ics_propvar_compare(propvar x, propvar y);
/* Total ordering [<] on propositional variables with [p < q] 
   iff the [ics_propvar_compare p q] is negative.  Result is [0] 
   iff the argument function symbols are equal. Note: this 
   ordering does not necessarily respect the natural ordering
   on associated names. Moreover, the ordering might be
   different on different runs of ICS. */


/** Uninterpreted predicate symbols */

predsym ics_predsym_of_string(char* name);
int ics_predsym_compare(predsym f, predsym g);
char* ics_predsym_to_string(predsym f);


/*** Propositional formulas ***/

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

int ics_formula_kind(formula p);
int ics_formula_compare(formula p, formula q);
char* ics_formula_to_string(formula p);
formula ics_formula_mk_posvar(propvar p);
formula ics_formula_mk_negvar(propvar p);
formula ics_formula_mk_poslit(predsym p, term t);
formula ics_formula_mk_neglit(predsym p, term t);
formula ics_formula_mk_eq(term s, term t);
formula ics_formula_mk_deq(term s, term t);
formula ics_formula_mk_nonneg(term t);
formula ics_formula_mk_pos(term t);
formula ics_formula_mk_ge(term s, term t);
formula ics_formula_mk_gt(term s, term t);
formula ics_formula_mk_lt(term s, term t);
formula ics_formula_mk_le(term s, term t);
formula ics_formula_mk_real(term t);
formula ics_formula_mk_not(formula p);
formula ics_formula_mk_andthen(formula p, formula q);
formula ics_formula_mk_orelse(formula p, formula q);
formula ics_formula_mk_implies(formula p, formula q);
formula ics_formula_mk_equiv(formula p, formula q);
formula ics_formula_mk_ite(formula p, formula q, formula r);


/*** Logical contexts ***/

int ics_context_is_empty(context ctxt);
/* Test if argument is the empty context. */

formula ics_context_head(context ctxt);
/* Return the first formula in a nonempty context. */

context ics_context_tail(context ctxt);
/* Return all but the first formula in a context. */


/*** States ***/

#define UNKNOWN 0
#define SAT 1
#define UNSAT 2

int ics_status();
/* Return the status of the current logical context. [SAT] means
 the current context is satisfiable, [UNSAT] means it is unsatisfiable,
 and an [UNKNOWN] indicates that the status has not been resolved yet.
 [ics_resolve()] needs to be called to resolve the status 
 to [SAT] or [UNSAT]. */

context ics_context();
/* Return the logical context associated with the current state.
  The head of this context is the latest processed formula.  The formulas 
  in this context might contain internal variables, as they are canonized 
  wrt. to the current context. */

int ics_is_unchanged();
/* Test if state has been unchanged since the last [ics_initialize]
   or [ics_reset]. */

void ics_initialize(state s);
/* Initialize the current state with the argument state. */

void ics_reset();
/* Reset the current state to an initial state with empty context. */

state ics_finalize();
/* Save the current state. All future modifications of the
   current state do not affect this saved state. */

void ics_process(formula fml);
/* Conjoin the current context with formula [fml].
  [ics_process()] is not complete in that it does not
  apply "expensive" inference rules such as case splitting.
  Thus, the status after process might be [UNKNOWN]. */

void ics_resolve();
/* [ics_resolve()] resolves [UNKNOWN] status of the current
   configuration to either [SAT] or [UNSAT]. */

context ics_explain();
/* If the status of the current context is [UNSAT], then
   [ics_explain()] returns an irreducible unsatisfiable
   subset of [ics_context()]. */

term ics_can(term t);
/** Return a canonical form of term [t] wrt the current context. */

rat ics_sup(term t);
/** Return a rational [q] such that [t <= q]. */

rat ics_min(term t);
/** Return a rational [q] such that [q <= t]. */
