

/***************************************
  0)
  OCAML Support
  (ICS responsibility)
**************************************/

void icsat_register(value * r);
void icsat_deregister(value * r);


/****************************************
  1)
	Formulas
	(SAT solver responsibility)
*****************************************/


LPFormulaManager * icsat_mk_formula_manager();
void icsat_destroy_formula_manager(LPFormulaManager * m);
LPFormulaId icsat_mk_or(LPFormulaManager * m, unsigned int num_args, LPFormulaId * args);
LPFormulaId icsat_mk_and(LPFormulaManager * m, unsigned int num_args, LPFormulaId * args);
LPFormulaId icsat_mk_iff(LPFormulaManager * m, LPFormulaId lhs, LPFormulaId rhs);
LPFormulaId icsat_mk_implies(LPFormulaManager * m, LPFormulaId lhs, LPFormulaId rhs);
LPFormulaId icsat_mk_xor(LPFormulaManager * m, LPFormulaId lhs, LPFormulaId rhs);
LPFormulaId icsat_mk_not(LPFormulaManager * m, LPFormulaId f);
LPFormulaId icsat_mk_ite(LPFormulaManager * m, LPFormulaId c, LPFormulaId t, LPFormulaId e);
LPFormulaId icsat_mk_atom(LPFormulaManager * m, value a, value not_a);
LPFormulaId icsat_mk_var(LPFormulaManager * m, char * var);

/*********************************
  2)
	Assertion
	(ICS responsibility)
**********************************/

void icsat_reset();
void icsat_push(); 
void icsat_pop();
int icsat_assert(value f); /* returns 0 if unsatisfiable, 1 if satisfiable */

/**********************************
  3)
	SAT
	(SAT solver responsibility)
***********************************/

int icsat_sat(LPFormulaId f);
int icsat_assigned_value(LPFormulaId id);


/* How it works

1) before calling icsat_sat, you should insert the "current"
   ics context on the stack (stack initialization).

2) the function ics_reset re-initializes the stack.

3) icsat_push duplicates the stack top
   icsat_push == (stack := push(stack,top(stack)))

4) icsat_pop is
   icsat_pop == (stack := pop(stack))

5) icsat_assert(f) is
     
    result := process(top(stack), f)
    if (result is redundant)
		  return 1;
    else if (result is unsat)
		  return 0;
    else if (result is consistent) {
		  stack := pop(stack);
      stack := push(stack, d_consistent(result));
      return 1;
    }

6) icsat_sat returns 0 if the formula is unsatisfiable, and
   1 otherwise

7) icsat_assigned_value access the boolean assignment produced
   for satisfiable formulas, it can also be used to access the
   value of non atomic formulas. 
     0 - false
     1 - true
     2 - don't care
     
*/


/*************************************
  4)
	Scratch state
	(ICS responsibility)
**************************************/


void icsat_reset_scratch_context();
/* scratch := context before calling icsat_sat */

int icsat_assert_in_scratch_context(value f);
/*  
    result := process(scratch, f)
    if (result is redundant)
		  return 1;
    else if (result is unsat)
		  return 0;
    else if (result is consistent) {
		  scratch := d_consistent(result);
      return 1;
    }
*/


/*****************************************
  5) 
  Debugging
  (ICS provides)
******************************************/

void icsat_pp_current_context();
void icsat_pp_atom(value a);
