/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 13, 2002: Created.
***/

#include "LPSolver.h"

struct PolarityOptimizationToDoItem {
	LPFormulaId formula_id;
	LPFormulaPolarityForce force;
	PolarityOptimizationToDoItem(LPFormulaId f, LPFormulaPolarityForce p) {
		formula_id = f; force = p; 
	}
};

static LPFormulaPolarityForce invert_force(LPFormulaPolarityForce f)
{
	switch(f) {
	case POS_FORCE:
		return NEG_FORCE;
	case NEG_FORCE:
		return POS_FORCE;
	case POS_NEG_FORCE:
		return f;
	default:
		assert(false);
	}
}

static bool update_polarity(LPFormulaPolarityForce * forces, unsigned int idx, LPFormulaPolarityForce new_force) {
	if (forces[idx] == UNKNOWN_FORCE) {
		forces[idx] = new_force;
		return true;
	}
	else if (forces[idx] != POS_NEG_FORCE && forces[idx] != new_force) {
		forces[idx] = POS_NEG_FORCE;
		return true;
	}
	return false;
}

bool LPSolver::apply_polarity_optimization_main_loop(LPFormulaId formula_id, LPFormulaPolarityForce force, LPFormulaPolarityForce * forces)
{
	unsigned int formula_idx = absolute(formula_id);
	if (formula_idx == absolute(LPTrueId))
		return true; // ignore this case.
	marks[formula_idx] = true; 
	if (formula_id < 0)
		force = invert_force(force);
	update_polarity(forces, formula_idx, force);
	const LPFormula * formula = formula_manager->get_formula(formula_idx);
	switch (formula->get_kind()) {
	case LP_OR: {
		unsigned int n = formula->get_num_arguments();
		bool result = true;
		for (unsigned int i = 0; i < n; i++) {
			LPFormulaId child = formula->get_argument(i);
			if (!apply_polarity_optimization_main_loop(child, force, forces))
				result = false;
		}
		if (!result)
			update_polarity(forces, formula_idx, POS_NEG_FORCE); // it was not safe to apply polarity optimization
		return result;
	}
	case LP_IFF: {
		LPFormulaId lhs = formula->get_iff_lhs();
		LPFormulaId rhs = formula->get_iff_rhs();
		bool result = true;
		if (!apply_polarity_optimization_main_loop(lhs, POS_NEG_FORCE, forces))
			result = false;
		if (!apply_polarity_optimization_main_loop(rhs, POS_NEG_FORCE, forces))
			result = false;
		if (!result)
			update_polarity(forces, formula_idx, POS_NEG_FORCE); // it was not safe to apply polarity optimization
		return result;
	}
	case LP_ITE: {
		LPFormulaId c = formula->get_cond();
		LPFormulaId t = formula->get_then();
		LPFormulaId e = formula->get_else();
		bool result = true;
		if (!apply_polarity_optimization_main_loop(c, POS_NEG_FORCE, forces))
			result = false;
		if (!apply_polarity_optimization_main_loop(t, force, forces))
			result = false;
		if (!apply_polarity_optimization_main_loop(e, force, forces))
			result = false;
		return result;
	}
	case LP_EXISTS:
		feature_not_implemented_yet();
		return true;
	case LP_PROPOSITION:
		return true; // safe application of the polarity rule...
	case LP_EQ:
		// polarity rule does not work for non-propositional variables
		return false; // unsafe to apply polarity rule
	default:
		assert(false);
	}
}


void LPSolver::apply_polarity_optimization(LPFormulaId root)
{
 	clock_t start = clock();
	if (verbose)
		cout << "    applying polarity based optimization...\n";
	LPFormulaPolarityForce * forces;

	assert(check_marks());

	forces = new LPFormulaPolarityForce[num_formulas+1];
	memset(forces, 0, sizeof(LPFormulaPolarityForce) * get_internal_formulas_array_size());

	apply_polarity_optimization_main_loop(root, POS_FORCE, forces);

	// Starts from 2, since 0 is the null formula, and 1 is the true atom.
	for (unsigned int idx = 2; idx <= num_formulas; idx++) { 
		assert(IMPLY(marks[idx], forces[idx] != UNKNOWN_FORCE));
		if (marks[idx]) {
			marks[idx] = false;
			// the rule can only be applied to propositions... since we do not use a canonical representation...
			// For example: consider the following counter-example:
			//   root = ITE(C, A <=> B, A <=> C) & (A <=> ~B)
			//    the polarity optimizer will return
			//       A <=> B is positive
			//       A <=> ~B is positive
			//    Notice that A <=> ~B is equivalent to ~(A <=> B)
			//
			// idx == 1 is the True atom.
			//
			if (forces[idx] != POS_NEG_FORCE && formula_manager->get_formula(idx)->is_proposition()) {
				assert(forces[idx] == POS_FORCE || forces[idx] == NEG_FORCE);
				SOLVER_TRACE(ctrace << "POLARITY: ";
										 formula_manager->dump_formula(ctrace, idx);
										 ctrace << " " << forces[idx] << endl;);
				bool assign_result = assign_formula(idx, forces[idx] == POS_FORCE ? 1 : -1, 0);
				assert(assign_result);
			}
		}
	}

	assert(check_marks());
	delete forces;
	clock_t end = clock();
	polarity_optimization_time = ((double) (end - start)) / CLOCKS_PER_SEC;
}
