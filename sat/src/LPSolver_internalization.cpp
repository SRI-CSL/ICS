/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 6, 2002: Created.
***/

#include "LPSolver.h"

void LPSolver::internalize_formula(unsigned int idx)
{
	queue<unsigned int> & to_internalize = tmp_queue;
	to_internalize.reset();

	to_check.reset();
	to_check_inverse.reset();

	assert(check_marks());

	queue_push(to_internalize, idx);
	while (!to_internalize.is_empty()) {
		unsigned int curr_idx = to_internalize.pop();
		assert(marks[curr_idx]);
		const LPFormula * formula = formula_manager->get_formula(curr_idx);
		// add formula in the to_check array...
		if ((branching_mode == LP_ACTIVATION_MODE) ||
				(branching_mode == LP_ASSIGNMENT_MODE) ||
				(branching_mode == LP_VAR_ASSIGNMENT_MODE && formula->is_atomic())) {
			to_check_inverse.set_x(curr_idx, to_check.get_size());
			to_check.push(curr_idx);
		}
		// process children...
		switch (formula->get_kind()) {
		case LP_OR: {
			unsigned int n = formula->get_num_arguments();
			for (unsigned int i = 0; i < n; i++) {
				LPFormulaId child = formula->get_argument(i);
				set_occurrence(child, curr_idx);
				queue_push(to_internalize, absolute(child));
			}
			break;
		}
		case LP_IFF: {
			LPFormulaId lhs = formula->get_iff_lhs();
			LPFormulaId rhs = formula->get_iff_rhs();

			set_occurrence(lhs, curr_idx);
			set_occurrence(rhs, curr_idx);
			
			queue_push(to_internalize, absolute(lhs));
			queue_push(to_internalize, absolute(rhs));
			break;
		}
		case LP_ITE: {
			LPFormulaId c = formula->get_cond();
			LPFormulaId t = formula->get_then();
			LPFormulaId e = formula->get_else();
			set_occurrence(c, curr_idx);
			set_occurrence(t, curr_idx);
			set_occurrence(e, curr_idx);

			queue_push(to_internalize, absolute(c));
			queue_push(to_internalize, absolute(t));
			queue_push(to_internalize, absolute(e));
			break;
		}
		case LP_EXISTS:
			feature_not_implemented_yet();
		case LP_EQ:
		case LP_PROPOSITION:
			break; // do nothing... 
		default:
			assert(false);
		}
	}
	
	SOLVER_TRACE(ctrace << "to_check vector = ";
							 dump_to_check_vector(ctrace););
	
	DBG_CODE(unsigned int n = to_check.get_size();
					 for (unsigned int i = 0; i < n; i++) {
						 assert(to_check_inverse.get(to_check.get(i)) == i);
					 });
	remove_marks_from_tmp_queue();
}


void LPSolver::internalize_formula_as_clauses(int f_id)
{
	queue<unsigned int> & to_internalize = tmp_queue;
	to_internalize.reset();

	assert(check_marks());

	growable_vector<int> clause;

	const LPFormula * formula = formula_manager->get_formula(absolute(f_id));

	if (formula->is_or() && f_id < 0) {
		// formula is an AND!
		unsigned int n = formula->get_num_arguments();
		unsigned int num_non_atoms = 0;
		for (unsigned int i = 0; i < n; i++) {
			int child_id = -formula->get_argument(i); // I must invert the child, since the AND is represented as -(OR -child_1 ... - child_n)
			const LPFormula * child = formula_manager->get_formula(absolute(child_id));
			if (!child->is_atomic())
				num_non_atoms++;
			if (child->is_or()) {
				assert(child_id > 0); // by construction of the LPFormula
				unsigned child_size = child->get_num_arguments();
				clause.reset();
				SOLVER_TRACE(ctrace << "  adding clause: ";);
				for (unsigned int j = 0; j < child_size; j++) {
					SOLVER_TRACE(ctrace << child->get_argument(j) << " ";);
					clause.push(child->get_argument(j));
					queue_push(to_internalize, absolute(child->get_argument(j)));
				}
				SOLVER_TRACE(ctrace << endl;);
				add_clause(clause);
			}
			else
				queue_push(to_internalize, absolute(child_id));
		}
		// cout << "num_non_atoms: " << num_non_atoms << endl;
		npc_threshold = npc_min_threshold + (num_non_atoms / 100);
	}
	else { 
		npc_threshold = npc_min_threshold;
		queue_push(to_internalize, absolute(f_id));
	}

	while (!to_internalize.is_empty()) {
		int curr_id = to_internalize.pop();
		unsigned int curr_idx = absolute(curr_id);
		assert(marks[curr_idx]);
		const LPFormula * curr_formula = formula_manager->get_formula(curr_idx);
		switch (curr_formula->get_kind()) {
		case LP_OR: {
			unsigned int n = curr_formula->get_num_arguments();
			clause.reset();
			clause.push(-curr_id);
			for (unsigned int i = 0; i < n; i++) {
				queue_push(to_internalize, absolute(curr_formula->get_argument(i)));
				clause.push(curr_formula->get_argument(i));
			}
			add_clause(clause);
			for (unsigned int i = 0; i < n; i++) {
				clause.reset();
				clause.push(-curr_formula->get_argument(i));
				clause.push(curr_id);
				add_clause(clause);
			}
			break;
		}
		case LP_IFF: {
			LPFormulaId lhs = curr_formula->get_iff_lhs();
			LPFormulaId rhs = curr_formula->get_iff_rhs();
			clause.reset(); clause.push(-curr_id); clause.push(-lhs); clause.push(rhs); add_clause(clause);
			clause.reset(); clause.push(-curr_id); clause.push(lhs); clause.push(-rhs); add_clause(clause);
			clause.reset(); clause.push(curr_id); clause.push(lhs); clause.push(rhs); add_clause(clause);
			clause.reset(); clause.push(curr_id); clause.push(-lhs); clause.push(-rhs); add_clause(clause);

			queue_push(to_internalize, absolute(lhs));
			queue_push(to_internalize, absolute(rhs));
			break;
		}
		case LP_ITE: {
			LPFormulaId c = curr_formula->get_cond();
			LPFormulaId t = curr_formula->get_then();
			LPFormulaId e = curr_formula->get_else();
			
			clause.reset(); clause.push(-curr_id); clause.push(-c); clause.push(t); add_clause(clause);
			clause.reset(); clause.push(-curr_id); clause.push(c); clause.push(e); add_clause(clause);
			clause.reset(); clause.push(curr_id); clause.push(c); clause.push(-e); add_clause(clause);
			clause.reset(); clause.push(curr_id); clause.push(-c); clause.push(-t); add_clause(clause);
			clause.reset(); clause.push(curr_id); clause.push(-t); clause.push(-e); add_clause(clause);
			

			queue_push(to_internalize, absolute(c));
			queue_push(to_internalize, absolute(t));
			queue_push(to_internalize, absolute(e));
			break;
		}
		case LP_EXISTS: 
			feature_not_implemented_yet();
			break;
		case LP_EQ:
		case LP_PROPOSITION:
			break; // do nothing... 
		default:
			assert(false);
		}
	}
	
	SOLVER_TRACE(ctrace << "Clauses: ";
							 dump_clauses(ctrace);
							 );

	remove_marks_from_tmp_queue();
}
