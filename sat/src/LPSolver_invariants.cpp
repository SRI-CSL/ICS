/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 6, 2002: Created.
***/

#include "LPSolver.h"

bool LPClause::check_invariant() const
{
	assert(size > 1);

 	if (get_watch1() == get_watch2()) {
		assert(false);
		return false;
	}
 	if (!(get_watch1() >= literals && get_watch1() <= &(literals[size-1]))) {
		assert(false);
		return false;
	}
 	if (!(get_watch2() >= literals && get_watch2() <= &(literals[size-1]))) {
		assert(false);
		return false;
	}

	return true;
}

bool LPSolver::check_assignment_invariant() const 
{
	for (unsigned int idx = 1; idx <= num_formulas; idx++)
		if (absolute(assignments[idx]) >= idx) {
			assert(false);
			return false;
		}
	return true;
}

bool LPSolver::check_equivalence_class_invariant()
{
	// INVARIANT
	// For each e1,e2 in the same equivalence_class |canonical_formula(e1)| = |canonical_formula(e2)|
	for (unsigned int i = 1; i <= num_formulas; i++) {
		int can_idx = abs(canonical_formula(i));
		LPInternalFormulaInfo & f = internal_formulas[i];
		unsigned int curr = absolute(f.next);
		while (curr != i) {
			if (abs(canonical_formula(curr)) != can_idx) {
				assert(false);
				return false;
			}
			curr = abs(internal_formulas[curr].next);
		}
	}
	return true;
}

bool LPSolver::check_equivalence_class_root_invariant()
{	
	// INVARIANT
	// For each equivalence class root r:
	//        assignments(r) = 0, 1, or -1
	//        assignments(r) = 0  IMPLIES  r.antecedent = 0
	//        assignments(r) = 0  IMPLIES  r.decision_level = 0
	for (unsigned int i = 1; i <= num_formulas; i++) {
		int can_idx = abs(canonical_formula(i));
		if (assignments[can_idx] != 0 && assignments[can_idx] != -1 && assignments[can_idx] != 1) {
			assert(false);
			return false;
		}
		if (assignments[can_idx] == 0) {
			if (internal_formulas[can_idx].antecedent != 0) {
				assert(false);
				return false;
			}
			if (internal_formulas[can_idx].decision_level != 0) {
				assert(false);
				return false;
			}
		}
	}
	return true;
}

bool LPSolver::check_activation_invariant()
{
	// if a formula was processed, then it should be active
	for (unsigned int i = 1; i <= num_formulas; i++) {
		if (!IMPLY(processed[i], active[i])) {
			assert(false);
			return false;
		}
	}
	// if a formula is active... then it must be on the activation_trail_stack
	// if a formula was processed... then it must be on the processed_trail_stack
	for (unsigned int i = 1; i <= num_formulas; i++) {
		if (active[i])
			if (!activation_trail_stack.contains(i)) {
				assert(false);
				return false;
			}
		if (processed[i]) 
			if (!processed_trail_stack.contains(i)) {
				assert(false);
				return false;
			}
	}
	// check if all active formulas are really necessary
	// The following invariant is INCORRECT!!!
	// Problem: The formula is a DAG.
	//          (and (or a b) (or b c))
	//          "a" is activated because of (or a b)
	//          "b" is activated because of (or b c)
	//          Then (or a b) contains two active children
	//
	//          A similar example can be created for ite nodes.

//  	for (unsigned int i = 1; i <= num_formulas; i++) {
//  		if (active[i]) {
//  			int value = get_formula_value(i);
//  			const LPFormula * formula = formula_manager->get_formula(i);
//  			if (value == 1 && formula->is_or()) {
//  				// An active OR formula must have at most one active child!
//  				unsigned int n = formula->get_num_arguments();
//  				unsigned int num_active_children = 0;
//  				for (unsigned int j = 0; j < n; j++) {
//  					LPFormulaId child_id = formula->get_argument(j);
//  					if (active[absolute(child_id)])
//  						num_active_children++;
//  				}
//  				if (num_active_children > 1) {
//  					assert(false);
//  					return false;
//  				}
//  			}
//  			else if (formula->is_ite()) {
//  				// The then-brach and else-branch cannot be active at the same time.
//  				LPFormulaId t = formula->get_then();
//  				LPFormulaId e = formula->get_else();
//  				if (active[absolute(t)] && active[absolute(e)]) {
//  					assert(false);
//  					return false;
//  				}
//  			}
//  		}
//  	}
	return true;
}

bool LPSolver::check_clause_literals_invariant(LPClause * clause)
{
	unsigned int clause_idx = (unsigned int) *(clause->get_literals() - 1);
	LPClause * expected_clause = clauses.get_ptr(clause_idx);
	if (clause != expected_clause) {
		assert(false);
		return false;
	}
	return true;
}

bool LPSolver::check_clause_db_invariant()
{
	unsigned int n = clauses.get_size();
	for (unsigned int i = 1; i < n; i++) { // the first slot in the clause array is not used
		unsigned int clause_idx = i;
		LPClause & clause = clauses.get(clause_idx);
		if (!clause.was_deleted()) {
			assert(check_clause_literals_invariant(&clause));
			
			if (clause.get_watch1() == NULL) {
				assert(false); 
				return false;
			}
			if (clause.get_watch2() == NULL) {
				assert(false); 
				return false;
			}
			LPFormulaId w1 = clause.get_watch1_literal();
			LPFormulaId w2 = clause.get_watch2_literal();
			if (w1 == w2) {
				assert(false);
				return false;
			}
			LPInternalFormulaInfo & info1 = internal_formulas[absolute(w1)];
			LPInternalFormulaInfo & info2 = internal_formulas[absolute(w2)];
			LPClauseVector & vect1 = w1 < 0 ? info1.clausal_negative_occurrences : info1.clausal_positive_occurrences;
			LPClauseVector & vect2 = w2 < 0 ? info2.clausal_negative_occurrences : info2.clausal_positive_occurrences;
			if (!vect1.contains(clause_idx)) {
				DBG_CODE(cerr << "BUG detected: " << endl;
								 cerr << "  clause addr = " << &clause << endl;
								 cerr << "  "; dump_clause(cerr, clause); cerr << endl;
								 cerr << "  watch1 = " << w1 << endl;
								 cerr << "  occ    = " << vect1 << endl;);
				assert(false);
				return false;
			}
			if (!vect2.contains(clause_idx)) {
				DBG_CODE(cerr << "BUG detected: " << endl;
								 cerr << "  clause addr = " << &clause << endl;
								 cerr << "  "; dump_clause(cerr, clause); cerr << endl;
								 cerr << "  watch1 = " << w2 << endl;
								 cerr << "  occ    = " << vect2 << endl;);
				assert(false);
				return false;
			}
		}
	}
	
	for (unsigned int idx = 2; idx <= num_formulas; idx++) {
		LPInternalFormulaInfo & info = internal_formulas[idx];
		if (!(check_clausal_occurrences(info.clausal_positive_occurrences, idx) &&
					check_clausal_occurrences(info.clausal_negative_occurrences, -idx))) {
			assert(false);
			return false;
		}
	}

	return true;
}

bool LPSolver::check_clausal_occurrences(LPClauseVector & v, LPFormulaId formula_id)
{
	for (unsigned int i = 0; i < v.get_size(); i++) {
		unsigned int clause_idx = v.get(i);
		LPClause * clause = clauses.get_ptr(clause_idx);
		if (!clause->was_deleted() && clause->get_watch1_literal() != formula_id && clause->get_watch2_literal() != formula_id) {
			cerr << "Bug detected: formula_id = " << formula_id << ", clause = ";
			dump_clause(cerr, *clause);
			cerr << endl;
			assert(false);
			return false;
		}
	}
	return true;
}

bool LPSolver::check_invariant()
{
	return check_assignment_invariant() && check_equivalence_class_invariant() && check_equivalence_class_root_invariant() &&
		IMPLY(branching_mode == LP_ACTIVATION_MODE, check_activation_invariant());
}

bool LPSolver::check_after_constraint_propagation()
{
	unsigned int n = clauses.get_size();
	for (unsigned int i = 1; i < n; i++) { // the first slot in the clause array is not used
		LPClause & clause = clauses.get(i);
		if (!clause.was_deleted()) {
			unsigned int num_lits = clause.get_size();
			unsigned int num_unknow = 0;
			bool is_ok = false;
			for (unsigned int j = 0; j < num_lits; j++) {
				LPFormulaId curr_lit = clause.get_literal(j);
				int val = get_formula_value(curr_lit);
				if (val == 1) {
					is_ok = true;
					break;
				}
				else if (val == 0) {
					num_unknow++;
					if (num_unknow == 2) {
						is_ok = true;
						break;
					}
				}
				else {
					assert(val == -1);
				}
			}
			assert(num_unknow <= 2);
			if (!is_ok) {
				DBG_CODE(cerr << "BUG detected: " << endl;
								 cerr << "  clause addr = " << &clause << endl;
								 cerr << "  "; dump_clause(cerr, clause); cerr << endl;
								 cerr << "  num_unknow = " << num_unknow << endl;);
				assert(false);
				return false;
			}
		}
	}
	return true;
}

bool LPSolver::check_completeness_argument()
{
	unsigned int n = clauses.get_size();
	for (unsigned int i = 1; i < n; i++) { // the first slot in the clause array is not used
		unsigned int clause_idx = i;
		LPClause & clause = clauses.get(clause_idx);
		if (!clause.was_deleted()) {
			LPFormulaId w1 = clause.get_watch1_literal();
			LPFormulaId w2 = clause.get_watch2_literal();
			LPInternalFormulaInfo & info1 = internal_formulas[absolute(w1)];
			LPInternalFormulaInfo & info2 = internal_formulas[absolute(w2)];
			
			// if the clause was satisfied and
			// decision_level(watch1) != 0 AND decision_level(watch2) != 0, then 
			//   exists a literal "l" assigned to true, such that
			//      decision_level("l") <= decision_level("watch1")
			//      OR
			//      decision_level("l") <= decision_level("watch2")
			//
			if (is_clause_satisfiable(clause_idx) &&
					info1.decision_level != 0 &&
					info2.decision_level != 0) {
				unsigned int min_dec_level_of_true_literal = max_decision_level + 1;
				for (unsigned int i = 0; i < clause.get_size(); i++) {
					LPFormulaId curr_literal = clause.get_literal(i);
					if (get_formula_value(curr_literal) == 1) {
						unsigned int dec_level = internal_formulas[absolute(curr_literal)].decision_level;
						assert(dec_level <= max_decision_level);
						if (dec_level < min_dec_level_of_true_literal)
							min_dec_level_of_true_literal = dec_level;
					}
				}
				if (!(min_dec_level_of_true_literal <= info1.decision_level ||
							min_dec_level_of_true_literal <= info2.decision_level)) {
					cout << "Completeness Problem: ";
					dump_clause(cout, clause);
					cout << endl;
				
					assert(false);
					return false;
				}
			}
		}
	}
	return true;
}


bool LPSolver::check_marks() {
	for (unsigned int i = 1; i <= num_formulas; i++) {
		if (marks[i]) {
			assert(false);		
			return false;
		}
	}
	return true;
}

