/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 6, 2002: Created.
***/

#include "LPSolver.h"

void LPSolver::set_backtrack_point()
{
	SOLVER_TRACE(ctrace << "  adding backtrack point: (" << get_trail_stack_top() << ", " << 
							 get_activation_trail_stack_top() << ", " << get_processed_trail_stack_top() << ", " << next_to_check << ") at " << 
							 get_backtrack_point_stack_top() << endl;);
	LPBacktrackPoint p;
	p.trail_stack_top = get_trail_stack_top();
	p.processed_trail_stack_top = get_processed_trail_stack_top();
	if (branching_mode == LP_ACTIVATION_MODE) {
		p.activation_trail_stack_top = get_activation_trail_stack_top();
		p.next_to_check = next_to_check;
	}
	ics_interface.push();
	backtrack_point_stack.push(p);
}

/*
	Backtracking is not that simple, when we are using non-chronological backtracking and conflict_clauses.
	The main problem is that the decision level of a variable can be modified by conflict clauses...

	in standard backtracking, the variables in the stack trail_stack
	are in non-decreasing decision level order.
	But, this invariant is NOT true when we use conflict_clauses and non-chronological backtracking.
	If this problem is ignored, we may produce wrong answers.
	To solve the problem, when we backtrack, we should move the "increasing" decision level variables.
	
	For instance suppose that we have the following variables in the trail stack:
	
	var_idx : associated decision level
	10      :        2
	9       :        2
	12      :        4
  7       :        4
  21      :        1
	6       :        3
  8       :        3
	31      :        2
	32      :        2
	3       :        1
  4       :        1


	If we backtrack to decision level 2, then we should produce the following stack

	var_idx : associated decision level
	10      :        2
	9       :        2
  21      :        1
	31      :        2
	32      :        2
	3       :        1
  4       :        1

	activation_trail_stack and processed_trail_stack do not have this problem.

	If we are in LP_ASSIGNMENT_MODE or LP_VAR_ASSIGNMENT_MODE, then	
	  we have to assert again the variables that were moved in the activation_trail_stack.
	In both cases, all assertions MUST succeed.

	All moved variables must be inserted into the to_process queue, since some consequences
	of them were undone.

	ALTERNATIVE SOLUTION: multiple stacks (one for each decision level).
	This solution is incompatible with an online integration with  decision procedures, 
	since it will require the decision procedures to have multiple stacks. 
	However, it can be used with an offline integration, that is, the decision procedure
	is only called after we generate a complete boolean assignment. 

 */
bool LPSolver::backtrack(unsigned int num_levels)
{
	static growable_vector<unsigned int> trail_stack_to_move;
	trail_stack_to_move.reset();

	SOLVER_TRACE(ctrace << "backtracking " << num_levels << " level(s), current decision level = " << decision_level << endl;);
	if (num_levels > get_backtrack_point_stack_top())
		return false;

	decision_level -= num_levels;

//  	if (!to_process.is_empty()) {
// 		bool found = false;
//  		for (int i = 0; i < to_process.get_size(); i++) {
//  			int var_idx = to_process.get(i);
//  			int var_idx_level = internal_formulas[var_idx].decision_level;
//  			if (var_idx_level < decision_level) {
//  				cout << var_idx << ":" << var_idx_level << " ";
//  				found = true;
//  			}
//  		}
//  		if (found) {
//  			cout << "after_branching = " << after_branching << " num_levels = " << num_levels << ", decision_level = " << decision_level << ", to_process.get_size() = " << to_process.get_size() << "\n";
// 		}
//  	}

	num_backtracks++;
	if (num_levels > 1)
		num_non_chronological_backtracking++;

	while (num_levels > 0) {
		num_levels--;
		LPBacktrackPoint & p = backtrack_point_stack.pop();
		if (num_levels == 0) {
			while (p.trail_stack_top != get_trail_stack_top()) {
				unsigned int var_idx = trail_stack.pop();
				if (internal_formulas[var_idx].decision_level <= decision_level) 
					trail_stack_to_move.push(var_idx);
				else
					undo_assignment(var_idx);
			}
			while (p.processed_trail_stack_top != get_processed_trail_stack_top()) {
				unsigned int var_idx = processed_trail_stack.pop();
				SOLVER_TRACE(ctrace << "  unprocessing: " << var_idx << endl;);
				processed[var_idx] = false;
			}
			if (branching_mode == LP_ACTIVATION_MODE) {
				while (p.activation_trail_stack_top != get_activation_trail_stack_top()) {
					unsigned int var_idx = activation_trail_stack.pop();
					SOLVER_TRACE(ctrace << "  unactivating: " << var_idx << endl;);
					active[var_idx] = false;
				}
				SOLVER_TRACE(ctrace << "  restoring next_to_check to " << p.next_to_check << endl;);
				next_to_check = p.next_to_check;
			}
		}
		ics_interface.pop();
	}

	// save real top
	unsigned int trail_real_top = trail_stack.pop();

	// put vars back on the trail stack.
	int n = trail_stack_to_move.get_size();
	for (int i = n; --i >= 0; ) {
		unsigned int var_idx = trail_stack_to_move.get(i);
		SOLVER_TRACE(ctrace << "  moving " << var_idx << ", decision_level = " << internal_formulas[var_idx].decision_level << endl;);
		trail_stack.push(var_idx);
		DBG_CODE(int val = fast_get_formula_value(var_idx);
						 assert(val == 1 || val == -1);
						 LPClauseVector & v = *(val == 1 ? internal_formulas[var_idx].all_clausal_negative_occurrences : internal_formulas[var_idx].all_clausal_positive_occurrences);
						 unsigned int n = v.get_size();
						 for (unsigned int i = 0; i < n; i++) {
							 unsigned int clause_idx = v.get(i);
							 if (!is_clause_satisfiable(clause_idx) && get_clause_num_unknowns(clause_idx) <= 1) {
								 if (absolute(clauses.get(clause_idx).get_watch1_literal()) != var_idx &&
										 absolute(clauses.get(clause_idx).get_watch2_literal()) != var_idx)
									 SOLVER_TRACE(ctrace << "NON EXPECTED: ";);
								 SOLVER_TRACE(ctrace << "Warning (possibly missing constraint): ";
															dump_clause(ctrace, clauses.get(clause_idx));
															ctrace << "   ---  var_idx = " << var_idx << endl;);
								 // to_process_queue_push(var_idx);
							 }
						 });
		// I can remove the following line, if I decide only to use the -f option (assign all formulas and subformulas)
		// However, if I remove the following line, the CP invariant is not valid, that is, I can miss some possible
		// constraint propagations, but the program is still correct when used with the -f option...
		// CP invariant: All possible constraints are propagated
		to_process_queue_push(var_idx);
	}
	
	// restore real trail top!
	trail_stack.push(trail_real_top);
	return true;
}


