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

/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 6, 2002: Created.
***/

#include"LPSolver.h"

// #undef SOLVER_TRACE
// #define SOLVER_TRACE(code) { code }
// #define ctrace cout

LPBranchReturnType LPSolver::branch_default()
{
	if (num_new_non_propositional_assignments > npc_threshold) {
		if (!process_complex_constraints())
			return LP_BRANCH_INCONSISTENCY;
	}

	// get next unassigned formula
	unsigned int n = to_check.get_size();
	while (next_to_check < n) {
		unsigned int f_idx = to_check.get(next_to_check);
		if (is_unknown(f_idx))
			break;
		next_to_check++;
	}
	assert(next_to_check <= n);
	if (next_to_check == n) {
		if (!process_complex_constraints())
			return LP_BRANCH_INCONSISTENCY;
		return LP_BRANCH_FINISHED; /* all formula were assigned */
	}
	unsigned f_idx = to_check.get(next_to_check);
	assert(is_unknown(f_idx));
	SOLVER_TRACE(ctrace << "branching: " << f_idx << " = 1" << endl;);
	inc_decision_level();
	bool result;
	if (num_clausal_positive_occurrences[f_idx] > num_clausal_negative_occurrences[f_idx])
		result = assign_formula(f_idx, LPTrueId, 0);
	else
		result = assign_formula(f_idx, LPFalseId, 0);
	assert(result);
	set_backtrack_point();
	return LP_BRANCH_OK;
}

LPBranchReturnType LPSolver::branch_activation_mode() 
{
	SOLVER_TRACE(ctrace << "branching... next_to_check = " << next_to_check << endl;);
	DBG_CODE(for(unsigned int i = 0; i < next_to_check; i++) {
		unsigned int f_idx = to_check.get(i);
		if (formula_manager->get_formula(f_idx)->is_eq() && active[f_idx] && !processed[f_idx])
			assert(false);
	});
	// get next unassigned formula
	unsigned int n = to_check.get_size();
	while (next_to_check < n) {
		assert(check_invariant());
		unsigned int f_idx = to_check.get(next_to_check);
		SOLVER_TRACE(ctrace << "  branching routine is visiting " << f_idx << ", next_to_check = " << next_to_check << endl;);
		if (active[f_idx] && !processed[f_idx]) {
			if (is_unknown(f_idx)) {
				SOLVER_TRACE(ctrace << "  >> branching at [variable value]: " << f_idx << " = 1" << endl;);
				inc_decision_level();
				bool result;
				if (num_clausal_positive_occurrences[f_idx] > num_clausal_negative_occurrences[f_idx])
					result = assign_formula(f_idx, LPTrueId, 0);
				else
					result = assign_formula(f_idx, LPFalseId, 0);
				assert(result);
				set_backtrack_point();
				// next_to_check++; I should not advance..., let the constraint propagator to work
				return LP_BRANCH_OK;
			}
			else {
				// the formula must be processed...
				SOLVER_TRACE(ctrace << "  processing: " << f_idx << " ";
										 formula_manager->dump_formula(ctrace, f_idx);
										 ctrace << endl;);
				const LPFormula * formula = formula_manager->get_formula(f_idx);
				switch (formula->get_kind()) {
				case LP_OR: {
					int value = get_formula_value(f_idx);
					assert(value == -1 || value == 1);
					if (value == -1) { // false
						// OR is "behaving" like an AND
						// All children should be activated!
						unsigned num_args = formula->get_num_arguments();
						for (unsigned int i = 0; i < num_args; i++)
							activate(abs(formula->get_argument(i)));
						// all children must be false
						DBG_CODE(for (unsigned int i = 0; i < num_args; i++)
										   if (get_formula_value(formula->get_argument(i)) != -1)
											   assert(false);
										 );
						set_as_processed(f_idx);
						next_to_check++; // advance to the next position... 
					} 
					else { // true
						// there are two alternatives
						// 1- there is a child assigned to true
						// 2- there are more than two children assigned to unknown

						// TODO:
						// actually... I found a new alternative
						// if there are two children c1 and c2, such that
						// c1 = -c2, then the or node is justified...
						// we just need to activate c1 and -c2

						int first_child = 0;
						unsigned int max_occurrences = 0;
						bool is_ok = false;
						unsigned num_args = formula->get_num_arguments();
						
						queue<int> & to_select_children = tmp_int_queue;
						to_select_children.reset();

						for (unsigned int i = 0; i < num_args; i++) {
							int curr_arg = formula->get_argument(i);
							int curr_arg_value = get_formula_value(curr_arg);
							if (curr_arg_value == 1) {
								// a true child was found... 
								// it is not necessary to branch
								activate(abs(curr_arg));
								first_child = 0;
								is_ok = true;
								break;
							}
							else if (curr_arg_value == 0) {
								to_select_children.push(curr_arg);
								if (first_child == 0) {
									first_child = curr_arg;
									max_occurrences = get_num_clausal_occurrences(curr_arg);
								}
								else if (first_child != curr_arg) {
									is_ok = true;
									unsigned int aux = get_num_clausal_occurrences(curr_arg);
									if (aux > max_occurrences) {
										first_child = curr_arg; // gets the child that satisfies more clauses...
										max_occurrences = aux;
									}
								}
							}
						}
						assert(is_ok);
						if (first_child != 0) {
							SOLVER_TRACE(ctrace << "  >> branching at [or]: " << f_idx << ", child_idx = " << first_child << endl;);
							inc_decision_level();
							if (curr_randomness > 0) {
								// select a random children...
								int first_child_pos = random()%(to_select_children.get_size());
								first_child = to_select_children.get(first_child_pos);
								curr_randomness--;
							}
							bool result = assign_formula(first_child, 1, 0);
							assert(result);
							set_backtrack_point();
							// IMPORTANT...
							// The child should be activated after setting the backtrack point... since it should be deactivated in the case of 
							// backtracking...
							activate(abs(first_child));
							// IMPORTANT... next_to_check should be updated AFTER setting the backtrack point...
							next_to_check++; // advance to the next position... 
							set_as_processed(f_idx); // the processed mark should be removed when backtracking...
							return LP_BRANCH_OK;
						}
						else {
							next_to_check++; // advance to the next position...
							set_as_processed(f_idx);
						}
							
					}
					break;
				}
				case LP_IFF: 
					// simply activate both children
					activate(abs(formula->get_iff_lhs()));
					activate(abs(formula->get_iff_rhs()));
					next_to_check++; // advance to the next position...
					set_as_processed(f_idx);
					break;
				case LP_ITE: {
					// TODO:
					// I found new optimizations for the LP_ITE case...
					//  
					// if t == e, then we can justify the value of ite(c,t,e)
					// considering (activating) just t and e, that is, ignoring c.
					// This case is only useful if c is huge, and t and e are not.
					//
					
					int value = get_formula_value(f_idx);
					assert(value == -1 || value == 1);
					int cond = formula->get_cond();
					int cond_value = get_formula_value(cond);
					int then_branch = formula->get_then();
					int else_branch = formula->get_else();
					if (cond_value == 1) {
						assert(value == get_formula_value(then_branch));										 
						activate(abs(cond));
						activate(abs(then_branch));
						next_to_check++; // advance to the next position...
						set_as_processed(f_idx);
					}
					else if (cond_value == -1) {
						assert(value == get_formula_value(else_branch));										 
						activate(abs(cond));
						activate(abs(else_branch));
						next_to_check++; // advance to the next position...
						set_as_processed(f_idx); 
					}
					else {
						// need to branch...
						SOLVER_TRACE(ctrace << "  >> branching at [ite]: " << f_idx << ", cond_idx = " << cond << endl;);
						inc_decision_level();
						activate(abs(cond));
						// I should use heuristics to select then/else branch
						bool result = assign_formula(cond, 1, 0);
						assert(result);
						set_backtrack_point();
						// IMPORTANT...
						// The "then branch" should be activated after setting the backtrack point... since it should be deactivated in the case of 
						// backtracking...
						activate(abs(then_branch));
						// IMPORTANT... next_to_check should be updated AFTER setting the backtrack point...
						next_to_check++; // advance to the next position... 
						set_as_processed(f_idx); // the processed mark should be removed when backtracking...
						return LP_BRANCH_OK;
					}
					break;
				}
				case LP_EXISTS:
					feature_not_implemented_yet();
					break;
				case LP_PROPOSITION: 
					// do nothing... 
					// assigned propositions can be ignored...
					// I'm not setting as processed, since it is very *cheap* to process a proposition :-)
					next_to_check++; // advance to the next position... 
					break;
				case LP_EQ: {
					// send to ICS...
					next_to_check++; // advance to the next position... 
					set_as_processed(f_idx);
					int value = get_formula_value(f_idx);
					bool ics_result;
					SOLVER_TRACE(ctrace << "  sending assignment to ICS [";
											 formula_manager->dump_formula(ctrace, f_idx);
											 ctrace << "] = " << value << endl;);
					if (value == 1) 
						ics_result = ics_interface.assert_formula(f_idx);
					else {
						assert(value == -1);
						ics_result = ics_interface.assert_formula(-f_idx);
					}
					if (!ics_result) {
						SOLVER_TRACE(ctrace << "  conflict detected in ICS assertion [";
												 formula_manager->dump_formula(ctrace, f_idx);
												 ctrace << "] = " << value << endl;);
						explain_ics_inconsistency(f_idx);
						return LP_BRANCH_INCONSISTENCY;
					}
					break;
				}
				default:
					assert(false);
				}
			}
		}
		else 
			next_to_check++;
	}

	return LP_BRANCH_FINISHED;
}
