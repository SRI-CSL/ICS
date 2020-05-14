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
     demoura - May 14, 2002: Created.
***/

#include "LPSolver.h"

void LPSolver::compute_lookahead_intersection(int * assignments1, int * assignments2, int * result_assignments)
{
	SOLVER_LOOKAHEAD_TRACE(ctrace << "  computing intersection...\n";);
	for (unsigned int i = 2; i < get_internal_formulas_array_size(); i++) {
		if (assignments1[i] == assignments2[i])
			result_assignments[i] = assignments1[i];
		else {
			result_assignments[i] = assignments1[i];
			int can1 = canonical_formula(result_assignments, i);
			result_assignments[i] = assignments2[i];
			int can2 = canonical_formula(result_assignments, i);
			if (can1 == can2)
				result_assignments[i] = can1;
			else
				result_assignments[i] = 0; // unknown
		}
	}
}

void LPSolver::consume_intersection(int * intersection)
{
	for (unsigned int i = 2; i < get_internal_formulas_array_size(); i++) {
		if (assignments[i] != intersection[i]) {
			assert(intersection[i] != 0);
			SOLVER_LOOKAHEAD_TRACE(ctrace << "    " << i << " = " << intersection[i] << endl;);
			bool assign_result = assign_formula(i, intersection[i], 0);
			assert(assign_result);
			num_lookahead_optimization_assignments++;
		}
	}
	int cp_result = propagate_rich_constraints();
	assert(cp_result != 0);
}

bool LPSolver::apply_lookahead_optimization()
{
 	clock_t start = clock();
	int * back_assignments = new int[get_internal_formulas_array_size()];
	int * back_assignments2 = new int[get_internal_formulas_array_size()];
	if (verbose)
		cerr << "    applying lookahead optimization...\n";

	SOLVER_LOOKAHEAD_TRACE(ctrace << "Lookahead optimization\n";);

	bool result = true;
	for (unsigned int f_idx = 2; f_idx < get_internal_formulas_array_size(); f_idx++) {
		if (assignments[f_idx] == 0 && get_num_occurrences(f_idx) >= lookahead_relevance) { // only unknown literals are used... 
			// try to assign "true"
			set_backtrack_point(); decision_level = 1;
			to_process.reset();
			SOLVER_LOOKAHEAD_TRACE(ctrace << "  trying " << f_idx << " = true\n";);
			bool assign_result = assign_formula(f_idx, LPTrueId, 0);
			assert(assign_result);
			if (propagate_rich_constraints() != 0) {
				SOLVER_LOOKAHEAD_TRACE(ctrace << "    inconsistency detected, so " << f_idx << " must be false\n";);
				num_lookahead_optimization_assignments++;
				backtrack();
				// the value of f_idx must be "false"
				if (!assign_formula(f_idx, LPFalseId, 0) || propagate_rich_constraints() != 0) {
					SOLVER_LOOKAHEAD_TRACE(ctrace << "    another inconsistency detected, problem solved!\n";);
					result = false;
					break;
				}
			}
			else {
				// save the current assignments
				memcpy(back_assignments, assignments, sizeof(int) * get_internal_formulas_array_size());
				backtrack();
				set_backtrack_point(); decision_level = 1;
				to_process.reset();
				SOLVER_LOOKAHEAD_TRACE(ctrace << "  trying " << f_idx << " = false\n";);
				bool assign_result = assign_formula(f_idx, LPFalseId, 0);
				assert(assign_result);
				if (propagate_rich_constraints() != 0) {
					SOLVER_LOOKAHEAD_TRACE(ctrace << "    inconsistency detected, so " << f_idx << " must be true\n";);
					num_lookahead_optimization_assignments++;
					backtrack();
					// the value of f_idx must be "false"
					if (!assign_formula(f_idx, LPTrueId, 0) || propagate_rich_constraints() != 0) {
						SOLVER_LOOKAHEAD_TRACE(ctrace << "    another inconsistency detected, problem solved!\n";);
						result = false;
						break;
					}
				}
				else {
					// compute the intersection of assignments an back_assignments, and put the result in
					// back_assignments
					compute_lookahead_intersection(assignments, back_assignments, back_assignments2);
					backtrack();
					consume_intersection(back_assignments2);
				}
			}
		}
	}

	delete[] back_assignments;
	delete[] back_assignments2;

	clock_t end = clock();
	lookahead_optimization_time = ((double) (end - start)) / CLOCKS_PER_SEC;

	return result;
}
