/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 8, 2002: Created.
***/

#include "LPSolver.h"

int ICS_EXPLAIN_NUM_REFINEMENTS  = 0;

#define ADD_VAR_TO_EXPLAIN_TODO_QUEUE(VAR_IDX) {	\
	if (!marks[VAR_IDX]) {													\
		marks[VAR_IDX] = true;												\
		explain_to_do.push(VAR_IDX);									\
	}																								\
} ((void) 0)

bool LPSolver::conflict_resolution_only_decided()
{
	SOLVER_TRACE(ctrace << "conflict resolution [only decided strategy]...\n";);
	queue<unsigned int> & explain_to_do = tmp_queue;
	explain_to_do.reset();
	assert(check_marks());
	
	static growable_vector<int> new_conflict_clause;
	new_conflict_clause.reset();

	unsigned new_conflict_clause_highest_decision_level = 0;
	unsigned new_conflict_clause_highest_decision_level_literal_pos;
	unsigned new_conflict_clause_second_highest_decision_level = 0;
	unsigned new_conflict_clause_second_highest_decision_level_literal_pos;
	
	assert(new_conflict_point.get_size() > 0);
	
	// add variables in the new conflict point into the explain_to_do queue
	unsigned int n = new_conflict_point.get_size();
	SOLVER_TRACE(ctrace << "  conflict point: \n";);
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = absolute(new_conflict_point.get(i));
		SOLVER_TRACE(ctrace << "    "; formula_manager->dump_formula(ctrace, new_conflict_point.get(i)); ctrace << endl;);
		assert(!marks[var_idx]);
		ADD_VAR_TO_EXPLAIN_TODO_QUEUE(var_idx);
	}

	//
	// Computing conflict clause
	//

	// IMPORTANT:
	// I'm ignoring equivalence classes when I build a conflict clause.
	// I should decide in the future whether this information should be used or not.
	//
	// Two scenarios:
	// 1) Equivalence classes are only used in the preprocessor.
	//    In this case, I must fix how a formula is internalized.
	//    Since, only the roots of the equivalence classes should be considered.
	//   
	// 2) Equivalence classes are used during the search.
	//    I should fix the following code to consider equivalence classes.
	//
	// The same comment is also valid for conflict_resolution_first_uip
	//
	// Comment Update: I implemented Scenario 1!
	//

	while (!explain_to_do.is_empty()) {
		unsigned int curr_idx = explain_to_do.pop();
		LPInternalFormulaInfo & info = internal_formulas[curr_idx];
		if (info.decision_level > 0) {
			// decision_level == 0 can be ignored, since its value is fixed!
			if (info.antecedent == 0) {
				// it is a choice point, then add it to the conflict clause!

				// it is not necessary to add consider the equivalence class here, since the equivalence class of curr_idx
				// was inserted in the explain_to_do queue

				if (info.decision_level > new_conflict_clause_highest_decision_level) {
					new_conflict_clause_second_highest_decision_level = new_conflict_clause_highest_decision_level;
					new_conflict_clause_second_highest_decision_level_literal_pos = new_conflict_clause_highest_decision_level_literal_pos;
					new_conflict_clause_highest_decision_level = info.decision_level;
					new_conflict_clause_highest_decision_level_literal_pos = new_conflict_clause.get_size();
				}
				else if (info.decision_level > new_conflict_clause_second_highest_decision_level) {
					new_conflict_clause_second_highest_decision_level = info.decision_level;
					new_conflict_clause_second_highest_decision_level_literal_pos = new_conflict_clause.get_size();
				}

				int value = get_formula_value(curr_idx);										 
				assert(value == 1 || value == -1);					
				new_conflict_clause.push(value == 1 ? -curr_idx : curr_idx);
			}
			else {
				assert(info.antecedent < 0); // only clausal constraints are allowed
				unsigned int clause_idx = -info.antecedent;
				LPClause & clause = clauses.get(clause_idx);
				unsigned int n = clause.get_size();
				for (int i = n; --i >= 0; ) {
					unsigned int var_idx = absolute(clause.get_literal(i));
					ADD_VAR_TO_EXPLAIN_TODO_QUEUE(var_idx);
				}
			}
		}
	}
	remove_marks_from_tmp_queue();


	SOLVER_EXPLAIN_TRACE(ctrace << "  explain function result:\n";
											 unsigned int n = new_conflict_clause.get_size();
											 ctrace << "    ";
											 for (unsigned int i = 0; i < n; i++) {
												 LPFormulaId id = new_conflict_clause.get(i);
												 unsigned int var_idx = absolute(id);
												 ctrace << id << ":" << internal_formulas[var_idx].decision_level << " ";
											 }
											 ctrace << endl;
											 ctrace << "    highest decision-level = " << new_conflict_clause_highest_decision_level << endl;
											 ctrace << "    highest decision-level literal_pos = " << new_conflict_clause_highest_decision_level_literal_pos << endl;
											 ctrace << "    second highest decision-level = " << new_conflict_clause_second_highest_decision_level << endl;
											 ctrace << "    second highest decision-level literal_pos = " << new_conflict_clause_second_highest_decision_level_literal_pos << endl;
											 ctrace << "    current decision-level = " << decision_level << endl;
											 );
	

	unsigned int num_levels_to_backtrack = decision_level - new_conflict_clause_highest_decision_level + 1;
	bool found_fix_value = false;
	int new_antecedent = 0;
#ifndef NDEBUG
	bool check_expected_var_idx = false;
	unsigned int expected_var_idx;
#endif
	if (new_conflict_clause.get_size() > 1) {
		LPClause * new_clause = add_clause(new_conflict_clause.get_contents(), new_conflict_clause.get_size(), 
																			 new_conflict_clause_highest_decision_level_literal_pos, 
																			 new_conflict_clause_second_highest_decision_level_literal_pos,
																			 false);
#ifndef NDEBUG
		check_expected_var_idx = true;
		expected_var_idx = absolute(new_conflict_clause.get(new_conflict_clause_highest_decision_level_literal_pos));
#endif
		new_antecedent = - clauses.get_id(new_clause);
	}
	else 
		found_fix_value = true;
	if (!backtrack(num_levels_to_backtrack))
		return false;

	unsigned int f_idx = trail_stack.pop();
	assert(IMPLY(check_expected_var_idx, expected_var_idx == f_idx));
	int val = get_formula_value(f_idx);
	undo_assignment(f_idx);
	SOLVER_TRACE(ctrace << "  conflict solved, assigning " << f_idx << " = " << (-val) << endl;);
	bool assign_result = assign_formula(f_idx, -val, new_antecedent); /* invert value */
	assert(assign_result);
	if (found_fix_value) {
		// set the decision level to 0
		internal_formulas[f_idx].decision_level = 0;
	}
	
	return true;	
}

#define PROCESS_VAR_IN_UIP_STRATEGY(VAR_IDX) {																													\
	unsigned int var_decision_level = internal_formulas[VAR_IDX].decision_level;													\
	if (var_decision_level == 0) {																																				\
		/* do nothing... this kind of var can be ignored... */																							\
	}																																																			\
	else if (var_decision_level < conflict_point_highest_decision_level) {																\
		if (!added_literals[VAR_IDX]) {																																			\
			added_literals[VAR_IDX] = true;																																		\
			/* update second highest decision level information */																						\
			if (var_decision_level > new_conflict_clause_second_highest_decision_level) {											\
				new_conflict_clause_second_highest_decision_level = var_decision_level;													\
				new_conflict_clause_second_highest_decision_level_literal_pos = new_conflict_clause.get_size();	\
			}																																																	\
			/* add literal to the conflict clause */																													\
			new_conflict_clause.push(get_formula_value(VAR_IDX) == 1 ? -VAR_IDX : VAR_IDX);										\
		}																																																		\
	}																																																			\
	else {																																																\
		assert(var_decision_level == conflict_point_highest_decision_level);																\
		if (!marks[VAR_IDX]) {																																							\
			marks[VAR_IDX] = true;																																						\
			num_marked_variables++;																																						\
		}																																																		\
	}																																																			\
} ((void) 0)

bool LPSolver::conflict_resolution_first_uip()
{
	SOLVER_TRACE(ctrace << "conflict resolution [first uip strategy]...\n";);
	assert(check_marks());
	
	unsigned int num_marked_variables = 0;
	assert(check_marks());

	assert(new_conflict_point.get_size() > 0);

	static growable_vector<int> new_conflict_clause;
	new_conflict_clause.reset();

	unsigned new_conflict_clause_second_highest_decision_level = 0;
	unsigned new_conflict_clause_second_highest_decision_level_literal_pos;

	unsigned int UIP = 0;

	// compute the conflict_point_highest_decision_level
	unsigned int conflict_point_highest_decision_level = 0;
	unsigned int n = new_conflict_point.get_size();
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = absolute(new_conflict_point.get(i));
		if (internal_formulas[var_idx].decision_level > conflict_point_highest_decision_level)
			conflict_point_highest_decision_level = internal_formulas[var_idx].decision_level;
	}

	if (conflict_point_highest_decision_level == 0) {
		SOLVER_TRACE(ctrace << "  conflict point = " << new_conflict_point << endl;);
		SOLVER_TRACE(ctrace << "  conflict point highest decision level == 0\n";);
		return false;
	}
	
	// mark/add variables in the conflict point
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = absolute(new_conflict_point.get(i));
		PROCESS_VAR_IN_UIP_STRATEGY(var_idx);
	}

	unsigned int num_levels_to_backtrack = decision_level - conflict_point_highest_decision_level + 1;
	
	// analyze the trail stack.
	int pos = trail_stack.get_size() - 1;
	while(true) {
		DBG_CODE(if (pos <= 0) {
			cerr << "BUG detected: \n";
			cerr << "  marks:\n";
				for (unsigned int i = 1; i <= num_formulas; i++)
					cerr << "    " << i << ": " << marks[i] << endl;
			cerr << "  trail-stack = " << trail_stack << endl;
			cerr << "  num_marked_variables = " << num_marked_variables << endl;
			cerr << "  UIP = " << UIP << endl;
			cerr << "  conflict-point = " << new_conflict_point << endl;
			assert(false);
		});
		unsigned int var_idx = trail_stack.get(pos--);
		unsigned int var_decision_level = internal_formulas[var_idx].decision_level;
		// three cases:
		// 1 - var_decision_level > conflict_point_highest_decision_level
		//     do nothing... the variable is irrelevant for this conflict resolution procedure
		// 2 - var_decision_level < conflict_point_highest_decision_level
		// 	   do nothing... this variable is not in the correct place... the backtrack function will move it...
		// 3 - var_decision_level == conflict_point_highest_decision_level
		//     the interesting case!
		if (var_decision_level == conflict_point_highest_decision_level) {
			if (marks[var_idx]) {
				marks[var_idx] = false;
				num_marked_variables --;
				if (num_marked_variables == 0) {
					// UIP was detected...
					UIP = var_idx;
					// add UIP in the last position of the new conflict clause.
					new_conflict_clause.push(get_formula_value(var_idx) == 1 ? -var_idx : var_idx);
					break;
				} // if (num_marked_variables == 0)
				// this cannot be a decided variable... since decided variable are in the worst case the first UIP
				DBG_CODE(if (internal_formulas[var_idx].antecedent == 0) {
					cerr << "BUG DETECTED: \n";
					cerr << "  marks:\n";
					for (unsigned int i = 1; i <= num_formulas; i++)
						cerr << "    " << i << ": " << marks[i] << endl;
					cerr << "  trail-stack = " << trail_stack << endl;
					assert(false);
				});
				// only clausal constraint propagation is supported right now.
				assert(internal_formulas[var_idx].antecedent < 0);
				unsigned int antecedent_clause_idx = - internal_formulas[var_idx].antecedent;
				LPClause & antecedent_clause = clauses.get(antecedent_clause_idx);
				// mark/add literals in the antecedent clause
				unsigned int n = antecedent_clause.get_size();
				for (unsigned int i = 0; i < n; i++) {
					LPFormulaId literal = antecedent_clause.get_literal(i);
					unsigned int literal_idx = absolute(literal);
					if (literal_idx != var_idx) {
						PROCESS_VAR_IN_UIP_STRATEGY(literal_idx);
					}
				} // for (unsigned int i = 0; i < n; i++)
			} // if (marks[var_idx])
		} 
	} // end of the conflict clause generation main loop
	
	assert(check_marks());

	// reset auxiliar array added_literals
	n = new_conflict_clause.get_size();
	for (unsigned int i = 0; i < n; i++) {
		added_literals[absolute(new_conflict_clause.get(i))] = false;
	}

	DBG_CODE(for (unsigned int i = 0; i <= num_formulas; i++) 
					   if (added_literals[i])
						   assert(false);
					 );

	
	SOLVER_EXPLAIN_TRACE(ctrace << "  explain function result:\n";
											 unsigned int n = new_conflict_clause.get_size();
											 ctrace << "    ";
											 for (unsigned int i = 0; i < n; i++) {
												 LPFormulaId id = new_conflict_clause.get(i);
												 unsigned int var_idx = absolute(id);
												 ctrace << id << ":" << internal_formulas[var_idx].decision_level << " ";
											 }
											 ctrace << endl;
											 ctrace << "    conflict decision-level = " << conflict_point_highest_decision_level << endl;
											 ctrace << "    second highest decision-level = " << new_conflict_clause_second_highest_decision_level << endl;
											 ctrace << "    second highest decision-level literal_pos = " << new_conflict_clause_second_highest_decision_level_literal_pos << endl;
											 ctrace << "    current decision-level = " << decision_level << endl;
											 );
	
	int new_antecedent = 0;
	bool found_fix_value = false;
	if (new_conflict_clause.get_size() > 1) {
		LPClause * new_clause = add_clause(new_conflict_clause.get_contents(), new_conflict_clause.get_size(), 
																			 new_conflict_clause_second_highest_decision_level_literal_pos,
																			 new_conflict_clause.get_size() - 1,
																			 false);
		new_antecedent = - clauses.get_id(new_clause);
	}
	else 
		found_fix_value = true;

	// return conflict_resolution_only_decided(); // trying both techniques...
	
	int UIP_val = get_formula_value(UIP);

	if (!backtrack(num_levels_to_backtrack))
		return false;
	
	// remove decision variable... it is not always the case that UIP is a decision variable
	unsigned int decided_var_idx = trail_stack.pop();
	undo_assignment(decided_var_idx);

	assert(is_unknown(UIP));

	SOLVER_TRACE(ctrace << "  conflict solved, assigning " << UIP << " = " << (-UIP_val) << endl;);
	bool assign_result = assign_formula(UIP, -UIP_val, new_antecedent); /* invert value */
	assert(assign_result);
	
	if (found_fix_value) {
		// set the decision level to 0
		internal_formulas[UIP].decision_level = 0;
	}
	
	return true;	
}

void LPSolver::explain_ics_inconsistency(unsigned int f_idx)
{
	clock_t start = clock();	

	num_ics_detected_inconsistencies++;

	if (ics_interface.use_ics_explain()) {
  	if (ics_interface.is_explained()) {
	  	pair<int *,int> explanation = ics_interface.explain();
		  int * atom_array = explanation.first;
		  int array_len = explanation.second;
			new_conflict_point.reset();
//			cout << "ICS explanation: \n";
		  for(int i = 0; i < array_len; i++) {
				int formula_id = ics_interface.get_formula_id_of_atom_id(atom_array[i]);
//  				cout << "assert "; cout.flush();
//  				icsat_atom_pp(atom_array[i]);
//  				cout << ".\n";
//  				cout.flush();
//  				cout << "id = " << formula_id << "\n";
//  				cout << "pos-atom : "; cout.flush();
//  				icsat_atom_pp(formula_manager->get_formula(abs(formula_id))->get_atom());
//  				cout << endl;
//  				cout << "neg-atom : "; cout.flush();
//  				icsat_atom_pp(formula_manager->get_formula(abs(formula_id))->get_not_atom());
// 				int val = get_formula_value(abs(formula_id));
//  				cout << endl;
// 				cout << "value = " << val << endl << endl;

				new_conflict_point.push(- formula_id);
		  }
//			cout << "---------------------\n";
			return;
	  }
	  else {
	  	cerr << "Error: there isn't an explanation in ICS...\n";
		  exit(-1);
	  }
	}
		
	queue<unsigned int> & to_do = tmp_queue;
	to_do.reset();
	assert(check_marks());
	to_do.push(f_idx); marks[f_idx] = true;
	while (!to_do.is_empty()) {
		unsigned int curr_idx = to_do.pop();
		vector<unsigned int> & assoc_formulas = ics_interface.get_associated_formulas(curr_idx);
		vector<unsigned int>::const_iterator it = assoc_formulas.begin();
		for (; it != assoc_formulas.end(); it++) {
			unsigned int assoc_idx = *it;
			if (!marks[assoc_idx] && (processed == NULL || processed[assoc_idx]) && !is_unknown(assoc_idx)) {
				to_do.push(assoc_idx);
				marks[assoc_idx] = true;
			}
		}
	}

	// to_do list contains all relevant literals...
	SOLVER_EXPLAIN_TRACE(ctrace << "  relevant atomic formulas for ICS inconsistency:\n";
											 unsigned int n = to_do.get_num_elems_inserted();
											 for (unsigned int i = 0; i < n; i++) {
												 ctrace << "    ";
												 formula_manager->dump_formula(ctrace, to_do.low_level_get(i));
												 ctrace << endl;
											 }
											 ctrace << "    number of relevant formulas: " << to_do.get_num_elems_inserted() << endl;
											 if (branching_mode == LP_ACTIVATION_MODE) {
												 ctrace << "  ---- list of all processed atoms:\n";
												 n = processed_trail_stack.get_size();
												 unsigned int num_processed_atomic_formulas = 0;
												 for (unsigned int i = 0; i < n; i++) {
													 unsigned int curr_idx = processed_trail_stack.get(i);
													 const LPFormula * formula = formula_manager->get_formula(curr_idx);
													 if (formula->is_eq()) {
														 num_processed_atomic_formulas++;
														 ctrace << "    ";
														 formula_manager->dump_formula(ctrace, curr_idx);
														 ctrace << endl;
													 }
												 }
												 ctrace << "    total number of atomic formulas: " << num_processed_atomic_formulas << endl;
											 });
	
	assert(to_do.low_level_get(0) == f_idx);
	static growable_vector<unsigned int> cached;
	cached.reset();
	cached.push(f_idx);
	unsigned int low = 1;
	unsigned int high = to_do.get_num_elems_inserted() - 1;
	unsigned int highest_decision_level = internal_formulas[f_idx].decision_level;
	bool up = true;
	unsigned int j;
	if (ICS_EXPLAIN_NUM_REFINEMENTS > 0) {
		for (j = 0; j < ICS_EXPLAIN_NUM_REFINEMENTS; j++) {
			ics_interface.reset_scratch_state();
			// assert cached literals...
			for (unsigned int i = 0; i < cached.get_size(); i++) {
				unsigned int f_idx = cached.get(i);
				assert(!is_unknown(f_idx));
				int value = get_formula_value(f_idx);
				assert(value == 1 || value == -1);
				bool is_consistent;
				if (value == 1)
					is_consistent = ics_interface.assert_formula_in_scratch_state(f_idx);
				else
					is_consistent = ics_interface.assert_formula_in_scratch_state(-f_idx);
				if (!is_consistent)
					goto end_for; // the minimal set was detected...
			}
			if (up) {
				for (unsigned int i = low; i <= high; i++) {
					unsigned int f_idx = to_do.low_level_get(i);
					assert(!is_unknown(f_idx));
					int value = get_formula_value(f_idx);
					assert(value == 1 || value == -1);
					bool is_consistent;
					if (value == 1)
						is_consistent = ics_interface.assert_formula_in_scratch_state(f_idx);
					else
						is_consistent = ics_interface.assert_formula_in_scratch_state(-f_idx);
					if (!is_consistent) {
						cached.push(f_idx);
						if (internal_formulas[f_idx].decision_level > highest_decision_level)
							highest_decision_level = internal_formulas[f_idx].decision_level;
						SOLVER_EXPLAIN_TRACE(ctrace << "  new high: " << (i-1) << ", previous: " << high << endl;);
						high = i - 1;
						up = false;
						break;
					}
				}
			}
			else {
				for (unsigned int i = high; i >= low; i--) {
					unsigned int f_idx = to_do.low_level_get(i);
					assert(!is_unknown(f_idx));
					int value = get_formula_value(f_idx);
					assert(value == 1 || value == -1);
					bool is_consistent;
					if (value == 1)
						is_consistent = ics_interface.assert_formula_in_scratch_state(f_idx);
					else
						is_consistent = ics_interface.assert_formula_in_scratch_state(-f_idx);
					if (!is_consistent) {
						cached.push(f_idx);
						if (internal_formulas[f_idx].decision_level > highest_decision_level)
							highest_decision_level = internal_formulas[f_idx].decision_level;
						SOLVER_EXPLAIN_TRACE(ctrace << "  new low: " << (i+1) << ", previous: " << low << endl;);
						low = i + 1;
						up = true;
						break;
					}
				}
			}
			assert(ICS_EXPLAIN_NUM_REFINEMENTS > 0);
			// the following typecast is intended to avoid warning messages generated by the compiler
			if (j == (unsigned int ) (ICS_EXPLAIN_NUM_REFINEMENTS - 1)) { // this is the last step 
				for (unsigned int i = low; i <= high; i++)
					cached.push(to_do.low_level_get(i));
			}
		}
	end_for:
		((void)0);
	}
	else {
		// HACK... ICS_EXPLAIN_NUM_REFINEMENTS == 0... to generate numbers....
		for (unsigned int i = low; i <= high; i++)
			cached.push(to_do.low_level_get(i));
	}
	
	SOLVER_EXPLAIN_TRACE(
	{
                       cout << "  explain function resul:\n";
											 unsigned int n = cached.get_size();
											 for (unsigned int i = 0; i < n; i++) {
												 cout << "    ";
												 formula_manager->dump_formula(cout, cached.get(i));
												 cout << " = " << get_formula_value(cached.get(i)) << ", decision-level = " << 
													 internal_formulas[cached.get(i)].decision_level << endl;
											 }
											 cout << "    highest decision-level = " << highest_decision_level << endl;
											 cout << "    current decision-level = " << decision_level << endl;
											 cout << "    number of atoms: " << cached.get_size() << endl;
	}
	);


	clock_t end = clock();	
 	ics_explain_time += ((double) (end - start)) / CLOCKS_PER_SEC;
	remove_marks_from_tmp_queue();

	// set the new conflict point
	new_conflict_point.reset();
	unsigned int n = cached.get_size();
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = cached.get(i);
		int val = get_formula_value(var_idx);
		// cout << "---idx: " << (val < 0 ? -((int)var_idx) : ((int)var_idx)) << endl;
		new_conflict_point.push(val < 0 ? -var_idx : var_idx);
	}

	// Obs.: the ICS conflict point so far can be added to the database!
//  	{ // this code should be improved... I should create a new function 
//  		unsigned highest_decision_level = 0;
//  	  int highest_decision_level_literal_pos = -1;
//  		unsigned second_highest_decision_level = 0;
//  		int second_highest_decision_level_literal_pos = -1;

//  		unsigned int n = new_conflict_point.get_size();
//  		for (unsigned int i = 0; i < n; i++) {
//  			assert(get_formula_value(new_conflict_point.get(i)) == -1);
//  			unsigned int f_idx = absolute(new_conflict_point.get(i));
//  			unsigned int f_decision_level = internal_formulas[f_idx].decision_level;
//  			if ( f_decision_level > highest_decision_level) {
//  				second_highest_decision_level = highest_decision_level;
//  				second_highest_decision_level_literal_pos = highest_decision_level_literal_pos;
//  				highest_decision_level = f_decision_level;
//  				highest_decision_level_literal_pos = i;
//  			}
//  			else if (f_decision_level > second_highest_decision_level) {
//  				second_highest_decision_level = f_decision_level;
//  				second_highest_decision_level = i;
//  			}
//  		}
		
//  		assert(highest_decision_level_literal_pos != -1);
//  		assert(second_highest_decision_level_literal_pos != -1);
//  		add_clause(new_conflict_point.get_contents(), new_conflict_point.get_size(),
//  							 highest_decision_level_literal_pos,
//  							 second_highest_decision_level_literal_pos,
//  							 false);
//  	}

}


void LPSolver::set_conflict_point(int conflict_point)
{
	new_conflict_point.reset();

	if (conflict_point < 0) {
		unsigned int clause_idx = -conflict_point;
		LPClause & clause = clauses.get(clause_idx);
		unsigned int n = clause.get_size();
		for (unsigned int i = 0; i < n; i++)
			new_conflict_point.push(clause.get_literal(i));
	}
	else {
		feature_not_implemented_yet();
	}
}
