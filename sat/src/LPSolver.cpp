/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 11, 2002: Created.
***/

#include<time.h>
#include"sort.h"
#include"LPSolver.h"

bool LPClause::subsumes(LPClause * c) const {
	if (c->deleted)
		return false;
	if (size > c->get_size())
		return false;
	unsigned int i = 0;
	unsigned int j = 0;
	while (true) {
		if (i == size)
			return true;
		if (j == c->size)
			return false;
		LPFormulaId l1 = literals[i];
		LPFormulaId l2 = c->literals[j];
		if (l1 == l2) {
				i++; j++;
		}
		else if (absolute(l1) < absolute(l2))
			return false;
		else 
			j++;
	}
}

LPSolver::LPSolver(LPFormulaManager * f_manager, unsigned int initial_capacity, unsigned int initial_number_of_clauses,
									 unsigned initial_number_of_literals):
	trail_stack(f_manager->get_num_formulas()),
	backtrack_point_stack(f_manager->get_num_formulas()),
	clauses(initial_number_of_clauses),
	ics_interface(f_manager)
{
	formula_manager = f_manager;
	num_formulas = formula_manager->get_num_formulas();
	capacity_formulas = max(initial_capacity, get_internal_formulas_array_size());
	assignments = new int[capacity_formulas];
 	// in_to_process_queue = new bool[capacity_formulas];
 	// memset(in_to_process_queue, 0, sizeof(bool) * capacity_formulas);
	internal_formulas = new LPInternalFormulaInfo[capacity_formulas];
	// initialize internal information...
	for (unsigned int idx = 1; idx <= num_formulas; idx++)
		reset_formula(idx);
	decision_level = 0;
	literals = new LPFormulaId[initial_number_of_literals];
	capacity_literals = initial_number_of_literals;
	num_literals = 0;
	clauses.allocate(); // the first slot is not used...
	clause_relevance = DEFAULT_CLAUSE_RELEVANCE;
	cleanup_period = DEFAULT_CLEANUP_PERIOD;
	num_clausal_positive_occurrences = new unsigned int[capacity_formulas];
	num_clausal_negative_occurrences = new unsigned int[capacity_formulas];
	num_deleted_clauses = 0;
	marks = new bool[capacity_formulas];
	memset(marks, 0, sizeof(bool) * capacity_formulas);
	added_literals = new int[capacity_formulas];
	memset(added_literals, 0, sizeof(int) * capacity_formulas);
	active = NULL;
	processed = new bool[capacity_formulas];
	memset(processed, 0, sizeof(bool) * capacity_formulas);
	verbose = false;
	branching_mode = LP_ACTIVATION_MODE; 
	if (branching_mode == LP_ACTIVATION_MODE)
		allocate_activation_mode_arrays();
	conflict_resolution_mode = LP_ONLY_DECIDED_CR;
	randomness_level = 0;
	polarity_optimization = false;
	implication_graph_optimization = false;
	lookahead_optimization = false;
	lookahead_relevance = DEFAULT_LOOKAHEAD_RELEVANCE;
	npc_threshold = DEFAULT_NPC_THRESHOLD;
}

LPSolver::~LPSolver()
{
	DBG_CODE(cout << "Destroying LPSolver\n";);
	// cout << ">>Destroying LPSolver\n";
	delete[] literals;
	delete_dynamic_arrays();
	// cout << "<<Destroying LPSolver\n";
}

void LPSolver::delete_dynamic_arrays()
{
	MEM_TRACE(ctrace << "  [memory] deleting dynamic arrays.\n";);
	delete[] marks;
	delete[] processed;
	delete[] added_literals;
	delete[] internal_formulas;
	delete[] assignments;
	delete[] num_clausal_negative_occurrences;
	delete[] num_clausal_positive_occurrences;
	// delete[] in_to_process_queue;
	deallocate_activation_mode_arrays();
}

void LPSolver::expand_formula_capacity() 
{
	MEM_TRACE(ctrace << "  [memory] expanding formula capacity.\n";);
	double grow_factor = 1.2;
	expand_array(internal_formulas, capacity_formulas, grow_factor, false);
	expand_array(assignments, capacity_formulas, grow_factor, false);
	expand_array(num_clausal_positive_occurrences, capacity_formulas, grow_factor, false);
	expand_array(num_clausal_negative_occurrences, capacity_formulas, grow_factor, false);
	expand_array(marks, capacity_formulas, grow_factor, false);
	expand_array(added_literals, capacity_formulas, grow_factor, false);
	expand_array(processed, capacity_formulas, grow_factor, false);
	if (active != NULL)
		expand_array(active, capacity_formulas, grow_factor, false);
	capacity_formulas = (unsigned int) (capacity_formulas * grow_factor);
}

void LPSolver::add_new_formulas(unsigned int num_new_formulas)
{
	// I'm adding 1, since the first slot (0) is not used.
	while (num_new_formulas + num_formulas + 1 >= capacity_formulas)
		expand_formula_capacity();
	for (unsigned int idx = num_formulas + 1; idx <= num_formulas + num_new_formulas; idx++)
		reset_formula(idx);
	num_formulas += num_new_formulas;
}

bool LPSolver::assign_formula(LPFormulaId target, LPFormulaId value, int antecedent)
{
	assert(IMPLY(!searching, antecedent == 0));

	assert(target != 0 && value != 0);
	LPFormulaId can1 = canonical_formula(target);
	LPFormulaId can2 = canonical_formula(value);
	if (can1 == can2)
		return true; 
	else if (can1 == -can2) {
		SOLVER_TRACE(ctrace << "  conflict detected in assignment " << target << " [";
								 formula_manager->dump_formula(ctrace, target);
								 ctrace << "] = " << value << " [";
								 formula_manager->dump_formula(ctrace, value);
								 ctrace << "]\n";);
		if (searching)
			set_conflict_point(antecedent);
		return false; // conflict detected...
	}
	else {
		if (antecedent != 0)
			num_deductions++;

		if (abs(can1) < abs(can2)) 
			swap(can1, can2);
		
		bool sign1 = can1 < 0;
		unsigned int can1_idx = sign1 ? -can1 : can1;
		
		if (searching) 
			to_process_queue_push(can1_idx); // searching mode only uses normalized clausal constraints
		else
			add_equivalence_class_to_queue(can1_idx);
		assert(assignments[can1_idx] == 0);
		
		unsigned formula_decision_level;
		if (can2 == LPTrueId || can2 == LPFalseId) {
			// simple case...
			
			// save state...
			trail_stack.push(can1_idx);
			
			LPInternalFormulaInfo & info1 = internal_formulas[can1_idx];
			assert(info1.antecedent == 0);
			
			formula_decision_level = get_curr_decision_level(antecedent);
			info1.decision_level = formula_decision_level;
			info1.antecedent = antecedent;
			assignments[can1_idx] = sign1 ? -can2 : can2;
		}
		else {
			assert(!searching);
			// merge equivalence classes...
			bool sign2 = can2 < 0;
			unsigned int can2_idx = sign2 ? -can2 : can2;
			assert(assignments[can2_idx] == 0);
			
			// we also have to add the elements in the equivalence class associated with can2
			add_equivalence_class_to_queue(can2_idx);
			
			// save state...
			trail_stack.push(can1_idx);
			
			LPInternalFormulaInfo & info1 = internal_formulas[can1_idx];
			assert(info1.antecedent == 0);
			LPInternalFormulaInfo & info2 = internal_formulas[can2_idx];
			assert(info2.antecedent == 0);
			
			formula_decision_level = get_curr_decision_level(antecedent);
			info1.decision_level = formula_decision_level;
			info1.antecedent = antecedent; 
			assignments[can1_idx] = sign1 ? -can2 : can2;
			
			LPFormulaId tmp = info1.next;
			if (sign1 == sign2) {
				info1.next = info2.next;
				info2.next = tmp;
			}
			else {
				assert(sign1 == !sign2);
				info1.next = - info2.next;
				info2.next = - tmp;
			}
		}
		SOLVER_TRACE(ctrace << "  assigning: " << can1 << " [";
								 formula_manager->dump_formula(ctrace, can1);
								 ctrace << "] = " << can2 << " [";
								 formula_manager->dump_formula(ctrace, can2);
								 ctrace << "], decision_level = " << formula_decision_level << endl;);
		return true;
	}
}

// Important:
// the following method can only be used to assert the root of an equivalence class to "true/false"
bool LPSolver::fast_assign_formula(LPFormulaId target, LPFormulaId value, int antecedent)
{
	bool sign = target < 0;
	int target_idx = target;
	if (sign) {
		target_idx = - target_idx;
		value = - value;
	}

	assert(target_idx > 0);
	assert(value == 1 || value == -1);
	assert(assignments[target_idx] == 0 || assignments[target_idx] == -1 || assignments[target_idx] == 1);
	assert(searching);
	assert(IMPLY(!searching, antecedent == 0));

	if (assignments[target_idx] == value)
		return true; 
	else if (assignments[target_idx] == -value) {
		SOLVER_TRACE(ctrace << "  conflict detected in assignment " << target << " [";
								 formula_manager->dump_formula(ctrace, target);
								 ctrace << "] = " << value << " [";
								 formula_manager->dump_formula(ctrace, value);
								 ctrace << "]\n";);
		set_conflict_point(antecedent);
		return false;
	}
	else {
		if (antecedent != 0)
			num_deductions++;
		if (formula_manager->get_formula(target_idx)->is_eq()) {
			// this "if" statement is an approximation, since target_idx can be the root of an equivalence class that 
			// contains LP_EQ nodes. In other words, we are only "counting" the LP_EQ nodes that are roots of 
			// an equivalence class.
			num_new_non_propositional_assignments++;
		}
		to_process_queue_push(target_idx); // searching mode only uses normalized clausal constraints
		assert(assignments[target_idx] == 0);
		trail_stack.push(target_idx);
		internal_formulas[target_idx].decision_level = get_curr_decision_level(antecedent);
		internal_formulas[target_idx].antecedent = antecedent;
		assignments[target_idx] = value;
		SOLVER_TRACE(ctrace << "  assigning: " << target_idx << " [";
								 formula_manager->dump_formula(ctrace, target_idx);
								 ctrace << "] = " << value << " [";
								 formula_manager->dump_formula(ctrace, value);
								 ctrace << "], decision_level = " << get_curr_decision_level(antecedent) << endl;);
		return true;
	}
}

void LPSolver::add_equivalence_class_to_queue(unsigned int formula_idx)
{
	to_process_queue_push(formula_idx);
	unsigned int curr = absolute(internal_formulas[formula_idx].next);
	while (curr != formula_idx) {
		to_process_queue_push(curr);
		curr = absolute(internal_formulas[curr].next);
	}
}

void LPSolver::dump_internal_info(ostream & target)
{
	assert(check_invariant());
	target << "Solver\n---------------\n";

	target << "1 : true\n";
	for (unsigned int idx = 2; idx <= num_formulas; idx++) {
		if (assignments[idx] == 0 || assignments[idx] == 1 || assignments[idx] == -1) {
			target << idx << " : ";
			switch(assignments[idx]) {
			case 0:	target << "(unassigned)"; break;
			case 1: target << "true"; break;
			case -1: target << "false"; break;
			default:
				assert(false);
			}
			LPInternalFormulaInfo & info = internal_formulas[idx];
			if (absolute(info.next) != idx) {
				target << ", eq_class = (" << idx;
				unsigned int curr = abs(info.next);
				bool curr_sign = info.next < 0;
				while (curr != idx) {
					target << " " << (curr_sign ? "-" : "") << curr;
					curr_sign = internal_formulas[curr].next < 0 ? !curr_sign : curr_sign;
					curr = abs(internal_formulas[curr].next);
				}
				target << ")";
			}
			target << ", antecedents = (";
			if (info.antecedent != 0)
				target << info.antecedent;
			unsigned int curr = abs(info.next);
			while (curr != idx) {
				if (internal_formulas[curr].antecedent != 0)
					target << " " << internal_formulas[curr].antecedent;
				curr = abs(internal_formulas[curr].next);
			}
			target << ")";

			target << endl;
		}
		else 
			target << idx << " : " << assignments[idx] << endl;
	}
}

//
// - If all literals in lits are unassigned, then the watch literals position doesn't matter.
//   Actually, we use the first and last literals in this case.
//
// - If some of the literals in lits are assigned, then the user MUST specify the watch literal
//   positions...
//
LPClause * LPSolver::add_clause(LPFormulaId * lits, unsigned int num_lits, unsigned int w1, unsigned int w2, bool is_main_clause)
{
	static sortFcn<LPFormulaId, LPFormulaIdLt> sort;

	DBG_CODE(for (unsigned int i = 0; i < num_lits; i++)
					 assert(lits[i] == get_eq_class_root(lits[i])););

	request_more_literals(num_lits+1); /* we have to add 1, since we save a pointer to the clause object... */

	if (remove_subsumed_clauses) {
		int val1 = lits[w1];
		int val2 = lits[w2];
		sort(lits, num_lits);
		// adjust w1 and w2 position...
		for (unsigned int i = 0; i < num_lits; i++) {
			if (lits[i] == val1)
				w1 = i;
			else if (lits[i] == val2)
				w2 = i;
		}
	}

	LPClause * new_clause = NULL;
	if (!free_clauses.is_empty()) 
		new_clause = free_clauses.pop();
	else 
		new_clause = clauses.allocate();
	unsigned int new_clause_idx = clauses.get_id(new_clause);

	literals[num_literals++] = (LPFormulaId) new_clause_idx;

	LPFormulaId * ptr = &(literals[num_literals]);
	// update total number of allocated literals...
	num_literals += num_lits;
	LPFormulaId * watch1 = &(ptr[w1]);
	LPFormulaId * watch2 = &(ptr[w2]);
	assert(watch1 != watch2);
	new_clause->init(ptr, num_lits, is_main_clause, watch1, watch2);

	unsigned int min_occurrences = UINT_MAX;
	unsigned int min_occurrences_pos = 0;
	for (unsigned int i = 0; i < num_lits; i++, ptr++) {
		unsigned int id = absolute(lits[i]);
		LPInternalFormulaInfo & info = internal_formulas[id];
		*ptr = lits[i];
		if (remove_subsumed_clauses) {
			if (info.all_clausal_positive_occurrences == NULL) { // initialize vectors if necessary
				info.all_clausal_positive_occurrences = new LPClauseVector();
				info.all_clausal_negative_occurrences = new LPClauseVector();
			}
			unsigned int curr_occurs;
			if (lits[i] < 0) { 
				info.all_clausal_negative_occurrences->push(new_clause_idx);
				curr_occurs = info.all_clausal_negative_occurrences->get_size();
			}
			else {
				info.all_clausal_positive_occurrences->push(new_clause_idx);
				curr_occurs = info.all_clausal_positive_occurrences->get_size();
			}
			if (curr_occurs < min_occurrences) {
				min_occurrences = curr_occurs;
				min_occurrences_pos = i;
			}
		}
		// update clausal occurrences...
		if (lits[i] < 0) {
			num_clausal_negative_occurrences[id]++;
			if (ptr == watch1 || ptr == watch2)
				info.clausal_negative_occurrences.push(new_clause_idx);
		}
		else {
			num_clausal_positive_occurrences[id]++;
			if (ptr == watch1 || ptr == watch2)
				info.clausal_positive_occurrences.push(new_clause_idx);
		}
	}
	assert(new_clause->check_invariant());

	// collect statistic information
	if (!is_main_clause) {
		// conflict clause
		if (num_lits <= 10)
			num_generated_conflict_clauses_smaller_than_10++;
		else if (num_lits <= 30)
			num_generated_conflict_clauses_smaller_than_30++;
		else if (num_lits <= 50)
			num_generated_conflict_clauses_smaller_than_50++;
		else if (num_lits <= 100)
			num_generated_conflict_clauses_smaller_than_100++;
		else 
			num_generated_conflict_clauses_larger_than_100++;
	}

	// ctrace << "adding clause: " << *new_clause << endl;
	if (remove_subsumed_clauses) {
		// we use the literal with less occurrences;
		unsigned int idx = absolute(lits[min_occurrences_pos]);
		LPInternalFormulaInfo & info = internal_formulas[idx];
		LPClauseVector * v = lits[min_occurrences_pos] < 0 ? info.all_clausal_negative_occurrences : info.all_clausal_positive_occurrences;
		unsigned int n = v->get_size();
		unsigned int j = 0;
		for (unsigned int i = 0; i < n; i++) {
			unsigned int curr_clause_idx = v->get(i);
			LPClause * curr = clauses.get_ptr(curr_clause_idx);
			if (!curr->was_deleted()) {
				if (new_clause != curr && new_clause->subsumes(curr) && !curr->is_main_clause()) {
					SOLVER_TRACE(ctrace << "  SUBSUMPTION: " << endl;
											 ctrace << "    " << new_clause_idx << " : " << *new_clause << endl;
											 ctrace << "    " << curr_clause_idx << " : " << *curr << endl;);
					curr->set_as_deleted();
					num_subsumed_clauses++;
				}
				else {
					// keep curr_clause_idx in "v"
					v->set(j++, curr_clause_idx);
				}
			}
			else {
				// do nothing...
				// does not update j, that is, removes curr_clause_idx from "v"
			}
		}
		v->resize(j);
	}

	return new_clause;
}

#define CLEANUP_ADDED_LITERALS() {							\
	for (unsigned int i = 0; i < num_lits; i++)		\
		added_literals[absolute(lits[i])] = false; 	\
} ((void) 0)

LPClause * LPSolver::add_clause(LPFormulaId * lits, unsigned int num_lits)
{	
	unsigned int j = 0;
	for (unsigned int i = 0; i < num_lits; i++) {
		LPFormulaId curr = get_eq_class_root(lits[i]);

		unsigned int curr_idx = absolute(curr);
		assert(assignments[curr_idx] == 1 || assignments[curr_idx] == -1 || assignments[curr_idx] == 0);
		if ((added_literals[curr_idx] > 0 && curr < 0) || 
				(added_literals[curr_idx] < 0 && curr > 0)) {
			CLEANUP_ADDED_LITERALS();
			return NULL; // trivially true;
		}
		if (added_literals[curr_idx] == 0) {
			added_literals[curr_idx] = curr;
			lits[j++] = curr;
		}
	}

	num_lits = j;

	// reset added_literals
	CLEANUP_ADDED_LITERALS();

	int watch1 = -1;
	int watch2 = -1;

	for (unsigned int i = 0; i < num_lits; i++) {
		int curr_val = get_formula_value(lits[i]);
		if (curr_val == 1)
			return NULL; // clause is trivially true...
		if (curr_val == 0) {
			if (watch1 == -1)
				watch1 = i;
			else
				watch2 = i;
		}
	}
	assert(watch1 != -1 && watch2 != -1);
	return add_clause(lits, num_lits, watch1, watch2);
}

void LPSolver::remove_irrelevant_clauses()
{
	unsigned int num_clauses = clauses.get_size();
	for (unsigned int i = 1; i < num_clauses; i++) { // the first slot in the clause array is not used
		LPClause * clause = clauses.get_ptr(i);
		if (!clause->is_main_clause() && !clause->was_deleted()) {
			unsigned int n = clause->get_size();
			unsigned int num_unassigned = 0;
			for (unsigned int j = 0; j < n; j++) {
				LPFormulaId curr = clause->get_literal(j);
				if (is_unknown(curr))
					num_unassigned++;
			}
			if (num_unassigned > clause_relevance) {
				clause->set_as_deleted();
				num_deleted_clauses++;
			}
		}
	}
}

void LPSolver::expand_literals()
{
	if (verbose) {
		cerr << "*";
		cerr.flush();
	}
	unsigned int grow_factor = 2;
	unsigned int new_capacity_literals = capacity_literals * grow_factor;
	LPFormulaId * new_literals = new LPFormulaId[new_capacity_literals];
	int delta = new_literals - literals;
	memcpy(new_literals, literals, sizeof(LPFormulaId) * capacity_literals);
	MEM_TRACE(ctrace << "[memory] expanding literals: (" << capacity_literals << ", " << literals << ") -> (" << 
						new_capacity_literals << ", " << new_literals << ")\n";);

	capacity_literals = new_capacity_literals;

	// update clauses...
	unsigned int num_clauses = clauses.get_size();
	for (unsigned int i = 1; i < num_clauses; i++) { // the first slot in the clause array is not used
		LPClause * clause = clauses.get_ptr(i);
		clause->literals = clause->literals + delta;
		clause->watch1 = clause->watch1 + delta;
		clause->watch2 = clause->watch2 + delta;
		assert(clause->check_invariant());
	}
	
	delete[] literals;
	literals = new_literals;
}

void LPSolver::compact_literals()
{
	// TO DO:
	// I have to check this function... I think it is not correct, since variables may have pointer to deleted clauses.
	//

	if (verbose) {
		cerr << "!";
		cerr.flush();
	}

	LPFormulaId * ptr = literals;
	LPFormulaId * curr = literals;
	LPFormulaId * end = &(literals[num_literals]) + 1;
	while (ptr < end) {
		unsigned int clause_idx = (unsigned int) *ptr;
		LPClause * clause = clauses.get_ptr(clause_idx);
		assert(clause->check_invariant());
		if (clause->was_deleted()) {
			ptr++;
			ptr += clause->get_size();
			free_clauses.push(clause); // recycle clause...
		}
		else {
			*curr = *ptr;
			ptr++; curr++;
			unsigned int n = clause->get_size();
			clause->literals = curr;
			for (unsigned int i = 0; i < n; i++) {
				*curr = *ptr;
				if (ptr == clause->get_watch1())
					clause->set_watch1(curr);
				if (ptr == clause->get_watch2())
					clause->set_watch2(curr);
				curr++;
				ptr++;
			}
		}
	}
	num_literals = curr - literals;
}

void LPSolver::request_more_literals(unsigned int num_lits)
{
	if (num_lits + num_literals < capacity_literals)
		return;

	if (num_deleted_clauses * 2 > clauses.get_size()) {
		// too much fragmentation... compacting literals...
		compact_literals();
		if (num_lits + num_literals < capacity_literals)
			return;
	}

	// expand literals...
	expand_literals();
}

void LPSolver::dump_to_check_vector(ostream & target) {
	unsigned int n = to_check.get_size();
	for (unsigned int i = 0; i < n; i++) {
		unsigned int c = to_check.get(i);
		target << c << " ";
	}
	target << endl;
}

void LPSolver::initialize_solver() {
	next_to_check = 0;
	preproc_time = 0.0;
	polarity_optimization_time = 0.0;
	solver_time = 0.0;
	branching_time = 0.0;
	conflict_resolution_time = 0.0;
	constraint_propagation_time = 0.0;
	ics_explain_time = 0.0;
	ics_interface.reset();
	num_backtracks = 0;
	num_non_chronological_backtracking = 0;
	num_ics_detected_inconsistencies = 0;
	num_case_splits = 0;
	after_branching = true;
	num_deductions = 0;
	max_decision_level = 0;
	num_generated_conflict_clauses_smaller_than_10 = 0;
	num_generated_conflict_clauses_smaller_than_30 = 0;
	num_generated_conflict_clauses_smaller_than_50 = 0;
	num_generated_conflict_clauses_smaller_than_100 = 0;
	num_generated_conflict_clauses_larger_than_100 = 0;
	searching = false;
	tick_counter = 0;
	curr_randomness = randomness_level;
	to_process.reset();
	num_subsumed_clauses = 0;
	num_implication_graph_optimization_assignments = 0;
	implication_graph_optimization_time = 0.0;
	cleanup_period_counter = 0;
	num_lookahead_optimization_assignments = 0;
	lookahead_optimization_time = 0.0;
	heurisitic_update_time = 0.0;
	relevant_atoms.clear();
}

void LPSolver::setup_ics(unsigned int f_idx)
{
	queue<unsigned int> & to_setup = tmp_queue;
	to_setup.reset();
	
	to_setup.push(f_idx);
	
	while (!to_setup.is_empty()) {
		unsigned int curr_idx = to_setup.pop();
		const LPFormula * formula = formula_manager->get_formula(curr_idx);
		switch (formula->get_kind()) {
		case LP_OR: {
			unsigned int n = formula->get_num_arguments();
			for (unsigned int i = 0; i < n; i++) {
				LPFormulaId child = formula->get_argument(i);
					to_setup.push(abs(child));
			}
			break;
		}
		case LP_IFF: {
			LPFormulaId lhs = formula->get_iff_lhs();
			LPFormulaId rhs = formula->get_iff_rhs();
			to_setup.push(abs(lhs));
			to_setup.push(abs(rhs));
			break;
		}
		case LP_ITE: {
			LPFormulaId c = formula->get_cond();
			LPFormulaId t = formula->get_then();
			LPFormulaId e = formula->get_else();
			to_setup.push(abs(c));
			to_setup.push(abs(t));
			to_setup.push(abs(e));
			break;
		}
		case LP_EXISTS:
			feature_not_implemented_yet();
		case LP_EQ:
			ics_interface.set_formula(curr_idx);
			break;
		case LP_PROPOSITION:
			// ignore
			break;
		default:
			assert(false);
		}
	}

	ics_interface.compute_associated_formulas_info();
}

bool LPSolver::is_satisfiable(LPFormulaId f)
{
	SOLVER_TRACE(ctrace<< "Starting solver..." << endl;);
 	clock_t start = clock();
	root_formula_id = f;
	initialize_solver();

	if (verbose)
		cerr << "  preprocessing...\n";

	if (polarity_optimization)
		apply_polarity_optimization(f);

	if (verbose)
		cerr << "    internalizing formula...\n";
	
	internalize_formula(absolute(f));

	if (!assign_formula(f, 1, 0)) {
		// an inconsistency was detected...
		clock_t end = clock();
		preproc_time = ((double) (end - start)) / CLOCKS_PER_SEC;
		return false; /* formula is not satisfiable */
	}
	
	if (implication_graph_optimization) {
		if (!apply_implication_graph_optimization()) {
			// an inconsistency was detected...
			clock_t end = clock();
			preproc_time = ((double) (end - start)) / CLOCKS_PER_SEC;
			return false; /* formula is not satisfiable */
		}
	}

	if (verbose)
		cerr << "    propagating extended constraints...\n";

	if (propagate_rich_constraints() != 0) {
		// an inconsistency was detected...
		clock_t end = clock();
		preproc_time = ((double) (end - start)) / CLOCKS_PER_SEC;
		return false; /* formula is not satisfiable */
	}

	if (lookahead_optimization) {
		if (!apply_lookahead_optimization()) {
			// an inconsistency was detected...
			clock_t end = clock();
			preproc_time = ((double) (end - start)) / CLOCKS_PER_SEC;
			return false; /* formula is not satisfiable */
		}
	}

	if (verbose)
		cerr << "    preparing depth first search...\n";

	internalize_formula_as_clauses(f);

	if (branching_mode == LP_ACTIVATION_MODE) {
		assert(active != NULL);
		activate(absolute(f));
	}

	setup_ics(absolute(f));
	clock_t end = clock();
	preproc_time = ((double) (end - start)) / CLOCKS_PER_SEC;


	if (verbose)
		cerr << "  branching (depth first search)...\n  ";
	
	constraint_propagation_time = 0.0; // I only want the constraint_propagation_time spent in the depth first search
	num_backtracks = 0; // I only want to record the number of backtracks in the depth first search

	start = clock();
	// begin to branch...
	searching = true;
	bool result = false;
	while(true) {
		if (num_case_splits % 0xFFF == 0) {
			update_heuristics_info();
		}
		
		assert(check_invariant());
		LPBranchReturnType branch_result = branch();
		after_branching = true;
		if (branch_result == LP_BRANCH_FINISHED) {
			// the following code prints the assignments sent to ICS.
			ICS_TRACE(ctrace << "[ics] satisfiable assignment sequence: \n";
								cout << "\n[ics] satisfiable assignment sequence: \n";
								unsigned int n = processed_trail_stack.get_size();
								// ics_interface.reset_scratch_state();
								for (unsigned int i = 0; i < n; i++) {
									unsigned int idx = processed_trail_stack.get(i);
									if (formula_manager->get_formula(idx)->is_eq()) {
										int val = get_formula_value(idx);
										ctrace << "  ";
										formula_manager->dump_formula(ctrace, idx);
										ctrace << " = " << val << endl;
										// send information in ICS format to stdout
										cout << "assert "; cout.flush();
										ics_interface.dump_ics_formula(val == 1 ? idx : -idx);
										cout << ".\n";
										// if (!ics_interface.assert_formula_in_scratch_state(val == 1 ? idx : -idx)) {
										// cerr << "\n\nError validating ICS counter-example\n";
										//	exit(-1);
										// }											
									}
								}
								cout << "[ics] -------------------------\n";
								);
			result = true;
			goto end_branching_loop;
		}
		else if (branch_result == LP_BRANCH_INCONSISTENCY) {
			SOLVER_TRACE(ctrace << "inconsistency detected...\n";);
			after_branching = false; // conflict resolution...
			if (!conflict_resolution()) {
				result = false;
				goto end_branching_loop;
			}
		}
		while (propagate_clausal_constraints() != 0) {
			after_branching = false;
			if (!conflict_resolution()) {
				result = false;
				goto end_branching_loop;
			}
		}
		// check if we are missing constraint propagations...
		// this is not necessary for completeness and soundness,
		// and it should not be true if we are using multiple trail stacks,
		// or some other kind of non-chronological backtracking...
		assert(check_after_constraint_propagation());

		assert(check_completeness_argument());

		assert(check_clause_db_invariant());
		
		assert(check_invariant());
	}
 end_branching_loop:
	if (verbose)
		cerr << endl;
	end = clock();
	solver_time = ((double) (end - start)) / CLOCKS_PER_SEC;
	
	return result;
}

bool LPSolver::prove(LPFormulaId f) 
{
	return !is_satisfiable(-f); /* f is a theorem iff -f is unsatisfiable */ 
}

void LPSolver::compute_relevant_atoms(LPFormulaId f)
{
	queue<unsigned int> & to_dump = tmp_queue;
	to_dump.reset();

	assert(check_marks());

	queue_push(to_dump, absolute(f));
	while (!to_dump.is_empty()) {
		unsigned int curr_idx = to_dump.pop();
		// process children...
		const LPFormula * formula = formula_manager->get_formula(curr_idx);
		switch (formula->get_kind()) {
		case LP_OR: {
			int val = get_formula_value(curr_idx);
			unsigned int n = formula->get_num_arguments();
			if (val == 1) {
				// add only the first positive (true) child
				for (unsigned int i = 0; i < n; i++) {
					LPFormulaId child = formula->get_argument(i);
					if (get_formula_value(child) == 1) {
							queue_push(to_dump, absolute(child));
							break;
					}
				}
			}
			else {
				assert(val == -1);
				for (unsigned int i = 0; i < n; i++)
					queue_push(to_dump, absolute(formula->get_argument(i)));
			}
			break;
		}
		case LP_IFF: 
			queue_push(to_dump, absolute(formula->get_iff_lhs()));
			queue_push(to_dump, absolute(formula->get_iff_rhs()));
			break;
		case LP_ITE: {
			LPFormulaId c = formula->get_cond();
			int c_val = get_formula_value(c);
			if (c_val == 1) { 
				queue_push(to_dump, absolute(c));
				queue_push(to_dump, absolute(formula->get_then()));
			}
			else if (c_val == -1) {
				queue_push(to_dump, absolute(c));
				queue_push(to_dump, absolute(formula->get_else()));
			}
			break;
		}
		case LP_EXISTS:
			feature_not_implemented_yet();
		case LP_EQ:
			// perform an extra check for non-propositional constraints
			assert(IMPLY(formula_manager->get_formula(curr_idx)->is_eq(), processed[curr_idx])); 
		case LP_PROPOSITION: {
			relevant_atoms.insert(curr_idx);
			break; 
		}
		default:
			assert(false);
		}
	}
	remove_marks_from_tmp_queue();
}

void LPSolver::dump_atomic_formula_values(ostream & target, LPFormulaId f)
{
	queue<unsigned int> & to_dump = tmp_queue;
	to_dump.reset();

	assert(check_marks());

	queue_push(to_dump, absolute(f));
	while (!to_dump.is_empty()) {
		unsigned int curr_idx = to_dump.pop();
		// process children...
		const LPFormula * formula = formula_manager->get_formula(curr_idx);
		switch (formula->get_kind()) {
		case LP_OR: {
			int val = get_formula_value(curr_idx);
			unsigned int n = formula->get_num_arguments();
			if (val == 1) {
				// add only the first positive (true) child
				for (unsigned int i = 0; i < n; i++) {
					LPFormulaId child = formula->get_argument(i);
					if (get_formula_value(child) == 1) {
							queue_push(to_dump, absolute(child));
							break;
					}
				}
			}
			else {
				assert(val == -1);
				for (unsigned int i = 0; i < n; i++)
					queue_push(to_dump, absolute(formula->get_argument(i)));
			}
			break;
		}
		case LP_IFF: 
			queue_push(to_dump, absolute(formula->get_iff_lhs()));
			queue_push(to_dump, absolute(formula->get_iff_rhs()));
			break;
		case LP_ITE: {
			LPFormulaId c = formula->get_cond();
			int c_val = get_formula_value(c);
			if (c_val == 1) { 
				queue_push(to_dump, absolute(c));
				queue_push(to_dump, absolute(formula->get_then()));
			}
			else if (c_val == -1) {
				queue_push(to_dump, absolute(c));
				queue_push(to_dump, absolute(formula->get_else()));
			}
			break;
		}
		case LP_EXISTS:
			feature_not_implemented_yet();
		case LP_EQ:
			// perform an extra check for non-propositional constraints
			assert(IMPLY(formula_manager->get_formula(curr_idx)->is_eq(), processed[curr_idx])); 
		case LP_PROPOSITION: {
			formula_manager->dump_formula(target, curr_idx);
			target << " |-> ";
			char * str_value = NULL;
			if (branching_mode == LP_ACTIVATION_MODE) {
				assert(active != NULL);
				if (!active[curr_idx])
					str_value = "(irrelevant)";
			}
			if (str_value == NULL) {
				switch(get_formula_value(curr_idx)) {
				case 1: str_value = "true"; break;
				case -1: str_value = "false"; break;
				case 0: str_value = "(irrelevant)"; break;
				default:
					assert(false);
				}
			}
			target << str_value << endl;
			break; 
		}
		default:
			assert(false);
		}
	}
	remove_marks_from_tmp_queue();
}

void LPSolver::dump_clauses(ostream & target)
{
	unsigned int n = clauses.get_size();
	for (unsigned int i = 1; i < n; i++) {
		LPClause & c = clauses.get(i);
		unsigned int size = c.get_size();
		target << "  ";
		for (unsigned int j = 0; j < size; j++) {
			formula_manager->dump_formula(target, c.get_literal(j));
			target << " ";
		}
		target << endl;
	}
}

void LPSolver::dump_clause(ostream & target, LPClause & clause)
{
	unsigned int n = clause.get_size();
	for (unsigned int i = 0; i < n; i++) {
		LPFormulaId id = clause.get_literal(i);
		int val = get_formula_value(id);
		if (clause.get_watch1_literal() == id)
			target << "1:";
		else if (clause.get_watch2_literal() == id)
			target << "2:";
		target << id << ":" << val << ":" << internal_formulas[absolute(id)].decision_level << " ";
	}
	if (clause.was_deleted()) 
		target << "(deleted) ";
}

void LPSolver::allocate_activation_mode_arrays() {
	active = new bool[capacity_formulas];
	memset(active, 0, sizeof(bool) * capacity_formulas);
}

void LPSolver::deallocate_activation_mode_arrays() {
	if (active != NULL) {
		delete[] active;
		active = NULL;
	}
}

unsigned int LPSolver::get_clause_num_unknowns(unsigned int clause_idx)
{ 
	LPClause * clause = clauses.get_ptr(clause_idx);
	unsigned size = clause->get_size();
	int can_first = 0;
	unsigned int num_unknowns = 0;
	for (unsigned int lit_idx = 0; lit_idx < size; lit_idx++) {
		LPFormulaId lit = clause->get_literal(lit_idx);
		int lit_value = get_formula_value(lit);
		if (lit_value == 0) {
			if (num_unknowns == 0) {
				num_unknowns = 1;
				can_first = canonical_formula(lit);
			}
			else {
				// we are computing the number of different unknowns
				if (canonical_formula(lit) != can_first)
					num_unknowns++;
			}
		}
	}
	return num_unknowns;
}

bool LPSolver::is_clause_satisfiable(unsigned int clause_idx)
{
	// Important:
	// is_clause_satisfiable(clause_idx) is NOT equivalent to !is_clause_unsatisfiable(clause_idx)
	//
	assert(clause_idx >= 1); // the clause id 0 is not used.
	LPClause & clause = clauses.get(clause_idx);
	assert(clause.check_invariant());
	unsigned int n = clause.get_size();
	for (unsigned int i = 0; i < n; i++) {
		int literal_id = clause.get_literal(i);
		if (get_formula_value(literal_id) == 1) 
			return true;
	}
	return false;
}

bool LPSolver::is_clause_unsatisfiable(unsigned int clause_idx)
{
	assert(clause_idx >= 1); // the clause id 0 is not used.
	LPClause & clause = clauses.get(clause_idx);
	assert(clause.check_invariant());
	unsigned int n = clause.get_size();
	for (unsigned int i = 0; i < n; i++) {
		int literal_id = clause.get_literal(i);
		if (get_formula_value(literal_id) != -1) // if there is a literal that is not false, then the clause can be satisfied...
			return false;
	}
	return true;
}

unsigned int LPSolver::get_max_decision_level_of(int antecedent)
{
	unsigned int curr_max = 0;
	if (antecedent < 0) {
		unsigned int clause_idx = -antecedent;
		assert(clause_idx > 0);
		LPClause & clause = clauses.get(clause_idx);
		unsigned int n = clause.get_size();
		for(unsigned int i = 0; i < n; i++) {
			unsigned int var_idx = absolute(clause.get_literal(i));
			if (internal_formulas[var_idx].decision_level > curr_max)
				curr_max = internal_formulas[var_idx].decision_level;
		}
	}
	else {
		assert(antecedent > 0);
		unsigned int formula_idx = antecedent;
		const LPFormula * formula = formula_manager->get_formula(formula_idx);
		curr_max = internal_formulas[formula_idx].decision_level;
		switch(formula->get_kind()) {
		case LP_OR: {
			unsigned int n = formula->get_num_arguments();
			for (unsigned int i = 0; i < n; i++) {
				unsigned int var_idx = absolute(formula->get_argument(i));
				if (internal_formulas[var_idx].decision_level > curr_max)
					curr_max = internal_formulas[var_idx].decision_level;
			}
			break;
		}
		case LP_IFF: {
			unsigned int lhs_dec_level = internal_formulas[absolute(formula->get_iff_lhs())].decision_level;
			unsigned int rhs_dec_level = internal_formulas[absolute(formula->get_iff_rhs())].decision_level;
			curr_max = max(curr_max, max(lhs_dec_level, rhs_dec_level));
			break;
		}
		case LP_ITE: {
			unsigned int c_dec_level = internal_formulas[absolute(formula->get_cond())].decision_level;
			unsigned int t_dec_level = internal_formulas[absolute(formula->get_then())].decision_level;
			unsigned int e_dec_level = internal_formulas[absolute(formula->get_else())].decision_level;
			curr_max = max(curr_max, max(c_dec_level, max(t_dec_level, e_dec_level)));
			break;
		}
		case LP_EXISTS:
			feature_not_implemented_yet();
			break;
		case LP_EQ:
		case LP_PROPOSITION:
		default:
			assert(false);
		}
	}
	return curr_max;
}

bool LPSolver::check_counter_example_main_loop(LPFormulaId f_id)
{
	unsigned int f_idx = absolute(f_id);
	if (!marks[f_idx])
		marks[f_idx] = true;
	else 
		return true;


	const LPFormula * formula = formula_manager->get_formula(f_idx);
	LPFormulaId val = get_formula_value(f_idx);

	assert(val == LPTrueId || val == LPFalseId);

	switch(formula->get_kind()) {
	case LP_OR: {
		unsigned int n = formula->get_num_arguments();
		if (val == LPFalseId) { // is false...
			for (unsigned int i = 0; i < n; i++) {
				LPFormulaId child_id = formula->get_argument(i);
				if (get_formula_value(child_id) != -1) {
					assert(false);
					return false;
				}
				if (!check_counter_example_main_loop(child_id)) 
					return false;
			}
			return true;
		} else {
			assert(val == LPTrueId);
			for (unsigned int i = 0; i < n; i++) {
				LPFormulaId child_id = formula->get_argument(i);
				if (get_formula_value(child_id) == LPTrueId) 
					return check_counter_example_main_loop(child_id);
			}
			assert(false);
			return false;
		}
	}
	case LP_IFF: {
		LPFormulaId lhs = formula->get_iff_lhs();
		LPFormulaId rhs = formula->get_iff_rhs();
		if (get_formula_value(lhs) == 0 || get_formula_value(rhs) == 0) {
			assert(false);
			return false;
		}
		if (val == LPTrueId && get_formula_value(lhs) != get_formula_value(rhs)) {
			assert(false);
			return false;
		}
		if (val == LPFalseId && get_formula_value(lhs) != -get_formula_value(rhs)) {
			assert(false);
			return false;
		}
		return check_counter_example_main_loop(lhs) && check_counter_example_main_loop(rhs);
	}
	case LP_ITE: {
		LPFormulaId c = formula->get_cond();
		LPFormulaId t = formula->get_then();
		LPFormulaId e = formula->get_else();
		if (get_formula_value(c) == LPTrueId) {
			if (get_formula_value(t) == -val) {
				assert(false);
				return false;
			}
			return check_counter_example_main_loop(c) && check_counter_example_main_loop(t);
		}
		else {
			assert(get_formula_value(c) == LPFalseId);
			if (get_formula_value(e) == -val) {
				assert(false);
				return false;
			}
			return check_counter_example_main_loop(c) && check_counter_example_main_loop(e);
		}
	}
	case LP_EXISTS:
		feature_not_implemented_yet();
		break;
	case LP_PROPOSITION:
	case LP_EQ:
		// do nothing...
		break;
	default:
		assert(false);
	}
	
	return true;
}

bool LPSolver::check_assignment(LPFormulaId root_id) {
	assert(check_marks());

	if (get_formula_value(root_id) != 1) {
		assert(false);
		return false;
	}
	
	bool result = check_counter_example_main_loop(root_id);
	reset_marks();
	return result;
}

class LPIdxClausalOccursLt {
	unsigned int * num_clausal_positive_occurrences;
	unsigned int * num_clausal_negative_occurrences;
public:
	LPIdxClausalOccursLt(LPSolver * s) { 
		num_clausal_positive_occurrences = s->get_num_positive_clausal_occurrences_array();
		num_clausal_negative_occurrences = s->get_num_negative_clausal_occurrences_array();
	}
	bool operator()(unsigned int idx1, unsigned int idx2) {
		unsigned int val1 = max(num_clausal_positive_occurrences[idx1], num_clausal_negative_occurrences[idx1]);
		unsigned int val2 = max(num_clausal_positive_occurrences[idx2], num_clausal_negative_occurrences[idx2]);
		return val1 > val2;
	}
};

unsigned int LPSolver::update_heuristics_in_activation_mode(LPFormulaId f)
{
 	clock_t start = clock();
	unsigned int f_idx = absolute(f);
	const LPFormula * formula = formula_manager->get_formula(f_idx);
	switch(formula->get_kind()) {
	case LP_OR: {
		unsigned int n = formula->get_num_arguments();
		unsigned int m = 0;
		for (unsigned int i = 0; i < n; i++) {
			unsigned int curr = update_heuristics_in_activation_mode(formula->get_argument(i));
			if (curr > m)
				m = curr;
		}
		if (m > num_clausal_positive_occurrences[f_idx])
			num_clausal_positive_occurrences[f_idx] += m;
		if (m > num_clausal_negative_occurrences[f_idx])
			num_clausal_negative_occurrences[f_idx] += m;
		break;
	}
	case LP_IFF: {
		unsigned int lhs = update_heuristics_in_activation_mode(formula->get_iff_lhs());
		unsigned int rhs = update_heuristics_in_activation_mode(formula->get_iff_rhs());
		unsigned int m = max(lhs, rhs);
		if (m > num_clausal_positive_occurrences[f_idx])
			num_clausal_positive_occurrences[f_idx] += m;
		if (m > num_clausal_negative_occurrences[f_idx])
			num_clausal_negative_occurrences[f_idx] += m;
		break;
	}
	case LP_ITE: {
		unsigned int c = update_heuristics_in_activation_mode(formula->get_cond());
		unsigned int t = update_heuristics_in_activation_mode(formula->get_then());
		unsigned int e = update_heuristics_in_activation_mode(formula->get_else());
		unsigned int m = max(c, max(t,e));
		if (m > num_clausal_positive_occurrences[f_idx])
			num_clausal_positive_occurrences[f_idx] += m;
		if (m > num_clausal_negative_occurrences[f_idx])
			num_clausal_negative_occurrences[f_idx] += m;
		break;
	}
	case LP_PROPOSITION:
	case LP_EQ:
		break;
	case LP_EXISTS:
		feature_not_implemented_yet();
		break;
	default:
		assert(false);
	}
	clock_t end = clock();
	heurisitic_update_time += ((double) (end - start)) / CLOCKS_PER_SEC;
	return max(num_clausal_positive_occurrences[f_idx], num_clausal_negative_occurrences[f_idx]);
}


void LPSolver::update_heuristics_info()
{
 	clock_t start = clock();
	if (branching_mode == LP_ACTIVATION_MODE) {
		if (experimental_heuristics)
			update_heuristics_in_activation_mode(root_formula_id);
		else
			return; // We only have experimental heuristics for the activation mode.
	}

	LPIdxClausalOccursLt lt(this);
	sortFcn<unsigned int, LPIdxClausalOccursLt> sort(lt);
	
	sort(to_check.get_contents(), to_check.get_size());

	// adjust counters...
	for (unsigned int i = 0; i < get_internal_formulas_array_size(); i++) {
		num_clausal_positive_occurrences[i] /= 2;
		num_clausal_negative_occurrences[i] /= 2;
	}

	// update to_check_inverse
	memset(to_check_inverse.get_contents(), 0, sizeof(unsigned int) * to_check_inverse.get_size());
	unsigned int n = to_check.get_size();
	for (unsigned int i = 0; i < n; i++)
		to_check_inverse.set_x(to_check.get(i), i);
	
	next_to_check = 0;
	
	DBG_CODE(unsigned int n = to_check.get_size();
					 for (unsigned int i = 0; i < n; i++) {
						 assert(to_check_inverse.get(to_check.get(i)) == i);
					 });
	clock_t end = clock();
	heurisitic_update_time += ((double) (end - start)) / CLOCKS_PER_SEC;
}

bool LPSolver::process_complex_constraints()
{
	if (branching_mode == LP_ACTIVATION_MODE)
		return true; // this function is only used in the LP_ASSIGNMENT_MODE and LP_VAR_ASSIGNMENT_MODE

	queue<unsigned int> & to_do = tmp_queue;
	to_do.reset();
	assert(check_marks());

	queue_push(to_do, absolute(root_formula_id));

	while(!to_do.is_empty()) {
		unsigned int idx = to_do.pop();
		if (!is_unknown(idx)) {
			const LPFormula * formula = formula_manager->get_formula(idx);
			switch (formula->get_kind()) {
			case LP_OR: {
				int value = get_formula_value(idx);
				unsigned int n = formula->get_num_arguments();
				if (value == 1) {
					// add only the first positive (true) child
					for (unsigned int i = 0; i < n; i++) {
						LPFormulaId child = formula->get_argument(i);
						if (get_formula_value(child) == 1) {
							queue_push(to_do, absolute(child));
							break;
						}
					}
				}
				else {
					assert(value == -1);
					// the formula is false, so all children should be added to the queue
					for (unsigned int i = 0; i < n; i++) {
						assert(get_formula_value(formula->get_argument(i)) == -1);
						queue_push(to_do, absolute(formula->get_argument(i)));
					}
				}
				break;
			}
			case LP_IFF: 
				queue_push(to_do, absolute(formula->get_iff_lhs()));
				queue_push(to_do, absolute(formula->get_iff_rhs()));
				break;
			case LP_ITE: {
				LPFormulaId c = formula->get_cond();
				int c_val = get_formula_value(c);
				if (c_val == 1) { 
					queue_push(to_do, absolute(c));
					queue_push(to_do, absolute(formula->get_then()));
				}
				else if (c_val == -1) {
					queue_push(to_do, absolute(c));
					queue_push(to_do, absolute(formula->get_else()));
				}
				break;
			}
			case LP_EXISTS:
				feature_not_implemented_yet();
				break;
			case LP_PROPOSITION:
				break; // do nothing
			case LP_EQ: {
				if (!processed[idx]) {
					set_as_processed(idx);
					int value = get_formula_value(idx);
					bool ics_result;
					SOLVER_TRACE(ctrace << "  sending assignment to ICS [";
											 formula_manager->dump_formula(ctrace, idx);
											 ctrace << "] = " << value << endl;);
					if (value == 1) 
						ics_result = ics_interface.assert_formula(idx);
					else {
						assert(value == -1);
						ics_result = ics_interface.assert_formula(-idx);
					}
					if (!ics_result) {
						SOLVER_TRACE(ctrace << "  conflict detected in ICS assertion [";
												 formula_manager->dump_formula(ctrace, idx);
												 ctrace << "] = " << value << endl;);
						remove_marks_from_tmp_queue();
						explain_ics_inconsistency(idx);
						return false;
					}
				}
				break;
			}
			default:
				assert(false);
			}
		}
	}

	num_new_non_propositional_assignments = 0; // reset the number of complex assignments

	remove_marks_from_tmp_queue();
	return true;
}
