/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 11, 2002: Created.
***/

#ifndef LPSOLVER_H
#define LPSOLVER_H

#include"LPFormula.h"
#include"growable-vector.h"
#include"fix-size-allocator.h"
#include"queue.h"
#include"ics_interface.h"
#include"lisp.h"

#define TICK_FREQUENCY 2000

class LPClause;

class LPSolver;

// Clauses are only used inside the solver...
// they are mainly used to implement "learning", that is, to represent
// conflict clauses...
//
class LPClause
{
protected:
	LPFormulaId * literals; // contents of the clause...
	LPFormulaId * watch1;
	LPFormulaId * watch2;
	unsigned int size;
	bool main_clause; // main clause, cannot be deleted
	bool deleted;
	friend ostream & operator << ( ostream & target, LPClause & c);
	friend LPSolver;
public:
	LPClause() { /* do nothing */ }
	bool check_invariant() const;
	void init(LPFormulaId * l, unsigned int s, bool m, LPFormulaId * w1, LPFormulaId * w2) {
		literals = l;
		size = s;
		deleted = false;
		main_clause = m;
		watch1 = w1;
		watch2 = w2;
		assert(check_invariant());
	}
 	bool was_deleted() const { return deleted; }
 	void set_as_deleted() { deleted = true; }
	LPFormulaId * get_literals() const { return literals; }
	LPFormulaId get_literal(unsigned int idx) { assert(idx < size); return literals[idx]; }
	unsigned int get_size() const { return size; }
	LPFormulaId * get_watch1() const { return watch1; }
	LPFormulaId * get_watch2() const { return watch2; }
	LPFormulaId get_watch1_literal() const { return *watch1; }
	LPFormulaId get_watch2_literal() const { return *watch2; }
	void set_watch1(LPFormulaId * w1) { 
		watch1 = w1; 
		assert(check_invariant());
	}
	void set_watch2(LPFormulaId * w2) { 
		watch2 = w2; 
		assert(check_invariant());
	}
	bool is_main_clause() const { return main_clause; }
	bool subsumes(LPClause * c) const;
};

inline ostream & operator << (ostream & target, LPClause & c) {
	for (unsigned int i = 0; i < c.get_size(); i ++) 
		target << c.get_literal(i) << " ";
 	if (c.was_deleted()) 
		target << "(deleted)"; 
	return target;
}

typedef growable_vector<unsigned int, 4, false> LPClauseVector;
typedef growable_vector<unsigned int, 4, false> LPFormulaIdxVector;

struct LPInternalFormulaInfo {
	unsigned int decision_level;

	int antecedent; // positive values are LPFormulaId, and negative values are clauses

	LPFormulaIdxVector positive_occurrences;
	LPFormulaIdxVector negative_occurrences;

	LPClauseVector clausal_positive_occurrences;
	LPClauseVector clausal_negative_occurrences;

	// the following two vectors are only used to implement subsumed clauses removal...
	LPClauseVector * all_clausal_positive_occurrences;
	LPClauseVector * all_clausal_negative_occurrences;

	LPFormulaId next; // members of the equivalence class...
	// I can use a pointer instead a list, since a formula can be only in one equivalence class at each time.

	// Q. Why we need the "next" field?
	// A. To propagate constraints. For instance, suppose that an equivalence class
	//    contains the formulas {3, 5, -4}, and 3 is the root, then if 3 is assigned
	//    to TRUE, occurrences of 5 and 4 are also affected...
	// 
};

typedef growable_vector<unsigned int> LPTrailStack; 
typedef growable_vector<unsigned int> LPActivationTrailStack; 
typedef growable_vector<unsigned int> LPProcessedTrailStack;

struct LPBacktrackPoint {
	unsigned int trail_stack_top;
	unsigned int activation_trail_stack_top;
	unsigned int processed_trail_stack_top;
	unsigned int next_to_check;
};

typedef growable_vector<LPBacktrackPoint> LPBacktrackPointStack;

typedef enum { LP_ACTIVATION_MODE, LP_ASSIGNMENT_MODE, LP_VAR_ASSIGNMENT_MODE } LPSolverBranchingMode;

typedef enum { LP_ONLY_DECIDED_CR, LP_FIRST_UIP_CR } LPSolverConflictResolutionMode;

// The branch method can return three possible values:
// - LP_BRANCH_OK: branch was performed with success.
// - LP_BRANCH_FINISHED: no more branches... the problem was solved.
// - LP_BRANCH_INCONSISTENCY: a decision procedure inconsistency was detected.
typedef enum { LP_BRANCH_OK, LP_BRANCH_FINISHED, LP_BRANCH_INCONSISTENCY } LPBranchReturnType;

typedef enum{UNKNOWN_FORCE = 0, POS_FORCE, NEG_FORCE, POS_NEG_FORCE} LPFormulaPolarityForce;

typedef lisp_list<unsigned int> LPIdList;
typedef enum{white = 0, gray, black} DFSColor;
		
class LPSolver {
	unsigned int num_formulas; // number of formulas 
	unsigned int capacity_formulas; 

	LPInternalFormulaInfo * internal_formulas; // holds extra information about internalized formulas
	int * assignments; // the value of each internalized LPFormula
	// Meaning of the assignments field
	// assignments[idx] = 0    // the value of the formula idx is UNKNOWN
	// assignments[idx] = 1    // the value of the formula idx is TRUE
  // assignments[idx] = -1   // the value of the formula idx is FALSE
  // assignments[idx] = v    // v > 1, the value of the formula idx == value of the formula v
  // assignments[idx] = -v   // v < -1, the value of the formula idx == NOT value of the formula v

	bool * active; // activation array
	bool * processed; 
	bool * marks;
	int * added_literals; // can be improved in the future ... I only need three states {NOT_ADDED, POSITIVE_ADDED, NEGATIVE_ADDED}.

	bool searching; // true if the solver is in a depth search mode

	unsigned int decision_level;

	LPFormulaId root_formula_id;

	LPFormulaManager * formula_manager;

	LPTrailStack trail_stack;
	LPActivationTrailStack activation_trail_stack;
	LPProcessedTrailStack processed_trail_stack;

	LPBacktrackPointStack backtrack_point_stack;

	LPFormulaId * literals;
	unsigned int num_literals;
	unsigned int capacity_literals;
	
	growable_vector<LPClause> clauses;
	growable_vector<LPClause *> free_clauses;
	unsigned int clause_relevance;
	bool remove_subsumed_clauses;
	unsigned int num_deleted_clauses;
	unsigned int cleanup_period; 
	unsigned int cleanup_period_counter; 
	//
	// heuristic support variables
	//
	unsigned int * num_clausal_positive_occurrences;
	unsigned int * num_clausal_negative_occurrences;
public:
	unsigned int get_num_clausal_occurrences(LPFormulaId formula_id) const {
		return formula_id < 0 ? num_clausal_negative_occurrences[-formula_id] : num_clausal_positive_occurrences[formula_id];
	}
	unsigned int get_num_positive_clausal_occurrences(unsigned int idx) const {
		return num_clausal_positive_occurrences[idx];
	}
	unsigned int * get_num_positive_clausal_occurrences_array() const {
		return num_clausal_positive_occurrences;
	}
	unsigned int get_num_negative_clausal_occurrences(unsigned int idx) const {
		return num_clausal_negative_occurrences[idx];
	}
	unsigned int * get_num_negative_clausal_occurrences_array() const {
		return num_clausal_negative_occurrences;
	}
	unsigned int get_num_occurrences(unsigned int idx) const {
		return internal_formulas[idx].positive_occurrences.get_size() +
			internal_formulas[idx].negative_occurrences.get_size();
	}
private:
	//
	// end of heuristic support variables
	//

	// bool * in_to_process_queue;
	queue<unsigned int> to_process;

	unsigned int get_internal_formulas_array_size() { return num_formulas + 1; }

	unsigned int get_trail_stack_top() const { return trail_stack.get_size(); }
	unsigned int get_activation_trail_stack_top() const { return activation_trail_stack.get_size(); }
	unsigned int get_processed_trail_stack_top() const { return processed_trail_stack.get_size(); }

	unsigned int get_backtrack_point_stack_top() const { return backtrack_point_stack.get_size(); }


	// the to_check variables are used to decide when the process is complete.
	// 1) if branching_mode == LP_ACTIVATION_MODE
	//       the vector to_check contains all formulas, but some of them are mark as active.
	//       Only active (and not yet processed) formulas processed.
	//       A formula is processed in the following way:
	//         a) an unassigned active formula should be assigned. (formula is not marked as processed).
	//         b) an active OR(...) == False: all children should be activated. (formula is marked as processed).
	//         b) an active OR(...) == True,  one of the children should be assigned to true and activated. (formula is marked as processed).
	//         c) an active IFF(lhs, rhs) == True or False:  lhs and rhs should be activated. (formula is marked as processed).
	//         d) an active ITE(c,t,e) == True (False), 
	//               if c is unassigned, then choose a value for "c"
	//               Now, if c == true, then activate "c" and "t".
	//                    otherwise activate "c" and "e".
	//               (formula is marked as processed).
	//         e) a active proposition: do nothing.
	//         f) an atomic formula: send it to ICS. (formula is marked as processed).
	// 2) if branching_mode == LP_ASSIGNMENT_MODE
	//      the vector to_check contains all formulas... that is, the process is complete when all 
	//      formulas are assigned to a boolean variable.
	// 3) if branching_mode == LP_VAR_ASSIGNMENT_MODE
	//      the vector to_check contains all atomic formulas... that is, the process is complete when all 
	//      atomic formulas are assigned to a boolean variable.
	//      
	growable_vector<unsigned int> to_check;
	growable_vector<unsigned int> to_check_inverse;
	unsigned int next_to_check;

	// randomness_level support
	int randomness_level;
	int curr_randomness;
	// end of randomness_level support
	
	bool verbose;
	int tick_counter; // used to print the tick mark
	void update_tick() {
		if (verbose) {
			tick_counter++;
			if (tick_counter > TICK_FREQUENCY) {
				cout << ".";
				cout.flush();
				tick_counter = 0;
			}
		}
	}

	LPSolverBranchingMode branching_mode;
	LPSolverConflictResolutionMode conflict_resolution_mode;
	unsigned int npc_threshold;
	unsigned int num_new_non_propositional_assignments;
	bool polarity_optimization;
	bool implication_graph_optimization;
	bool lookahead_optimization;
	unsigned int lookahead_relevance;
	bool experimental_heuristics;

	ICSInterface ics_interface;

	double preproc_time;
	double solver_time;
	double ics_explain_time;
	double branching_time;
	double conflict_resolution_time;
	double constraint_propagation_time;
	double polarity_optimization_time;
	double implication_graph_optimization_time;
	double lookahead_optimization_time;

	unsigned int num_subsumed_clauses;
	unsigned int num_backtracks;
	unsigned int num_ics_detected_inconsistencies;
	unsigned int num_case_splits;
	unsigned int num_deductions;
	unsigned int max_decision_level;
	unsigned int num_non_chronological_backtracking;
	unsigned int num_generated_conflict_clauses_smaller_than_10; // interval [1, 10]
	unsigned int num_generated_conflict_clauses_smaller_than_30; // interval (10, 30]
	unsigned int num_generated_conflict_clauses_smaller_than_50; // interval (30, 50]
	unsigned int num_generated_conflict_clauses_smaller_than_100; // interval (50, 100]
	unsigned int num_generated_conflict_clauses_larger_than_100; // interval (100, ..)
	unsigned int num_implication_graph_optimization_assignments;
	unsigned int num_lookahead_optimization_assignments; 

	// This flag is used to implement a little optimization.
	// It is used to decide how we should compute the decision_level of an assigned formula
	// All variables assigned after a branching has a decision level equals to the current one.
	// After a conflict resolution, we may have a "virtual" decision level, since conflict clauses can be generated.
	// In this last case, the decision level is equal to the maximum decision level in the object that is implying the assigned formula.
	bool after_branching;

	// Conflict point communication 
	growable_vector<int> new_conflict_point;

	//
	// Invariant checking
	// 
	bool check_assignment_invariant() const;
	bool check_equivalence_class_invariant();
	bool check_equivalence_class_root_invariant();
	bool check_activation_invariant();
	bool check_clause_db_invariant();
	bool check_clause_literals_invariant(LPClause * clause);
	bool check_invariant();
	bool check_completeness_argument();
	bool check_after_constraint_propagation();
	bool check_clausal_occurrences(LPClauseVector & v, LPFormulaId formula_id);
	bool check_marks();
	//
	// End of Invariant checking methods
	//

	void reset_marks() {
		memset(marks, 0, sizeof(bool) * get_internal_formulas_array_size());
	}
	
	void report_unassignment(unsigned int f_idx) {
		if (branching_mode == LP_ASSIGNMENT_MODE ||
				(branching_mode == LP_VAR_ASSIGNMENT_MODE && formula_manager->get_formula(f_idx)->is_atomic())) {
			unsigned int pos = to_check_inverse.get(f_idx);
			if (pos < next_to_check) {
				SOLVER_TRACE(ctrace << "adjusting next_to_check, old = " << next_to_check << ", new = " << pos;);
				next_to_check = pos;
			}
		}
	}

	void undo_assignment(unsigned int f_idx) {
		assert(assignments[f_idx] != 0);
		SOLVER_TRACE(ctrace << "  unassigning " << f_idx << " [";
								 formula_manager->dump_formula(ctrace, f_idx);
								 ctrace << "] = " << assignments[f_idx] << ", decision_level = " << internal_formulas[f_idx].decision_level << endl;);
		int val = assignments[f_idx];
		LPInternalFormulaInfo & info = internal_formulas[f_idx];
		info.decision_level = 0;
		info.antecedent = 0;
		assignments[f_idx] = 0; // unknown
		
		if (val != 1 && val != -1) {
			LPInternalFormulaInfo & info2 = internal_formulas[absolute(val)];
			LPFormulaId tmp = info.next;
			if (val > 0) {
				info.next = info2.next;
				info2.next = tmp;
			}
			else {
				info.next = - info2.next;
				info2.next = tmp;
			}
		}

		report_unassignment(f_idx);
		assert(check_invariant());
	}

	void expand_formula_capacity();

	void reset_formula(unsigned int idx) {
		assignments[idx] = 0; // unknown
		LPInternalFormulaInfo & info = internal_formulas[idx];
		info.decision_level = 0;
		info.antecedent = 0;
		info.next = idx;
		info.all_clausal_positive_occurrences = NULL;
		info.all_clausal_negative_occurrences = NULL;
	}

	void add_new_formulas(unsigned int num_formulas);

	bool is_clause_satisfiable(unsigned int clause_idx);
	unsigned int get_clause_num_unknowns(unsigned int clause_idx);
	bool is_clause_unsatisfiable(unsigned int clause_idx);

	unsigned int get_max_decision_level_of(int antecedent);

	//
	// Literals memory management
	//
	void remove_irrelevant_clauses();
	void expand_literals();
	void compact_literals();
	void request_more_literals(unsigned int num_lits);
	//
	// End of Literals memory management
	//

	void inc_decision_level() {
		decision_level++;
		if (decision_level > max_decision_level)
			max_decision_level = decision_level;
	}

	void activate(unsigned int idx)
	{
		assert(branching_mode == LP_ACTIVATION_MODE);
		if (!active[idx]) {
			SOLVER_TRACE(ctrace << "  activating: " << idx << " ";
									 formula_manager->dump_formula(ctrace, idx);
									 ctrace << endl;);
			activation_trail_stack.push(idx);
			active[idx] = true;
			if (to_check_inverse.get(idx) < next_to_check && !processed[idx]) {
				// Formulas are not trees, but DAGs... so, a child can be located before the parent in the to_check array!
				SOLVER_TRACE(ctrace << "  adjusting next_to_check: " << next_to_check << ", new value = " << to_check_inverse.get(idx) << endl;);
				// Q.: Why we need a -1 here?
        // A.: Suppose that we are processing idx = 160 (located at next_to_check 32) , and we execute the following steps:
        //       - activates idx = 89 (located next_to_check 20), and updates next_to_check to 19 (20 - 1)
        //       - finishes and increments next_to_check (next_to_check = 20)
				next_to_check = to_check_inverse.get(idx) - 1;
			}
		}
	}

	void set_as_processed(unsigned int idx) 
	{
		if (!processed[idx]) {
			SOLVER_TRACE(ctrace << "  processed: " << idx << endl;);
			processed_trail_stack.push(idx);
			processed[idx] = true;
		}
	}

	void set_occurrence(LPFormulaId child, unsigned int parent_idx) {
		if (child < 0)
			internal_formulas[-child].negative_occurrences.push(parent_idx);
		else
			internal_formulas[child].positive_occurrences.push(parent_idx);
	}

	//
	// Temporary (Scratch) arrays/queues
	//
	queue<unsigned int> tmp_queue;
	queue<int> tmp_int_queue;
		
	void remove_marks_from_tmp_queue() {
		// reset marks
		unsigned int n = tmp_queue.get_num_elems_inserted();
		for (unsigned int i = 0; i < n; i++) {
			assert(tmp_queue.low_level_get(i) <= num_formulas);
			marks[tmp_queue.low_level_get(i)] = false;
		}
		tmp_queue.reset();
	}

	void remove_marks_from_tmp_int_queue() {
		// reset marks
		unsigned int n = tmp_int_queue.get_num_elems_inserted();
		for (unsigned int i = 0; i < n; i++) {
			assert(absolute(tmp_int_queue.low_level_get(i)) <= num_formulas);
			marks[absolute(tmp_int_queue.low_level_get(i))] = false;
		}
		tmp_int_queue.reset();
	}

	// the following methods should be used with queues that have the following property:
	//   marks[val] == true <=> q contains val
	void queue_push(queue<unsigned int> & q, unsigned int val) {
		if (!marks[val]) {
			marks[val] = true;
			q.push(val);
		}
	}
	void queue_push(queue<int> & q, int val) {
		unsigned int idx = absolute(val);
		if (!marks[idx]) {
			marks[idx] = true;
			q.push(val);
		}
	}
	//
	// End of Temporary (Scratch) arrays
	//

public:
	LPSolver(LPFormulaManager * f_manager, unsigned int initial_capacity = INITIAL_NUMBER_OF_FORMULAS,
					 unsigned int initial_number_of_clauses = INITIAL_NUMBER_OF_CLAUSES,
					 unsigned int initial_number_of_literals = INITIAL_NUMBER_OF_LITERALS);

	~LPSolver();

	void delete_dynamic_arrays();

	LPFormulaId get_eq_class_root(LPFormulaId f) {
		assert(f != 0);
		bool sign = f < 0;
		int v = assignments[sign ? -f : f];
		while (v < -1 || v > 1) {
			f = sign ? -v : v;
			sign = f < 0;
			v = assignments[sign ? -f : f];
		}
		return f;
	}

	LPFormulaId canonical_formula(int * aux_assignments, LPFormulaId f) {
		assert(f != 0);
		bool sign = f < 0;
		int v = aux_assignments[sign ? -f : f];
		if (v == 0)
			return f;
		while (v < -1 || v > 1) {
			f = sign ? -v : v;
			sign = f < 0;
			v = aux_assignments[sign ? -f : f];
		}
		if (v == 0)
			return f;
		assert(v == 1 || v == -1);
		return sign ? -v : v;
	}

	LPFormulaId canonical_formula(LPFormulaId f) { return canonical_formula(assignments, f); }

	// return 0, 1 or -1 (Unknown, True, or False)
	LPFormulaId get_formula_value(LPFormulaId f) {
		bool sign = f < 0;
		while (f < -1 || f > 1) {
			f = assignments[sign ? -f : f];
			f = sign ? -f : f;
			sign = f < 0;
		}
		return f;
	}

	// fast get_formula_value (Unknown, True, or False)
	// the following function only works for variables that are roots of equivalence classes
	// return 0, 1 or -1
	LPFormulaId fast_get_formula_value(LPFormulaId f) {
		assert(assignments[absolute(f)] >= -1 && assignments[absolute(f)] <= 1);
		return f < 0 ? -assignments[-f] : assignments[f];
	}

	// return true if the boolean value of the formula is unknown
	bool is_unknown(LPFormulaId f) { return get_formula_value(f) == 0; }

	bool fast_is_unknow(LPFormulaId f) { return fast_get_formula_value(f) == 0; }

	unsigned int get_curr_decision_level(int antecedent) {
		if (antecedent == 0 || after_branching)
			return decision_level;
		else
			return get_max_decision_level_of(antecedent);
	}
	bool assign_formula(LPFormulaId target, LPFormulaId value, int antecedent);
	void assign_formula(LPFormulaId target, LPFormulaId value);
	// Important:
	// the following method can only be used to assert the root of an equivalence class to "true/false"
	bool LPSolver::fast_assign_formula(LPFormulaId target, LPFormulaId value, int antecedent);
	void add_equivalence_class_to_queue(unsigned int formula_idx);

	void set_backtrack_point();
	bool backtrack(unsigned int num_levels = 1);

	LPClause * add_clause(LPFormulaId * lits, unsigned int num_lits, unsigned int watch1, unsigned int watch2, bool is_main_clause = true);
	// The following method is a wrapper for the previous one.
	// It automatically sets the watch pointers.
	// If the clause contains a literal assigned to true, then it returns NULL, since the clause is trivially true.
	LPClause * add_clause(LPFormulaId * lits, unsigned int num_lits);
	LPClause * add_clause(growable_vector<LPFormulaId> & lits) { return add_clause(lits.get_contents(), lits.get_size()); }

	//
	// Constraint Propagation
	// 
	void to_process_queue_push(unsigned int f_idx) {
		// if (!in_to_process_queue[f_idx]) {
		// in_to_process_queue[f_idx] = true;
		to_process.push(f_idx);
		// }
	}
 	unsigned to_process_queue_pop() {
		unsigned int f_idx = to_process.pop();
 		// in_to_process_queue[f_idx] = false;
 		return f_idx;
	}
protected:
 	int propagate_rich_constraints();
 	int propagate_logic_constraint_downward(const LPFormula * formula);
 	int propagate_or_constraint_downward(const LPFormula * formula);
 	int propagate_iff_constraint_downward(const LPFormula * formula);
 	int propagate_ite_constraint_downward(const LPFormula * formula);
 	int propagate_exists_constraint_downward(const LPFormula * formula);
 	int propagate_logic_constraint_upward(unsigned int formula_idx, unsigned int occurrence_idx, bool negative_occurrence);
 	int propagate_or_constraint_upward(unsigned int formula_idx, const LPFormula * parent, bool negative_occurrence);
 	int propagate_iff_constraint_upward(unsigned int formula_idx, const LPFormula * parent);
 	int propagate_ite_constraint_upward(unsigned int formula_idx, const LPFormula * parent);
 	int propagate_exists_constraint_upward(unsigned int formula_idx, const LPFormula * parent);
	int propagate_clausal_constraints();
	int propagate_clausal_constraints(unsigned int formula_idx);
	int propagate_clausal_constraints(unsigned int formula_idx, LPClauseVector & v);
	//
	// End of Constraint Propagation Methods
	//

	//
	// Polarity based optimization
	//
	bool apply_polarity_optimization_main_loop(LPFormulaId formula_id, LPFormulaPolarityForce force, LPFormulaPolarityForce * forces);
	void apply_polarity_optimization(LPFormulaId root);
	//
	// End of polarity based optimization
	//

	//
	// Implication graph optimization
	//
	unsigned int literal2node_id(LPFormulaId literal);
	LPFormulaId node_id2literal(unsigned int node_id);
	LPIdList ** compute_implication_graph();
	void dump_implication_graph(ostream & target, LPIdList ** implication_graph);
	LPIdList ** transpose_implication_graph(LPIdList ** implication_graph);
	bool apply_implication_graph_optimization();
	//
	// End Of implication graph optimization
	//

	bool process_complex_constraints();

	//
	// Lookahead optimization
	//
	void compute_lookahead_intersection(int * assignments1, int * assignments2, int * result_assignments);
	void consume_intersection(int * intersection);
	bool apply_lookahead_optimization();
	//
	// End of lookahead optimization
	//

	//
	// Configuration
	//
protected:
	void allocate_activation_mode_arrays();
	void deallocate_activation_mode_arrays();
public:
	void set_branching_mode(LPSolverBranchingMode new_mode) { 
		branching_mode = new_mode; 
		if (branching_mode == LP_ACTIVATION_MODE) {
			if (active == NULL)  
				allocate_activation_mode_arrays();
		}
		else 
			deallocate_activation_mode_arrays();
	}
	void set_npc_threshold(unsigned int n) { npc_threshold = n; }
	void set_clause_relevance(unsigned int r) { clause_relevance = r; }
	void set_cleanup_period(unsigned int p) { cleanup_period = p; }
	void set_conflict_resolution_mode (LPSolverConflictResolutionMode new_mode) { conflict_resolution_mode = new_mode; }
	void set_verbosity(bool flag) { verbose = flag; }
	void enable_subsumed_clauses_removal(bool flag = true) { remove_subsumed_clauses = flag; }
	void disable_subsumed_clauses_removal() { enable_subsumed_clauses_removal(false); }
	void set_randomness_level(int level) { randomness_level = level; }
	void enable_polarity_optimization(bool flag = true) { polarity_optimization = flag; }
	void disable_polarity_optimization() { enable_polarity_optimization(false); }
	void enable_implication_graph_optimization(bool flag = true) { implication_graph_optimization = flag; }
	void disable_implication_graph_optimization() { enable_implication_graph_optimization(false); }
	void enable_lookahead_optimization(bool flag = true) { lookahead_optimization = flag; }
	void disable_lookahead_optimization() { enable_lookahead_optimization(false); }
	void set_lookahead_relevance(unsigned int r) { lookahead_relevance = r; }
	void enable_experimental_heuristics(bool flag = true) { experimental_heuristics = flag; }
	void disable_experimental_heuristics() { enable_experimental_heuristics(false); }
	//
	// End of Configuration Methods
	//

	//
	// Branching
	// 
protected:
	LPBranchReturnType branch_activation_mode();
	LPBranchReturnType branch_default();
public:
	LPBranchReturnType branch() {
		clock_t start = clock();
		assert(check_invariant());
		LPBranchReturnType result;
		if (branching_mode == LP_ACTIVATION_MODE)
			result = branch_activation_mode();
		else
			result = branch_default();
		if (result == LP_BRANCH_OK)
			num_case_splits++;
		clock_t end = clock();
		branching_time += ((double) (end - start)) / CLOCKS_PER_SEC;
		return result;
	}
	//
	// End of Branching Methods
	// 

	
	// 
	// Internalization
	//
	void internalize_formula(unsigned int idx);
	void internalize_formula_as_clauses(int f_id);
	//
	// End of Internalization methods
	//

	unsigned int update_heuristics_in_activation_mode(LPFormulaId f);
	void update_heuristics_info();

	void initialize_solver();
	void setup_ics(unsigned int f_idx);
	bool is_satisfiable(LPFormulaId f);
	bool prove(LPFormulaId f);

	void process_cleanup_routines() {
		cleanup_period_counter++;
		if (cleanup_period_counter > cleanup_period) {
			if (verbose) {
				cout << "#";
				cout.flush();
			}
			cleanup_period_counter = 0;
			remove_irrelevant_clauses();
		}
	}

	//
	// Conflict Resolution
	//
	bool conflict_resolution() {
		clock_t start = clock();
		SOLVER_EXPLAIN_TRACE(ctrace << "  conflict point:\n";
												 ctrace << "    " << new_conflict_point << endl;
												 ctrace << "    ";
												 for (unsigned int i = 0; i < new_conflict_point.get_size(); i++) {
													 formula_manager->dump_formula(ctrace, new_conflict_point.get(i));
													 ctrace << "  ";
												 }
												 ctrace << endl;);
		update_tick();
		process_cleanup_routines();
		bool result;
		if (conflict_resolution_mode == LP_ONLY_DECIDED_CR)
			result = conflict_resolution_only_decided();
		else
			result = conflict_resolution_first_uip();
		clock_t end = clock();
		conflict_resolution_time += ((double) (end - start)) / CLOCKS_PER_SEC;
		return result;
	}
	bool conflict_resolution_first_uip();
	bool conflict_resolution_only_decided();
	void set_conflict_point(int conflict_point);
	void explain_ics_inconsistency(unsigned int f_idx);
	//
	// End of Conflict Resolution Methods
	//

	//
	// Statistics
	//
	double get_preprocessing_time() const { return preproc_time; }
	double get_ics_time() const { return ics_interface.get_time(); }
	double get_solver_time() const { return solver_time; }
	double get_ics_explain_time() const { return ics_explain_time; }
	// all calls to ICS are perfomed inside the branching function
	double get_branching_time() const { double v = branching_time - get_ics_time(); return v < 0 ? 0 : v;} 
	double get_conflict_resolution_time() const { return conflict_resolution_time; }
	double get_constraint_propagation_time() const { return constraint_propagation_time; }
	double get_polarity_optimization_time() const { return polarity_optimization_time; }
	double get_implication_graph_optimization_time() const { return implication_graph_optimization_time; }
	double get_lookahead_optimization_time() const { return lookahead_optimization_time; }
	unsigned int get_num_backtracks() const { return num_backtracks; }
	unsigned int get_num_ics_detected_inconsistencies() const { return num_ics_detected_inconsistencies; }
	unsigned int get_num_case_splits() const { return num_case_splits; }
	unsigned int get_num_ics_calls() const { return ics_interface.get_num_calls(); }
	unsigned int get_num_deductions() const { return num_deductions; }
	unsigned int get_max_decision_level() const { return max_decision_level; }
	unsigned int get_num_non_chronological_backtracking() const { return num_non_chronological_backtracking; }
	unsigned int get_num_generated_conflict_clauses_smaller_than_10() const { return num_generated_conflict_clauses_smaller_than_10; }
	unsigned int get_num_generated_conflict_clauses_smaller_than_30() const { return num_generated_conflict_clauses_smaller_than_30; }
	unsigned int get_num_generated_conflict_clauses_smaller_than_50() const { return num_generated_conflict_clauses_smaller_than_50; }
	unsigned int get_num_generated_conflict_clauses_smaller_than_100() const { return num_generated_conflict_clauses_smaller_than_100; }
	unsigned int get_num_generated_conflict_clauses_larger_than_100() const { return num_generated_conflict_clauses_larger_than_100; }
	unsigned int get_num_subsumed_clauses() const { return num_subsumed_clauses; }
	unsigned int get_num_deleted_clauses() const { return num_deleted_clauses; }
	unsigned int get_num_implication_graph_optimization_assignments () const { return num_implication_graph_optimization_assignments; }
	unsigned int get_num_lookahead_optimization_assignments() const { return num_lookahead_optimization_assignments; }
	//
	// End of Statistics methods
	//

	//
	// Result validation
	//
protected:
	bool check_counter_example_main_loop(LPFormulaId f_id);
public:
	bool check_counter_example(LPFormulaId root_id);
	//
	// End of Result validation methods
	//


	//
	// Pretty printing
	//
	void dump_internal_info(ostream & target);
	void dump_to_check_vector(ostream & target);
	void dump_atomic_formula_values(ostream & target, LPFormulaId f);
	void dump_ics_state() { ics_interface.dump_current_state(); }
	void dump_clauses(ostream & target);
	void dump_clause(ostream & target, LPClause & clause);
	//
	// End of Pretty printing methods
	//

};

extern LPSolver * sat_solver;

inline ostream & operator << (ostream & target, LPSolver & s) {
	s.dump_internal_info(target);
	return target;
}

#endif /* LPSOLVER_H */
