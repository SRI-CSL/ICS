/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Dec 11, 2002: Created.
***/

#include "LPSolver.h"

LPFormulaManager * sat_formula_manager = NULL;
LPSolver * sat_solver = NULL;

double SAT_total_solver_time; 
bool SAT_verbose = false;
bool SAT_remove_subsumed_clauses = true;
bool SAT_validate_counter_example = false;
bool SAT_polarity_optimization = false;
bool SAT_implication_graph_optimization = false;
int  SAT_clause_relevance = DEFAULT_CLAUSE_RELEVANCE;
int  SAT_cleanup_period = DEFAULT_CLEANUP_PERIOD;

extern "C" {
	void sat_initialize() {
		sat_formula_manager = new LPFormulaManager();
	}
	void sat_finalize() {
		if (sat_formula_manager != NULL) {
			delete sat_formula_manager;
			sat_formula_manager = NULL;
		}
		if (sat_solver != NULL) {
			delete sat_solver;
			sat_solver = NULL;
		}
	}
	LPFormulaId sat_mk_true() {
		return LPTrueId;
	}
	LPFormulaId sat_mk_false() {
		return LPFalseId;
	}
	LPFormulaId sat_mk_or(unsigned int num_args, LPFormulaId * args) {
		DBG_CODE(cout << ">>>>> mk_or\n";
						 cout << "arguments: ";
						 for(int i = 0; i < num_args; i++) 
						 cout << args[i] << " ";
						 cout << endl;);
		int r = sat_formula_manager->create_or(num_args, args);
		DBG_CODE(cout << "<<<<< result of mk_or = " << r << endl;);
		return r;
	}
	LPFormulaId sat_mk_and(unsigned int num_args, LPFormulaId * args) {
		DBG_CODE(cout << ">>>>> mk_and\n";
						 cout << "arguments: ";
						 for(int i = 0; i < num_args; i++) 
						 cout << args[i] << " ";
						 cout << endl;);
		int r = sat_formula_manager->create_and(num_args, args);
		DBG_CODE(cout << "<<<<< result of sat_mk_and = " << r << endl;);
		return r;
	}
	LPFormulaId sat_mk_iff(LPFormulaId lhs, LPFormulaId rhs) {
		return sat_formula_manager->create_iff(lhs, rhs);
	}
	LPFormulaId sat_mk_implies(LPFormulaId lhs, LPFormulaId rhs) {
		return sat_formula_manager->create_implies(lhs, rhs);
	}
	LPFormulaId sat_mk_xor(LPFormulaId lhs, LPFormulaId rhs) {
		return sat_formula_manager->create_xor(lhs, rhs);
	}
	LPFormulaId sat_mk_not(LPFormulaId f) {
		DBG_CODE(cout << ">>>>> mk_not\n";
						 cout << "argument: " << f << endl;);
		int r = sat_formula_manager->create_not(f);
		DBG_CODE(cout << "<<<<< result of mk_not = " << r << endl;);
		return r;
	}
	LPFormulaId sat_mk_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e) {
		return sat_formula_manager->create_ite(c,t,e);
	}
	LPFormulaId sat_mk_atom(int a, int not_a) {
		return sat_formula_manager->create_atom(a, not_a);
	}
	LPFormulaId sat_mk_var(char * var) {
		return sat_formula_manager->create_proposition(var);
	}
	int sat_is_true(LPFormulaId f) {
		return f == LPTrueId;
	}
	int sat_is_false(LPFormulaId f) {
		return f == LPFalseId;
	}
	int sat_is_not(LPFormulaId f) {
		return f < 0;
	}
	int sat_is_or(LPFormulaId f) {
		return f > 0 && sat_formula_manager->get_formula(f)->is_or();
	}
	int sat_is_iff(LPFormulaId f) {
		return f > 0 && sat_formula_manager->get_formula(f)->is_iff();
	}
	int sat_is_ite(LPFormulaId f) {
		return f > 0 && sat_formula_manager->get_formula(f)->is_ite();
	}
	int sat_is_var(LPFormulaId f) {
		return f > 0 && sat_formula_manager->get_formula(f)->is_proposition();
	}
	int sat_is_atom(LPFormulaId f) {
		return f > 0 && sat_formula_manager->get_formula(f)->is_atomic();
	}
	LPFormulaId sat_d_not(LPFormulaId f) {
		return -f;
	}
	char * sat_d_var(LPFormulaId f) {
		return (char *) sat_formula_manager->get_formula(f)->get_propositional_variable_name();
	}
	value sat_d_atom(LPFormulaId f) {
		return sat_formula_manager->get_formula(f)->get_atom();
	}
	int sat_num_arguments(LPFormulaId f) {
		if (f < 0)
			return 1;
		else
			return sat_formula_manager->get_formula(f)->get_num_arguments();
	}
	LPFormulaId sat_get_argument(LPFormulaId f, int idx) {
		if (f < 0)
			return -f;
		else
			return sat_formula_manager->get_formula(f)->get_argument(idx);
	}

	// return 0, 1 or -1 (Unknown, True, or False)
	int sat_get_assignment(LPFormulaId f) {
		return sat_solver->get_formula_value(f);
	}

	void sat_print_statistics() {
		if (sat_solver == NULL)
			return;
		cout << "Statistics...\n";
		cout << "  number of formulas: " << sat_formula_manager->get_num_formulas() << endl;
		cout << "  total solver time: " << SAT_total_solver_time << " secs\n";
		cout << "    preprocessor time: " << sat_solver->get_preprocessing_time() << " secs\n";
		if (SAT_polarity_optimization)
			cout << "      polarity optimization time: " << sat_solver->get_polarity_optimization_time() << " secs\n";
		if (SAT_implication_graph_optimization)
			cout << "      implication graph optimization time: " << sat_solver->get_implication_graph_optimization_time() << " secs\n";
		cout << "    (main loop) solver time: " << sat_solver->get_solver_time() << " secs\n";
		cout << "      branching/activation time: " << sat_solver->get_branching_time() << " secs\n";
		cout << "      constraint propagation time: " << sat_solver->get_constraint_propagation_time() << " secs\n";
		cout << "      conflict resolution time: " << sat_solver->get_conflict_resolution_time() << " secs\n";
		cout << "      ICS time: " << sat_solver->get_ics_time() << " secs\n";
		cout << "      ICS explain function time: " << sat_solver->get_ics_explain_time() << " secs\n";
		cout << "  number of backtracks: " << sat_solver->get_num_backtracks() << endl;
		cout << "    number of non-chronological backtrackings: " << sat_solver->get_num_non_chronological_backtracking() << endl;
		cout << "  number of inconsistencies detected by ICS: " << sat_solver->get_num_ics_detected_inconsistencies() << endl;
		cout << "  number of case splits: " << sat_solver->get_num_case_splits() << endl;
		cout << "  number of deductions: " << sat_solver->get_num_deductions() << endl;
		cout << "  maximum decision level: " << sat_solver->get_max_decision_level() << endl;
		cout << "  number of calls to ICS: " << sat_solver->get_num_ics_calls() << endl;
		cout << "  conflict clauses statistics:\n";
		cout << "    [1,  10]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_10() << endl;
		cout << "    (10, 30]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_30() << endl;
		cout << "    (30, 50]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_50() << endl;
		cout << "    (50, 100]  = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_100() << endl;
		cout << "    (100, inf) = " << sat_solver->get_num_generated_conflict_clauses_larger_than_100() << endl;
		cout << "  number of deleted clauses: " << sat_solver->get_num_deleted_clauses() << endl;
		if (SAT_remove_subsumed_clauses)
			cout << "  number of subsumed clauses: " << sat_solver->get_num_subsumed_clauses() << endl;
		if (SAT_implication_graph_optimization)
			cout << "  number of assignments produced by implication graph optimization: " 
					 << sat_solver->get_num_implication_graph_optimization_assignments() << endl;
	}

  int ics_sat(LPFormulaId root_id) {
    cout << "working!!! root_id = " << root_id << "\n";
		sat_formula_manager->dump_formula(cout, root_id);
		if (sat_solver != NULL) 
			delete sat_solver;
		sat_solver = new LPSolver(sat_formula_manager);

		sat_solver->set_branching_mode(LP_ASSIGNMENT_MODE);
		sat_solver->set_conflict_resolution_mode(LP_FIRST_UIP_CR);
		sat_solver->set_verbosity(SAT_verbose);
		sat_solver->set_randomness_level(0);
		sat_solver->set_clause_relevance(SAT_clause_relevance);
		sat_solver->set_cleanup_period(SAT_cleanup_period);
		sat_solver->enable_subsumed_clauses_removal(SAT_remove_subsumed_clauses);
		sat_solver->enable_polarity_optimization(SAT_polarity_optimization);
		sat_solver->enable_implication_graph_optimization(SAT_implication_graph_optimization);
		sat_solver->enable_lookahead_optimization(false);
		sat_solver->set_lookahead_relevance(DEFAULT_LOOKAHEAD_RELEVANCE);
		sat_solver->enable_experimental_heuristics(false);
		sat_solver->set_npc_threshold(DEFAULT_NPC_THRESHOLD);

		clock_t start = clock();
		bool result = sat_solver->is_satisfiable(root_id);
		clock_t end = clock();
		SAT_total_solver_time = ((double) (end - start)) / CLOCKS_PER_SEC;		
		cout << "result = " << result << endl;
		return result;
  }

	void sat_set_verbose(bool flag) {
		SAT_verbose = flag;
	}
	void sat_set_remove_subsumed_clauses(bool flag) {
		SAT_remove_subsumed_clauses = flag;
	}
	void sat_set_validate_counter_example(bool flag) {
		SAT_validate_counter_example = flag;
	}
	void sat_set_polarity_optimization(bool flag) {
		SAT_polarity_optimization = flag;
	}
	void sat_set_implication_graph_optimization(bool flag) {
		SAT_implication_graph_optimization = flag;
	}
	void sat_set_clause_relevance(int r) {
		SAT_clause_relevance = r;
	}
	void sat_set_cleanup_period(int p) {
		SAT_cleanup_period = p;
	}

}
