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
extern int ICS_EXPLAIN_NUM_REFINEMENTS;
double SAT_associated_formulas_time;

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
// 		DBG_CODE(cout << ">>>>> mk_or\n";
// 						 cout << "arguments: ";
// 						 for(int i = 0; i < num_args; i++) 
// 						 cout << args[i] << " ";
// 						 cout << endl;);
		int r = sat_formula_manager->create_or(num_args, args);
//		DBG_CODE(cout << "<<<<< result of mk_or = " << r << endl;);
		return r;
	}
	LPFormulaId sat_mk_and(unsigned int num_args, LPFormulaId * args) {
// 		DBG_CODE(cout << ">>>>> mk_and\n";
// 						 cout << "arguments: ";
// 						 for(int i = 0; i < num_args; i++) 
// 						 cout << args[i] << " ";
// 						 cout << endl;);
		int r = sat_formula_manager->create_and(num_args, args);
// 		DBG_CODE(cout << "<<<<< result of sat_mk_and = " << r << endl;);
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
// 		DBG_CODE(cout << ">>>>> mk_not\n";
// 						 cout << "argument: " << f << endl;);
		int r = sat_formula_manager->create_not(f);
// 		DBG_CODE(cout << "<<<<< result of mk_not = " << r << endl;);
		return r;
	}
	LPFormulaId sat_mk_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e) {
		return sat_formula_manager->create_ite(c,t,e);
	}
	LPFormulaId sat_mk_atom(int a, int not_a) {
		// icsat_atom_pp(a);
		// cout.flush(); cout << endl;
		// icsat_atom_pp(not_a);
		// cout.flush(); cout << endl;
		// cout<<"-----------------\n";
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
		if (sat_solver->is_relevant_atom(absolute(f)))
			return sat_solver->get_formula_value(f);
		else
			return 0;
	}

	void sat_print_statistics() {
		if (sat_solver == NULL)
			return;
		cerr << "Statistics...\n";
		cerr << "  number of formulas: " << sat_formula_manager->get_num_formulas() << endl;
		cerr << "  total solver time: " << SAT_total_solver_time << " secs\n";
		cerr << "    preprocessor time: " << sat_solver->get_preprocessing_time() << " secs\n";
		cerr << "      formula association time: " << SAT_associated_formulas_time << " secs\n";
		if (SAT_polarity_optimization)
			cerr << "      polarity optimization time: " << sat_solver->get_polarity_optimization_time() << " secs\n";
		if (SAT_implication_graph_optimization)
			cerr << "      implication graph optimization time: " << sat_solver->get_implication_graph_optimization_time() << " secs\n";
		cerr << "    (main loop) solver time: " << sat_solver->get_solver_time() << " secs\n";
		cerr << "      branching/activation time: " << sat_solver->get_branching_time() << " secs\n";
		cerr << "      constraint propagation time: " << sat_solver->get_constraint_propagation_time() << " secs\n";
		cerr << "      conflict resolution time: " << sat_solver->get_conflict_resolution_time() << " secs\n";
		cerr << "      heuristic statistics update time: " << sat_solver->get_heurisitic_update_time() << " secs\n";
		cerr << "      ICS time: " << sat_solver->get_ics_time() << " secs\n";
		cerr << "      ICS explain function time: " << sat_solver->get_ics_explain_time() << " secs\n";
		cerr << "  number of backtracks: " << sat_solver->get_num_backtracks() << endl;
		cerr << "    number of non-chronological backtrackings: " << sat_solver->get_num_non_chronological_backtracking() << endl;
		cerr << "  number of inconsistencies detected by ICS: " << sat_solver->get_num_ics_detected_inconsistencies() << endl;
		cerr << "  number of case splits: " << sat_solver->get_num_case_splits() << endl;
		cerr << "  number of deductions: " << sat_solver->get_num_deductions() << endl;
		cerr << "  maximum decision level: " << sat_solver->get_max_decision_level() << endl;
		cerr << "  number of calls to ICS: " << sat_solver->get_num_ics_calls() << endl;
		cerr << "  conflict clauses statistics:\n";
		cerr << "    [1,  10]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_10() << endl;
		cerr << "    (10, 30]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_30() << endl;
		cerr << "    (30, 50]   = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_50() << endl;
		cerr << "    (50, 100]  = " << sat_solver->get_num_generated_conflict_clauses_smaller_than_100() << endl;
		cerr << "    (100, inf) = " << sat_solver->get_num_generated_conflict_clauses_larger_than_100() << endl;
		cerr << "  number of deleted clauses: " << sat_solver->get_num_deleted_clauses() << endl;
		if (SAT_remove_subsumed_clauses)
			cerr << "  number of subsumed clauses: " << sat_solver->get_num_subsumed_clauses() << endl;
		if (SAT_implication_graph_optimization)
			cerr << "  number of assignments produced by implication graph optimization: " 
					 << sat_solver->get_num_implication_graph_optimization_assignments() << endl;
		sat_formula_manager->dump_mem_info();
	}

  int ics_sat(LPFormulaId root_id, bool use_proof_objs) {

//     DBG_CODE(cout << "working!!! root_id = " << root_id << "\n";
// 						 sat_formula_manager->dump_formula(cout, root_id););
		// The following code is a hack to make the connection between ICS and SAT a little bit easier
// 		DBG_CODE(
// 						 cout << "BEFORE NORMALIZATION:\n";
// 						 sat_formula_manager->dump_formula(cout, root_id);
// 						 cout << endl << endl;
// 						 );
		root_id = sat_formula_manager->normalize_formula(root_id);
// 		DBG_CODE(
// 						 cout << "AFTER NORMALIZATION:\n";
// 						 sat_formula_manager->dump_formula(cout, root_id);
// 						 cout << endl << endl;
// 						 );

		if (sat_solver != NULL) 
			delete sat_solver;
		sat_solver = new LPSolver(sat_formula_manager, use_proof_objs);

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
		if (result)
			sat_solver->compute_relevant_atoms(root_id);
		clock_t end = clock();
		SAT_total_solver_time = ((double) (end - start)) / CLOCKS_PER_SEC;		
		// cout << "result = " << result << endl;
		// sat_print_statistics();
// 		DBG_CODE(cout << "result = " << result << endl;);
		if (result && SAT_validate_counter_example) {
			if (SAT_verbose)
				cerr << "  validating counter example... "; 
			if (sat_solver->check_assignment(root_id)) {
				if (SAT_verbose)
					cerr << "done\n";
			}
			else {
				if (SAT_verbose)
					cerr << "FAILED\n";
				cerr << "BUG DETECTED: failed to validade SAT assignment\n";
				exit(-1);
			}
		}
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
	void sat_set_num_refinements(int n) {
		ICS_EXPLAIN_NUM_REFINEMENTS = n;
	}
}
