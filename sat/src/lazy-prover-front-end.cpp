/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 6, 2002: Created.
***/

#include<iostream.h>
#include "ics_interface.h"
#include "LPFormula.h"
#include "LPSolver.h"

extern "C" {
  // ics_sat : LPFormulaId -> int
  int ics_sat(LPFormulaId root_id) {
    cout << "working!!! root_id = " << root_id << "\n";
		sat_formula_manager->dump_formula(cout, root_id);
		if (sat_solver != NULL) 
			delete sat_solver;
		sat_solver = new LPSolver(sat_formula_manager);

		sat_solver->set_branching_mode(LP_ASSIGNMENT_MODE);
		sat_solver->set_conflict_resolution_mode(LP_FIRST_UIP_CR);
		sat_solver->set_verbosity(true);
		sat_solver->set_randomness_level(0);
		sat_solver->set_clause_relevance(DEFAULT_CLAUSE_RELEVANCE);
		sat_solver->set_cleanup_period(DEFAULT_CLEANUP_PERIOD);
		sat_solver->enable_subsumed_clauses_removal(true);
		sat_solver->enable_polarity_optimization(false);
		sat_solver->enable_implication_graph_optimization(false);
		sat_solver->enable_lookahead_optimization(false);
		sat_solver->set_lookahead_relevance(DEFAULT_LOOKAHEAD_RELEVANCE);
		sat_solver->enable_experimental_heuristics(false);
		sat_solver->set_npc_threshold(DEFAULT_NPC_THRESHOLD);


		bool result = sat_solver->is_satisfiable(root_id);
		cout << "result = " << result << endl;
		return result;
  }
}

// #include<stdio.h>
// #if defined(LP_TRACE_MALLOC) || !defined(NDEBUG)
// #include<mcheck.h>
// #endif
// #include<signal.h>
// #include"msgs.h"
// #include"errormsg.h"
// #include"error.h"
// #include"parser.h"
// #include"time.h"
// #include"LPSolver.h"

// static void killhandler (int i) {
// 	cerr << "\naborting...\n";
// 	exit(1);
// }

// extern int ICS_EXPLAIN_NUM_REFINEMENTS;

// struct lazy_prover_arguments
// {
// 	char * args[1];
// 	const char * th_name;
// 	bool verbose;
// 	bool convert_to_cnf;
// 	bool remove_subsumed_clauses;
// 	bool statistics;
// 	bool validate_counter_example;
// 	bool polarity_optimization;
// 	bool implication_graph_optimization;
// 	bool lookahead_optimization;
// 	bool dont_print_counter_example;
// 	bool restart;
// 	bool benchmark_mode;
// 	bool experimental_heuristics;
// 	int  randomness_level;
// 	int  clause_relevance;
// 	int  cleanup_period;
// 	int  lookahead_relevance;
// 	int  restart_period;
// 	int  restart_period_increment;
// 	int  npc_threshold;
// 	LPSolverBranchingMode branching_mode;
// 	LPSolverConflictResolutionMode conflict_resolution_mode;
// };

// /*-------------------------------------------------------*
//  * Argument Parser                                       *
//  *-------------------------------------------------------*/
// const char *argp_program_version = "lazy-prover 0.1 (alpha)\n\nCopyright (C) 2001 SRI International.\n\nWritten by Leonardo de Moura";
// const char *argp_program_bug_address = "<demoura@csl.sri.com>";
// // Program small documentation
// static char lazy_prover_doc[] = "SRI Lazy-Prover (alpha version). Copyright (c) 2001 SRI International.";
// static char lazy_prover_args_doc[] = "file-name";

// #define PROCESS_OPTION(arg)																			\
// 		case 'g':																										\
// 			ICS_EXPLAIN_NUM_REFINEMENTS = atoi(arg);									\
// 			break;																										\
// 		case 'o':																										\
// 			arguments->npc_threshold = atoi(arg);											\
// 			break;																										\
// 		case 'n':																										\
// 			arguments->th_name = UString::uniqueString(arg);					\
// 			break;																										\
// 		case 'x':																										\
// 			arguments->experimental_heuristics = true;								\
// 			break;																										\
// 		case 'm':																										\
// 			arguments->benchmark_mode = true;													\
// 			break;																										\
// 		case 'R':																										\
// 			arguments->restart = true;																\
// 			break;																										\
// 		case 'E':																										\
// 			arguments->restart_period = atoi(arg);										\
// 			break;																										\
// 		case 'I':																										\
// 			arguments->restart_period_increment = atoi(arg);					\
// 			break;																										\
// 		case 'e':																										\
// 			arguments->lookahead_relevance = atoi(arg);								\
// 			break;																										\
// 		case 'L':																										\
// 			arguments->lookahead_optimization = true;									\
// 			break;																										\
// 		case 'P':																										\
// 			arguments->cleanup_period = atoi(arg);										\
// 			break;																										\
// 		case 'r':																										\
// 			arguments->clause_relevance = atoi(arg);									\
// 			break;																										\
// 		case 'V':																										\
// 			arguments->validate_counter_example = true;								\
// 			break;																										\
// 		case 'D':																										\
// 			arguments->dont_print_counter_example = true;							\
// 			break;																										\
// 		case 'p':																										\
// 			arguments->polarity_optimization = true;									\
// 			break;																										\
// 		case 'i':																										\
// 			arguments->implication_graph_optimization = true;					\
// 			break;																										\
// 		case 'b':																										\
// 			arguments->remove_subsumed_clauses = true;								\
// 			break;																										\
// 		case 's':																										\
// 			arguments->statistics = true;															\
// 			break;																										\
// 		case 'v':																										\
// 			arguments->verbose = true;																\
// 			break;																										\
// 		case 'l':																										\
// 			arguments->randomness_level = atoi(arg);									\
// 			break;																										\
// 		case 'c':																										\
// 			arguments->convert_to_cnf = true;													\
// 			break;																										\
// 		case 'a':																										\
// 			arguments->branching_mode = LP_ACTIVATION_MODE;						\
// 			break;																										\
// 		case 't':																										\
// 			arguments->branching_mode = LP_VAR_ASSIGNMENT_MODE;				\
// 			break;																										\
// 		case 'f':																										\
// 			arguments->branching_mode = LP_ASSIGNMENT_MODE;						\
// 			break;																										\
// 		case 'u':																										\
// 			arguments->conflict_resolution_mode = LP_FIRST_UIP_CR;		\
// 			break;																										\
// 		case 'd':																										\
// 			arguments->conflict_resolution_mode = LP_ONLY_DECIDED_CR;	\
// 			break;

// #ifndef LP_ARGP_AVAILABLE
// struct argp_option {
// 	char * long_name;
// 	char id;
// 	char * arg_info;
// 	char * extra;
// 	char * doc;
// 	int group_id;
// };
// #endif

// static struct argp_option lazy_prover_options[] = {
// 	{"verbose", 'v', 0, 0, "Produce verbose output", 0},
// 	{"name", 'n', "STR", 0, "Name of the theorem to be proved", 0},
// 	{"statistics", 's', 0, 0, "Show statistics", 0},
// 	{"benchmark-mode", 'm', 0, 0, "The solver only prints runtime information in a `machine' format", 0},
// 	{"validate", 'V', 0, 0, "Validate counter example", 0},
// 	{"dont-print-counter-example", 'D', 0, 0, "The prover will not print counter examples", 0},
// 	{"to-cnf", 'c', 0, 0, "Translate the formula to CNF (this option is only used for debugging purposes)", 1},
// 	{"active-strategy", 'a', 0, 0, "Active nodes search strategy, this mode analyses the relevant subset of the formula", 2},
// 	{"atom-strategy", 't', 0, 0, "Atom search strategy, tries to find a counter-example based on the boolean values of the atomic formulas", 2},
// 	{"formula-strategy", 'f', 0, 0, "Formula search strategy, tries to find a counter-example based on the boolean values of atomic formulas and sub-formulas (default)", 2},
// 	{"npc-threshold", 'o', "NUM", 0, "This parameter controls how non-propositional constraints are sent to ICS. It is only used in the \"atom\" and \"formula\" search strategy. A process is activated to select the relevant constraints that should be sent to ICS. This process is activated after new <NUM> non-propositional constraints are assigned. A huge number can be used to simulate an offline integration, that is, ICS is called only when a complete assignment is found (default = 10)", 2},
// 	{"first-uip", 'u', 0, 0, "First UIP (unique implication point) conflict resolution strategy", 3},
// 	{"only-decided", 'd', 0, 0, "`Only decision point formulas' conflict resolution strategy (default)", 3},
// 	{"refinements", 'g', "NUM", 0, "Number of refinements in the ICS conflict resolution procedure (default = 0)", 3},
// 	{"remove-subsumed-clauses", 'b', 0, 0, "Remove subsumed clauses", 4},
// 	{"randomness-level", 'l', "NUM", 0, "A certain amount of transient randomness to the prover is used to aid in the selection of a search path (default = 0), a greater value means more randomness", 4},
// 	{"polarity", 'p', 0, 0, "Apply the polarity based optimization. It is safe even for non-propositional formulas", 4},
// 	{"implication-graph", 'i', 0, 0, "Apply implication graph optimization, it is only worth when the input contains several binary clauses", 4},
// 	{"conflict-clause-relevance", 'r', "NUM", 0, "Conflict clause relevance (default = 100)", 4},
// 	{"cleanup-period", 'P', "NUM", 0, "Cleanup period (default = 4000)", 4},
// 	{"lookahead", 'L', 0, 0, "Apply lookahead optimization", 5},
// 	{"lookahead-relevance", 'e', "NUM", 0, "Restricts lookahead optimization to formulas with more than NUM occurrences (default = 5)", 5},
// 	{"restart", 'R', 0, 0, "Enable restarts", 6},
// 	{"restart-period", 'E', "NUM", 0, "Restart period (default = 20000)", 6},
// 	{"restart-period-increment", 'I', "NUM", 0, "Restart period increment (default = 1000)", 6},
// 	{"experimental-heuristics", 'x', 0, 0, "Enable experimental heuristics", 6},
// 	{ NULL, 0, 0, 0, NULL, 0}
// };

// #ifdef LP_ARGP_AVAILABLE

// static error_t lazy_prover_parse_opt (int key, char *arg, struct argp_state *state)
// {
// 	/* Get the INPUT argument from `argp_parse', which we
// 		 know is a pointer to our arguments structure. */
// 	lazy_prover_arguments *arguments = (lazy_prover_arguments *) state->input;
	
// 	switch (key) {
// 		PROCESS_OPTION(arg)
// 		case ARGP_KEY_ARG:
// 			if (state->arg_num >= 1)
// 				/* Too many arguments. */
// 				argp_usage (state);
// 			arguments->args[state->arg_num] = arg;
// 			break;
// 		case ARGP_KEY_END:
// 			if (state->arg_num < 1)
// 				/* Not enough arguments. */
// 				argp_usage (state);
// 			break;
// 		default:
// 			return ARGP_ERR_UNKNOWN;
// 		}
// 	return 0;
// }
// #endif


// void lazy_prover_set_default_values(lazy_prover_arguments & args)
// {
// 	args.verbose = false;
// 	args.th_name = NULL;
// 	args.statistics = false;
// 	args.benchmark_mode = false;
// 	args.validate_counter_example = false;
// 	args.dont_print_counter_example = false;
// 	args.convert_to_cnf = false;
// 	args.branching_mode = LP_ASSIGNMENT_MODE;
// 	args.conflict_resolution_mode = LP_FIRST_UIP_CR;
// 	args.remove_subsumed_clauses = true;
// 	args.randomness_level = 0;
// 	args.clause_relevance = DEFAULT_CLAUSE_RELEVANCE;
// 	args.cleanup_period = DEFAULT_CLEANUP_PERIOD;
// 	args.polarity_optimization = false;
// 	args.implication_graph_optimization = false;
// 	args.lookahead_optimization = false;
// 	args.lookahead_relevance = DEFAULT_LOOKAHEAD_RELEVANCE;
// 	args.restart = false;
// 	args.restart_period = DEFAULT_RESTART_PERIOD;
// 	args.restart_period_increment = DEFAULT_RESTART_PERIOD_INCREMENT;
// 	args.experimental_heuristics = false;
// 	args.npc_threshold = DEFAULT_NPC_THRESHOLD;
// }

// #ifdef LP_ARGP_AVAILABLE
// static struct argp lazy_prover_argp = {lazy_prover_options, lazy_prover_parse_opt, lazy_prover_args_doc, lazy_prover_doc, NULL, NULL, NULL};
// #else
// void argument_parse_help() {
// 	cout << lazy_prover_doc << endl;
// 	cout << "Usage: icsat [OPTION...] file-name\n\n";
// 	int id = 0;
// 	while (lazy_prover_options[id].long_name != NULL) {
// 		cout << "\t-" << lazy_prover_options[id].id;
// 		if (lazy_prover_options[id].arg_info != NULL)
// 			cout << " <" << lazy_prover_options[id].arg_info << "> ";
// 		else
// 			cout << "       ";
// 		cout << lazy_prover_options[id].doc << ".\n";
// 		id++;
// 	}
// 	cout << "\t" << "-h" << "       Give this help list.\n\n";
// 	cout << "Report bugs to " << argp_program_bug_address << ".\n";
// }

// void argument_parse(int argc, char ** argv, lazy_prover_arguments * arguments)
// {
// 	int c;
// 	extern char *optarg;
// 	extern int optind;
// 	while ((c = getopt(argc, argv, "vn:smVDcatfo:udg:bl:pir:P:Le:RE:I:xh")) != EOF) {
// 		switch(c) {
// 			PROCESS_OPTION(optarg)
// 			case 'h':
// 				argument_parse_help();
// 			  exit(1);
// 			case '?':
// 			  argument_parse_help();
// 				exit(1);
// 		}
// 	}
	
// 	cout << "optind = " << optind << ", argc = " << argc << endl;

// 	if (optind != argc - 1) {
// 		cerr << "Error: Invalid usage." << endl;
// 		argument_parse_help();
// 		exit(1);
// 	}
		
// 	arguments->args[0] = argv[argc-1];
// }
// #endif

// int main(int argc, char ** argv)
// {
// #ifndef NDEBUG
// 	mcheck(NULL);
// #endif

// #ifdef LP_TRACE_MALLOC
// 	// the following option does not produce useful information for C++ code. However, glibc developers claim this will be fixed
// 	// in glibc-2.3
// 	mtrace ();
// #endif 

// 	ICSInterface::initialize(argv);
//   signal(SIGINT, killhandler);

// 	lazy_prover_arguments arguments;
// 	lazy_prover_set_default_values(arguments);
// #ifdef LP_ARGP_AVAILABLE	
// 	argp_parse (&lazy_prover_argp, argc, argv, 0, 0, &arguments);
// #else
// 	argument_parse(argc, argv, &arguments);
// #endif

// 	LPTermManager term_manager;
// 	LPFormulaManager formula_manager(&term_manager);

// 	if (arguments.benchmark_mode) {
// 		arguments.verbose = false;
// 		arguments.statistics = false;
// 	}

// 	if (arguments.verbose) {
//  		cout << "parsing... ";
// 		cout.flush();
// 	}
// 	clock_t start = clock();
// 	LPRootFormulaVector * parser_result = lazy_prover_parse(arguments.args[0], &formula_manager);
// 	clock_t end = clock();
// 	double parser_time = ((double) (end - start)) / CLOCKS_PER_SEC;

// 	if (parser_result == NULL) {
// 		if (arguments.verbose)
// 			cout << endl;
// 		cerr << "Parser error... " << endl;
// 		exit(-1);
// 	}
// 	else {
// 		if (arguments.verbose)
// 			cout << "done\n";
// 	}

// 	MEM_TRACE(formula_manager.dump_mem_info(););

// 	LPFormulaId root_id = 0;
	
// 	if (arguments.th_name == NULL) {
// 		if (parser_result->get_size() == 0) {
// 			cerr << "There isn't any theorem to be proved... \n";
// 			exit(-1);
// 		}
// 		if (arguments.verbose)
// 			cout << "Trying to prove: " << parser_result->get(0).name << endl;
// 		root_id = parser_result->get(0).formula_id;
// 	}
// 	else {
// 		unsigned int i = 0;
// 		for (; i < parser_result->get_size(); i++) {
// 			if (parser_result->get(i).name == arguments.th_name) {
// 				root_id = parser_result->get(i).formula_id;
// 				break;
// 			}
// 		}
// 		if (i == parser_result->get_size()) {
// 			cerr << "Undefined theorem " << arguments.th_name << endl;
// 			exit(-1);
// 		}
// 		if (arguments.verbose)
// 			cout << "Trying to Prove: " << parser_result->get(i).name << endl;
// 	}

// 	if (arguments.convert_to_cnf) {
// 		formula_manager.convert_to_cnf(cout, -root_id);
// 		return 0;
// 	}

// 	LPSolver solver(&formula_manager);
// 	solver.set_branching_mode(arguments.branching_mode);
// 	solver.set_conflict_resolution_mode(arguments.conflict_resolution_mode);
// 	solver.set_verbosity(arguments.verbose);
// 	solver.set_randomness_level(arguments.randomness_level);
// 	solver.set_clause_relevance(arguments.clause_relevance);
// 	solver.set_cleanup_period(arguments.cleanup_period);
// 	solver.enable_subsumed_clauses_removal(arguments.remove_subsumed_clauses);
// 	solver.enable_polarity_optimization(arguments.polarity_optimization);
// 	solver.enable_implication_graph_optimization(arguments.implication_graph_optimization);
// 	solver.enable_lookahead_optimization(arguments.lookahead_optimization);
// 	solver.set_lookahead_relevance(arguments.lookahead_relevance);
// 	solver.enable_experimental_heuristics(arguments.experimental_heuristics);
// 	solver.set_npc_threshold(arguments.npc_threshold);

// 	if (arguments.verbose)
// 		cout << "solving...\n";
// 	start = clock();
// 	bool solver_result = solver.prove(root_id);
// 	end = clock();
// 	double total_solver_time = ((double) (end - start)) / CLOCKS_PER_SEC;
 
// 	if (solver_result) {
// 		if (!arguments.benchmark_mode)
// 			cout << "Proved\n";
// 	}
// 	else {
// 		if (arguments.validate_counter_example) {
// 			if (arguments.verbose)
// 				cout << "  validating counter example... "; cout.flush();
// 			if (solver.check_counter_example(root_id)) {
// 				if (arguments.verbose)
// 					cout << "done\n";
// 			}
// 			else {
// 				if (arguments.verbose)
// 					cout << "FAILED\n";
// 				else {
// 					cout << "BUG DETECTED: failed to validate counter example\n";
// 					exit(1);
// 				}
// 			}
// 		}
// 		if (!arguments.benchmark_mode) {
// 			cout << "Counter example detected...\n";
// 			if (!arguments.dont_print_counter_example) {
// 				solver.dump_atomic_formula_values(cout, root_id);
// 				cout << "ICS information: \n";
// 				solver.dump_ics_state();
// 				cout << endl;
// 			}
// 		}
// 	}
	
// 	if (arguments.statistics) {
// 		cout << "Statistics...\n";
// 		cout << "  number of formulas: " << formula_manager.get_num_formulas() << endl;
// 		cout << "    number of atomic formulas: " << formula_manager.get_num_atoms() << endl;
// 		cout << "  parser time: " << parser_time << " secs\n";
// 		cout << "  total solver time: " << total_solver_time << " secs\n";
// 		cout << "    preprocessor time: " << solver.get_preprocessing_time() << " secs\n";
// 		if (arguments.polarity_optimization)
// 			cout << "      polarity optimization time: " << solver.get_polarity_optimization_time() << " secs\n";
// 		if (arguments.implication_graph_optimization)
// 			cout << "      implication graph optimization time: " << solver.get_implication_graph_optimization_time() << " secs\n";
// 		if (arguments.lookahead_optimization)
// 			cout << "      lookahead optimization time: " << solver.get_lookahead_optimization_time() << " secs\n";
// 		cout << "    (main loop) solver time: " << solver.get_solver_time() << " secs\n";
// 		cout << "      branching/activation time: " << solver.get_branching_time() << " secs\n";
// 		cout << "      constraint propagation time: " << solver.get_constraint_propagation_time() << " secs\n";
// 		cout << "      conflict resolution time: " << solver.get_conflict_resolution_time() << " secs\n";
// 		cout << "      ICS time: " << solver.get_ics_time() << " secs\n";
// 		cout << "      ICS explain function time: " << solver.get_ics_explain_time() << " secs\n";
// 		cout << "  number of backtracks: " << solver.get_num_backtracks() << endl;
// 		cout << "    number of non-chronological backtrackings: " << solver.get_num_non_chronological_backtracking() << endl;
// 		cout << "  number of inconsistencies detected by ICS: " << solver.get_num_ics_detected_inconsistencies() << endl;
// 		cout << "  number of case splits: " << solver.get_num_case_splits() << endl;
// 		cout << "  number of deductions: " << solver.get_num_deductions() << endl;
// 		cout << "  maximum decision level: " << solver.get_max_decision_level() << endl;
// 		cout << "  number of calls to ICS: " << solver.get_num_ics_calls() << endl;
// 		cout << "  conflict clauses statistics:\n";
// 		cout << "    [1,  10]   = " << solver.get_num_generated_conflict_clauses_smaller_than_10() << endl;
// 		cout << "    (10, 30]   = " << solver.get_num_generated_conflict_clauses_smaller_than_30() << endl;
// 		cout << "    (30, 50]   = " << solver.get_num_generated_conflict_clauses_smaller_than_50() << endl;
// 		cout << "    (50, 100]  = " << solver.get_num_generated_conflict_clauses_smaller_than_100() << endl;
// 		cout << "    (100, inf) = " << solver.get_num_generated_conflict_clauses_larger_than_100() << endl;
// 		cout << "  number of deleted clauses: " << solver.get_num_deleted_clauses() << endl;
// 		if (arguments.remove_subsumed_clauses)
// 			cout << "  number of subsumed clauses: " << solver.get_num_subsumed_clauses() << endl;
// 		if (arguments.implication_graph_optimization)
// 			cout << "  number of assignments produced by implication graph optimization: " 
// 					 << solver.get_num_implication_graph_optimization_assignments() << endl;
// 		if (arguments.lookahead_optimization)
// 			cout << "  number of assignments produced by lookahead optimization: "
// 					 << solver.get_num_lookahead_optimization_assignments() << endl;
// 	}

// 	if (arguments.benchmark_mode) {
// 		cout << total_solver_time << ", " << solver.get_preprocessing_time() << ", " << solver.get_solver_time() << ", " 
// 				 << solver.get_branching_time() << ", " << solver.get_constraint_propagation_time() << ", " << solver.get_conflict_resolution_time()
// 				 << ", " << solver.get_ics_time() << ", " << solver.get_ics_explain_time() << ", " << solver.get_num_backtracks() << ", "
// 				 << solver.get_num_case_splits() << ", " << solver.get_num_deductions() << ", " << solver.get_num_deleted_clauses() << ", "
// 				 << solver.get_num_subsumed_clauses() << endl;
// 	}

// 	if (solver_result)
// 		return 0;
// 	else
// 		return 1;
// }
















