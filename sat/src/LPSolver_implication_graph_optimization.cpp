/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 14, 2002: Created.
***/
#include"LPSolver.h"
#include"lisp.h"
#include"fast-allocator.h"

typedef FastAllocator LPIdListAllocator;

static LPIdListAllocator * int_list_allocator = NULL;

unsigned int LPSolver::literal2node_id(LPFormulaId literal) {
	unsigned int idx = absolute(literal);
	if (literal > 0)
		return idx;
	else
		return idx + get_internal_formulas_array_size();
}

LPFormulaId LPSolver::node_id2literal(unsigned int node_id) {
	if (node_id >= get_internal_formulas_array_size()) 
		return - (node_id - get_internal_formulas_array_size());
	else 
		return node_id;
}

inline void connect_nodes(LPIdList ** implication_graph, unsigned int source, unsigned int target) {
	implication_graph[source] = cons(target, implication_graph[source], int_list_allocator);
}

inline LPIdList** allocate_blank_implication_graph(unsigned int size)
{
	LPIdList ** implication_graph = new LPIdList*[size];
	memset(implication_graph, 0, sizeof(LPIdList *) * size);
	return implication_graph;
}

LPIdList ** LPSolver::compute_implication_graph() {
	
	unsigned int num_variables = num_formulas+1;
	unsigned int num_nodes = num_variables * 2;

	const LPFormula * formula = formula_manager->get_formula(absolute(root_formula_id));
	if (formula->is_or() && root_formula_id < 0) {
		// formula is an AND!
		LPIdList ** implication_graph = NULL;
		unsigned int n = formula->get_num_arguments();
		for (unsigned int i = 0; i < n; i++) {
			int child_id = -formula->get_argument(i); // I must invert the child, since the AND is represented as -(OR -child_1 ... - child_n)
			const LPFormula * child = formula_manager->get_formula(absolute(child_id));
			if (child->is_or()) {
				assert(child_id > 0); // by construction of the LPFormula
				unsigned child_size = child->get_num_arguments();
				if (child_size == 2) {
					// child is a binary clause...
					if (implication_graph == NULL)
						implication_graph = allocate_blank_implication_graph(num_nodes);
					LPFormulaId l1 = child->get_argument(0);
					LPFormulaId l2 = child->get_argument(1);
					connect_nodes(implication_graph,
												literal2node_id(-l1),
												literal2node_id(l2));
					connect_nodes(implication_graph,
												literal2node_id(-l2),
												literal2node_id(l1));
				}
			}
		}
		return implication_graph;
	}
	else
		return NULL; // optimization is not applicable
}

void LPSolver::dump_implication_graph(ostream & output, LPIdList ** implication_graph) {
	unsigned int num_variables = get_internal_formulas_array_size();
	unsigned int num_nodes = 2 * num_variables;
	output << "Implication graph: " << endl;
	for (unsigned int source_id = 0; source_id < num_nodes; source_id++) {
		LPFormulaId source = node_id2literal(source_id);
		LPIdList * targets = implication_graph[source_id];
		while (targets != NULL) {
			unsigned int target_id = car(targets);
			LPFormulaId target = node_id2literal(target_id);
			output << "  " << source << "[";
			formula_manager->dump_formula(output, source);
			output << "] -> " << target << "[";
			formula_manager->dump_formula(output, target);
			output << "]\n";
			targets = cdr(targets);
		}
	}
}

LPIdList ** LPSolver::transpose_implication_graph(LPIdList ** implication_graph) {
	unsigned int num_variables = get_internal_formulas_array_size();
	unsigned int num_nodes = 2 * num_variables;
	LPIdList ** result_graph = allocate_blank_implication_graph(num_nodes);
	
	for (unsigned int source_id = 0; source_id < num_nodes; source_id++) {
		LPIdList * targets = implication_graph[source_id];
		while (targets != NULL) {
			connect_nodes(result_graph, car(targets), source_id);
			targets = cdr(targets);
		}
	}
	return result_graph;
}

static void scc_first_dfs(LPIdList ** graph, DFSColor * colors, LPIdList *& node_list, unsigned int node_id) {
	colors[node_id] = gray;
	LPIdList * targets = graph[node_id];
	while(targets != NULL) {
		if (colors[car(targets)] == white)
			scc_first_dfs(graph, colors, node_list, car(targets));
		targets = cdr(targets);
	}
	colors[node_id] = black;
	node_list = cons(node_id, node_list);
}

static void scc_second_dfs(LPIdList ** graph, DFSColor * colors, unsigned int * predecessors, unsigned int node_id) {
	colors[node_id] = gray;
	LPIdList * targets = graph[node_id];
	while(targets != NULL) {
		unsigned int target_id = car(targets);
		if (node_id == target_id && predecessors[node_id] == 0) // self loop
			predecessors[node_id] = node_id;
		else if (colors[target_id] == white) {
			predecessors[target_id] = node_id;
			scc_second_dfs(graph, colors, predecessors, target_id);
		}
		targets = cdr(targets);
	}
	colors[node_id] = black;
}

bool LPSolver::apply_implication_graph_optimization()
{
 	clock_t start = clock();
	if (verbose) {
		cerr << "    applying implication graph optimization...";
		cout.flush();
	}

	int_list_allocator = new LPIdListAllocator(INT_LIST_ALLOCATOR_SIZE);

	unsigned int num_variables = get_internal_formulas_array_size();
	unsigned int num_nodes = 2 * num_variables;
	LPIdList ** implication_graph = compute_implication_graph();
	if (implication_graph == NULL) {
		if (verbose)
			cerr << " not applicable\n";
		return true;
	}
	if (verbose)
		cerr << endl;
	SOLVER_TRACE(dump_implication_graph(ctrace, implication_graph););
	LPIdList ** transpose_graph = transpose_implication_graph(implication_graph);
	DFSColor * colors = new DFSColor[num_nodes];
	memset(colors, 0, sizeof(DFSColor) * num_nodes);

	unsigned int * predecessors = new unsigned int[num_nodes];
	memset(predecessors, 0, sizeof(unsigned int) * num_nodes);

	LPIdList * node_list = NULL;
	
	for (unsigned int i = 0; i < num_nodes; i++) {
		if (colors[i] == white)
			scc_first_dfs(implication_graph, colors, node_list, i);
	}	

	// prepare for new dfs
	memset(colors, 0, sizeof(DFSColor) * num_nodes);

	LPIdList * curr = node_list;
	while (curr != NULL) {
		if (colors[car(curr)] == white)
			scc_second_dfs(transpose_graph, colors, predecessors, car(curr));
		curr = cdr(curr);
	}

	bool result = true;
	// The predecessor of a node is in the same SCC.
	for (unsigned int node_id = 0; node_id < num_nodes; node_id++) {
		unsigned int predecessor_id = predecessors[node_id];
		if (predecessor_id != 0) {
			if (node_id != predecessor_id) {
				LPFormulaId f1 = node_id2literal(node_id);
				LPFormulaId f2 = node_id2literal(predecessor_id);
				SOLVER_TRACE(ctrace << "  assigning " << f1 << " = " << f2 << endl;);
				num_implication_graph_optimization_assignments++;
				if (!assign_formula(f1, f2, 0)) {
					result = false;
					break;
				}
			}
		}
	}

	delete[] predecessors;
	delete[] colors;
	delete[] transpose_graph;
	delete[] implication_graph;
	delete int_list_allocator;

	clock_t end = clock();
	implication_graph_optimization_time = ((double) (end - start)) / CLOCKS_PER_SEC;

	return result;
}

