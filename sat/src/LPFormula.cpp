/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 18, 2002: Created.
***/

#include<assert.h>
#include"LPFormula.h"

value * ics_allocate_root()
{
	value * r = (value *) malloc(sizeof(value));
	icsat_register(r);
	return r;
}

void ics_free_root(value * r)
{
	icsat_deregister(r);
	free(r);
}

bool LPFormulaIdEqualTo::operator()(LPFormulaId id1, LPFormulaId id2) const {
	const LPFormula * obj1 = manager->get_formula(id1);
	const LPFormula * obj2 = manager->get_formula(id2);
	if (obj1->kind != obj2->kind)
		return false;
	switch (obj1->kind) {
	case LP_EQ:
		UNREACHABLE_CODE;
		// return obj1->get_atom() == obj2->get_atom() && obj1->get_not_atom() == obj2->get_not_atom();
	case LP_PROPOSITION:
		return obj1->get_propositional_variable_name() == obj2->get_propositional_variable_name();
	case LP_IFF:
	case LP_ITE:
	case LP_OR:
		if (obj1->get_num_arguments() != obj2->get_num_arguments())
			return false;
		return memcmp(obj1->get_arguments(), obj2->get_arguments(), sizeof(LPFormulaId) * obj1->get_num_arguments()) == 0;
	default:
		assert(false);
	}
}

static hash<const char *> proc;

size_t LPFormulaIdHashFcn::operator()(LPFormulaId id) const
{
	const LPFormula * obj = manager->get_formula(id);
	size_t result = 0;
	switch (obj->kind) {
	case LP_EQ:
		UNREACHABLE_CODE;
		// return obj->get_eq_lhs() + 3 * obj->get_eq_rhs();
	case LP_PROPOSITION:
		return proc(obj->data.prop_var_name);
	case LP_IFF:
	case LP_ITE:
	case LP_OR: {
		unsigned int num_args = obj->get_num_arguments();
		for (unsigned int i = 0; i < num_args; i++)
			result = result * 3 + obj->get_argument(i);
		return result;
	}	
	default:
		assert(false);
	}
}

#define AUX_ARGUMENTS_ARRAY_SIZE 1024

LPFormulaManager::LPFormulaManager(unsigned int init_num_formulas,
																	 unsigned int init_num_args, unsigned int init_pool_size):
	equal_fcn(this),
	hash_fcn(this),
	formula_pool(init_pool_size, hash_fcn, equal_fcn)
{
	formulas = new LPFormula[init_num_formulas];
	num_formulas = init_num_formulas;
	// initialize "true-proposition"
	formulas[LPTrueId].kind = LP_PROPOSITION;
	formulas[LPTrueId].data.prop_var_name = TRUE_CONSTANT;
	next_formula_id = 2; // 0 is not used, and 1 is the "true-proposition".
	
	arguments = new LPFormulaId[init_num_args];
	num_arguments = init_num_args;
	next_argument_pos = 0;

// 	variables = new LPTermId[init_num_args];
// 	num_variables = init_num_args;
// 	next_variable_pos = 0;

	// creates the true term constant
// 	term_true = create_constant(TRUE_CONSTANT);

// 	type_checking = false;
}

LPFormulaManager::~LPFormulaManager()
{
	delete[] formulas;
	delete[] arguments;
// 	delete[] variables;
}

void LPFormulaManager::dump_formula(ostream & target, const LPFormula * f, bool negated) const
{
	if (negated)
		target << "(~ ";
	switch(f->get_kind()) {
	case LP_PROPOSITION:
		target << f->get_propositional_variable_name(); 
		break;
	case LP_EQ:
		target << "(ATOM " << f->get_atom() << ")";
		// term_manager->dump_term(target, f->get_eq_lhs());
		// target << " ";
		// term_manager->dump_term(target, f->get_eq_rhs());
		// target << ")";
		break;
	case LP_OR:
	case LP_IFF:
	case LP_ITE:
		switch(f->get_kind()) {
		case LP_OR: target << "(OR"; break;
		case LP_IFF: target << "(IFF"; break; 
		case LP_ITE: target << "(ITE"; break;
		default:
			assert(false);
		}
		for (unsigned int i = 0; i < f->get_num_arguments(); i++) {
			target << " ";
			dump_formula(target, f->get_argument(i));
		}
		target << ")";
		break;
	default:
		assert(false);
	}
	if (negated)
		target << ")";
}

LPFormulaId LPFormulaManager::create_proposition(char * name)
{
	assert(name != NULL);

	create_space_for_new_formula();

	LPFormula * new_formula = &(formulas[next_formula_id]);
	new_formula->kind = LP_PROPOSITION;
	new_formula->data.prop_var_name = UString::uniqueString(name);

	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
	if (it != formula_pool.end()) {
		// formula was already created...
		return *it;
	}

	formula_pool.insert(next_formula_id);
	
	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_iff(LPFormulaId lhs, LPFormulaId rhs)
{
	if (lhs == rhs) // A <-> A === TRUE
		return LPTrueId; 
	if (lhs == -rhs) // A <-> -A === FALSE
		return LPFalseId; 
	if (!formula_lt(lhs, rhs))
		swap(lhs,rhs);
	if (lhs == LPTrueId) // TRUE <-> A === A
		return rhs;
	if (lhs == LPFalseId) // FALSE <-> A === -A
		return -rhs;
	
	create_space_for_new_formula();
	create_space_for_new_arguments(2);
	
	LPFormula * new_formula = &(formulas[next_formula_id]);

	new_formula->kind = LP_IFF;
	new_formula->data.boolean_op.num_arguments = 2;
	new_formula->data.boolean_op.arguments = &(arguments[next_argument_pos]);
	arguments[next_argument_pos] = lhs;
	arguments[next_argument_pos+1] = rhs;

	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
	if (it != formula_pool.end()) {
		// formula was already created...
		return *it;
	}

	next_argument_pos += 2;
	
	formula_pool.insert(next_formula_id);

	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e)
{
	if (t == e) // ite(C, A, A) == A
		return t; 
	if (c == LPTrueId) // ite(TRUE, A, B) == A
		return t;
	if (c == LPFalseId) // ite(FALSE, A, B) == B
		return e;
	if (t == LPTrueId)  // ite(C, TRUE, B) == C OR B
		return create_or(c, e); 
	if (t == LPFalseId) // ite(C, FALSE, B) == -C AND B
		return create_and(-c, e); 
	if (e == LPTrueId)  // ite(C, A, TRUE) == -C OR A
		return create_or(-c, t);
	if (e == LPFalseId)  // ite(C, A, FALSE) == C AND A
		return create_and(c, t);
	
	create_space_for_new_formula();
	create_space_for_new_arguments(3);

	LPFormula * new_formula = &(formulas[next_formula_id]);
	new_formula->kind = LP_ITE;
	new_formula->data.boolean_op.num_arguments = 3;
	new_formula->data.boolean_op.arguments = &(arguments[next_argument_pos]);
	arguments[next_argument_pos] = c;
	arguments[next_argument_pos+1] = t;
	arguments[next_argument_pos+2] = e;

	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
	if (it != formula_pool.end()) {
		// formula was already created...
		return *it;
	}

	next_argument_pos += 3;
	
	formula_pool.insert(next_formula_id);

	return next_formula_id++;
}

// create_eq is extremely similar to create_iff
LPFormulaId LPFormulaManager::create_atom(value a, value not_a)
{
// 	if (lhs == rhs)
// 		return LPTrueId; // true
// 	if (lhs > rhs)
// 		swap(lhs,rhs);

// 	if (type_checking) {
// 		if (get_term_sort(lhs) != get_term_sort(rhs))
// 			throw new LPEqTypeError(this, lhs, rhs);
// 	}
	
	value * a_ptr = ics_allocate_root();
	value * not_a_ptr = ics_allocate_root();
	*a_ptr = a;
	*not_a_ptr = not_a;

	create_space_for_new_formula();
	
	LPFormula * new_formula = &(formulas[next_formula_id]);
	new_formula->kind = LP_EQ;
	new_formula->data.eq.atom = a_ptr;
	new_formula->data.eq.not_atom = not_a_ptr;

//	REMARK: atoms are not cached...
// 	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
// 	if (it != formula_pool.end()) {
// 		// formula was already created...
// 		return *it;
// 	}
// 	formula_pool.insert(next_formula_id);

	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_or(unsigned int num_args, LPFormulaId * args)
{
	static sortLPFormulaId sortFn;

	LPFormulaId * aux_args = &(arguments[next_argument_pos]);

	// expand arguments
	unsigned int real_number_of_args = 0;
	for (unsigned int i = 0; i < num_args; i ++) {
		LPFormulaId curr_arg = args[i];
		const LPFormula * curr_arg_formula = get_formula(curr_arg);
		if (curr_arg_formula->is_or() && is_positive(curr_arg)) {
			if (create_space_for_new_arguments(real_number_of_args + curr_arg_formula->get_num_arguments()))
				aux_args = &(arguments[next_argument_pos]);
			memcpy(&(aux_args[real_number_of_args]), 
						 curr_arg_formula->get_arguments(), 
						 curr_arg_formula->get_num_arguments() * sizeof(LPFormulaId));
			real_number_of_args += curr_arg_formula->get_num_arguments();
		}
		else {
			if (create_space_for_new_arguments(real_number_of_args + 1))
				aux_args = &(arguments[next_argument_pos]);
			aux_args[real_number_of_args] = curr_arg;
			real_number_of_args++;
		}
	}
	args = aux_args;
	num_args = real_number_of_args;
	
	sortFn(args, num_args);

	// remove trues, falses, duplicates, and check if it is not trivially true.
	unsigned int j = 0;
	for (; j < num_args; j++) {
		if (args[j] == LPTrueId)
			return LPTrueId; // trivially true
		if (args[j] != LPFalseId)
			break;
	}
	if (j == num_args)
		return LPFalseId; // empty OR
	assert(args[j] != LPTrueId && args[j] != LPFalseId);
	args[0] = args[j];
	unsigned int i = j + 1;
	j = 0;
	for (; i < num_args; i++) {
		assert(args[i] != LPTrueId && args[i] != LPFalseId);
		if (args[j] == args[i]) {
			continue;
		}
		if (args[j] == -args[i]) {
			// the formula is trivially true...
			return LPTrueId;
		}
		assert(j+1 <= i);
		args[j+1] = args[i];
		j++;
	}
	num_args = j+1;
	assert(num_args >= 1);
	if (num_args == 1) {
		// It is not necessary to create an "or" node...
		return args[0];
	}

	create_space_for_new_formula();

	LPFormula * new_formula = &(formulas[next_formula_id]);
	
	new_formula->kind = LP_OR;
	new_formula->data.boolean_op.num_arguments = num_args;
	new_formula->data.boolean_op.arguments = args; 

	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
	if (it != formula_pool.end()) {
		// formula was already created...
		return *it;
	}
	next_argument_pos += num_args;

	// add formula to formula pool
	formula_pool.insert(next_formula_id);

	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_or(LPFormulaId arg1, LPFormulaId arg2)
{
	static LPFormulaId TmpArguments[2];
	TmpArguments[0] = arg1; TmpArguments[1] = arg2;
	return create_or(2, TmpArguments);
}

LPFormulaId LPFormulaManager::create_and(unsigned int num_args, LPFormulaId * arguments)
{
	for (unsigned int i = 0; i < num_args; i++)
		arguments[i] = -arguments[i];
	return -create_or(num_args, arguments);
}
		
// inline void remove_duplicate_variables(unsigned int & num_vars, LPTermId * vars)
// {
// #ifndef NDEBUG
// 	// The vars array must be sorted...
// 	static sortLPTermId sortFn;
// 	assert(sortFn.is_sorted(vars, num_vars));
// #endif

// 	unsigned int j = 0;
// 	for (unsigned int i = 1; i < num_vars; i++) {
// 		if (vars[j] == vars[i])
// 			continue;
// 		assert(j+1 <= i);
// 		vars[j+1] = vars[i];
// 		j++;
// 	}
// 	num_vars = j+1;
// 	assert(num_vars >= 1);
// }

// LPFormulaId LPFormulaManager::create_exists(unsigned int num_vars, LPTermId * vars, LPFormulaId f_id)
// {
// 	static sortLPTermId sortFn;

// 	LPTermId * aux_vars = &(variables[next_variable_pos]);
	
// 	const LPFormula * f = get_formula(f_id);
// 	if (f->is_exists() && is_positive(f_id)) {
// 		// expand the variables...
// 		if (create_space_for_new_variables(num_vars + f->data.exists.num_variables))
// 			aux_vars = &(variables[next_variable_pos]); // recompute aux_vars position
// 		memcpy(aux_vars, vars, num_vars * sizeof(LPTermId));
// 		memcpy(&(aux_vars[num_vars]), f->data.exists.variables, f->data.exists.num_variables);
// 		num_vars = num_vars + f->data.exists.num_variables;
// 		vars = aux_vars;
// 		f_id = f->data.exists.formula;
// 		f = get_formula(f_id);
// 	}
// 	else {
// 		if (create_space_for_new_variables(num_vars))
// 			aux_vars = &(variables[next_variable_pos]);
// 		memcpy(aux_vars, vars, num_vars * sizeof(LPTermId));
// 		vars = aux_vars;
// 	}

// 	sortFn(vars, num_vars);
// 	remove_duplicate_variables(num_vars, vars);

// 	// remove irrelevant variables, that is, map (FORALL X . EXISTS X ... ) to (EXISTS X ...)
// 	// In other words, I should compute the difference between "vars" and "f->data.exists.variables"
// 	if (f->is_exists()) {
// 		assert(is_negative(f_id));
// 		unsigned int curr = 0;
// 		unsigned int i = 0;
// 		unsigned int j = 0;
// 		while (i < num_vars && j < f->data.exists.num_variables) {
// 			assert(curr <= i);
// 			if (vars[i] == f->data.exists.variables[j]) {
// 				i++; j++; 
// 			}
// 			else if (term_lt(vars[i], f->data.exists.variables[j])) {
// 				vars[curr] = vars[i];
// 				curr++; i++;
// 			}
// 			else {
// 				assert(term_lt(f->data.exists.variables[j], vars[i]));
// 				j++;
// 			}
// 		}
// 		assert(i <= num_vars);
// 		// move the remaining variables...
// 		if (i < num_vars) {
// 			if (curr != i)
// 				for(; i < num_vars; i++, curr++)
// 					vars[curr] = vars[i];
// 			else
// 				curr = num_vars;
// 		}
// 		num_vars = curr;
// 		if (num_vars == 0) 
// 			return f_id;
// 	}

// 	create_space_for_new_formula();

// 	LPFormula * new_formula = &(formulas[next_formula_id]);
	
// 	new_formula->kind = LP_EXISTS;
// 	new_formula->data.exists.num_variables = num_vars;
// 	new_formula->data.exists.variables = vars; 
// 	new_formula->data.exists.formula = f_id;

// 	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
// 	if (it != formula_pool.end()) {
// 		// formula was already created...
// 		return *it;
// 	}

// 	next_variable_pos += num_vars;

// 	// add formula to formula pool
// 	formula_pool.insert(next_formula_id);

// 	return next_formula_id++;
// }

// LPFormulaId LPFormulaManager::create_exists(LPTermId variable, LPFormulaId f)
// {
// 	static LPTermId var_array[1];	
// 	var_array[0] = variable;
// 	return create_exists(1, var_array, f);
// }

// bool LPFormulaManager::convert_to_cnf(ostream & target, LPFormulaId f)
// {
// 	static bool * visited = NULL;
// 	static int size = 0;
// 	growable_vector<growable_vector<int>, 320628> cnf; // I hacked this line... I should fix it in the future

// 	if (next_formula_id > absolute(size)) {
// 		if (visited != NULL)
// 			delete[] visited;
// 		visited = new bool[next_formula_id];
// 		memset(visited, 0, sizeof(bool)*next_formula_id);
// 		size = next_formula_id;
// 	}

// 	growable_vector<int> * main_f = cnf.allocate();
// 	main_f->push(f);

// 	growable_vector<LPFormulaId> to_do;
// 	to_do.push(f);
// 	while (!to_do.is_empty()) {
// 		LPFormulaId curr = to_do.pop();
// 		int curr_idx = abs(curr);
// 		if (!visited[abs(curr)]) {
// 			visited[abs(curr)] = true;
// 			const LPFormula * curr_formula = get_formula(curr);
// 			switch (curr_formula->get_kind()) {
// 			case LP_OR: {
// 				unsigned int n = curr_formula->get_num_arguments();
// 				growable_vector<int> * c = cnf.allocate();
// 				c->push(-curr_idx);
// 				for (unsigned int i = 0; i < n; i++) {
// 					to_do.push(curr_formula->get_argument(i));
// 					c->push(curr_formula->get_argument(i));
// 				}
// 				for (unsigned int i = 0; i < n; i++) {
// 					growable_vector<int> * curr = cnf.allocate();
// 					curr->push(-curr_formula->get_argument(i));
// 					curr->push(curr_idx);
// 				}
// 				break;
// 			}
// 			case LP_IFF: {
// 				LPFormulaId lhs = curr_formula->get_iff_lhs();
// 				LPFormulaId rhs = curr_formula->get_iff_rhs();
// 				growable_vector<int> * c1 = cnf.allocate();
// 				c1->push(-curr_idx); c1->push(-lhs); c1->push(rhs);
// 				growable_vector<int> * c2 = cnf.allocate();
// 				c2->push(-curr_idx); c2->push(lhs); c2->push(-rhs);
// 				growable_vector<int> * c3 = cnf.allocate();
// 				c3->push(curr_idx); c3->push(lhs); c3->push(rhs);
// 				growable_vector<int> * c4 = cnf.allocate();
// 				c4->push(curr_idx); c4->push(-lhs); c4->push(-rhs);
// 				to_do.push(lhs);
// 				to_do.push(rhs);
// 				break;
// 			}
// 			case LP_ITE: {
// 				LPFormulaId c = curr_formula->get_cond();
// 				LPFormulaId t = curr_formula->get_then();
// 				LPFormulaId e = curr_formula->get_else();
// 				to_do.push(c);
// 				to_do.push(t);
// 				to_do.push(e);
// 				// it is equivalent to (~c | t) & (c | e)
// 				growable_vector<int> * c1 = cnf.allocate();
// 				c1->push(-curr_idx); c1->push(-c); c1->push(t);
// 				growable_vector<int> * c2 = cnf.allocate();
// 				c2->push(-curr_idx); c2->push(c); c2->push(e);
// 				growable_vector<int> * c3 = cnf.allocate();
// 				c3->push(curr_idx); c3->push(c); c3->push(-e);
// 				growable_vector<int> * c4 = cnf.allocate();
// 				c4->push(curr_idx); c4->push(-c); c4->push(-t); 
// 				growable_vector<int> * c5 = cnf.allocate();
// 				c5->push(curr_idx); c5->push(-t); c5->push(-e);
// 				break;
// 			}
// 			case LP_EXISTS:
// 				cnf.reset();
// 				return false; // formula cannot be converted to CNF
// 			case LP_PROPOSITION:
// 				// do nothing...
// 				break;
// 			case LP_EQ:
// 				cnf.reset();
// 				return false; // formula cannot be converted to CNF
// 			default:
// 				assert(false);
// 			}
// 		}
// 	}

// 	unsigned int n = cnf.get_size();
// 	target << "p cnf " << next_formula_id << " " << n << endl; 
// 	for (unsigned int i = 0; i < n; i++) {
// 		growable_vector<int> & clause = cnf.get(i);
// 		unsigned int num_lits = clause.get_size();
// 		for (unsigned int j = 0; j < num_lits; j++) {
// 			target << clause.get(j) << " ";
// 		}
// 		target << "0\n";
// 	}
// 	return true;
// }


// bool LPFormulaManager::contains_variable(const LPFormula * f, const LPTerm * var) const
// {
// 	// TODO
// 	return false;
// }

// unsigned int LPFormulaManager::get_num_atoms() const
// {
// 	unsigned int result = 0;
// 	for (unsigned int i = 1; i < next_formula_id; i++) {
// 		if (formulas[i].is_atomic())
// 			result++;
// 	}
// 	return result;
// }

void LPFormulaManager::dump_mem_info()
{
#ifdef TRACING
	ctrace << "[memory] Formula manager memory info: " << endl;
	ctrace << "  formula array size: " << num_formulas << endl;
	ctrace << "  next_formula_id: " << next_formula_id << endl;
	ctrace << "  argument array size: " << num_arguments << endl;
	ctrace << "  next_argument_pos: " << next_argument_pos << endl;
	ctrace << "  variable array size: " << num_variables << endl;
	ctrace << "  next_variable_pos: " << next_variable_pos << endl;
	ctrace << "  pool size: " << formula_pool.size() << endl;
	ctrace << "  pool max size: " << formula_pool.max_size() << endl;
	ctrace << "  memory consumed: " << ((sizeof(LPFormula) * num_formulas +
																		 sizeof(LPFormulaId) * num_arguments +
																		 sizeof(LPTermId) * num_variables) / 1024) << " Kb\n";
#endif
}


ostream& operator<<(ostream& target, const LPFormulaManager & m)
{
	target << "Formula Manager\n---------------\n";
	target << "\tnumber of formulas: " << m.get_num_formulas() << endl;
	for (unsigned int i = 1; i <= m.get_num_formulas(); i ++) {
		target << "\t" << i << ": ";
		m.dump_formula(target, i);
		target << endl;
	}
	return target;
}


LPFormulaManager * sat_formula_manager = NULL;

extern "C" {
	void sat_initialize() {
		sat_formula_manager = new LPFormulaManager();
	}
	void sat_finalize() {
		if (sat_formula_manager != NULL) {
			delete sat_formula_manager;
			sat_formula_manager = NULL;
		}
	}
	LPFormulaId sat_mk_true() {
		return LPTrueId;
	}
	LPFormulaId sat_mk_false() {
		return LPFalseId;
	}
	LPFormulaId sat_mk_or(unsigned int num_args, LPFormulaId * args) {
		return sat_formula_manager->create_or(num_args, args);
	}
	LPFormulaId sat_mk_and(unsigned int num_args, LPFormulaId * args) {
		return sat_formula_manager->create_and(num_args, args);
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
		return sat_formula_manager->create_not(f);
	}
	LPFormulaId sat_mk_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e) {
		return sat_formula_manager->create_ite(c,t,e);
	}
	LPFormulaId sat_mk_atom(value a, value not_a) {
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

}

