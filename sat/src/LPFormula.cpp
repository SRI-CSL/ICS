/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 18, 2002: Created.
***/

#include<assert.h>
#include"LPFormula.h"

bool LPFormulaIdEqualTo::operator()(LPFormulaId id1, LPFormulaId id2) const {
	const LPFormula * obj1 = manager->get_formula(id1);
	const LPFormula * obj2 = manager->get_formula(id2);
	if (obj1->kind != obj2->kind)
		return false;
	switch (obj1->kind) {
	case LP_EQ:
		return obj1->get_atom() == obj2->get_atom();
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
		return obj->get_atom();
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
	DBG_CODE(cout << "LPFormulaManager initialized.\n";
					 cout << "init_num_formulas = " << init_num_formulas << endl;);
	formulas = new LPFormula[init_num_formulas];
	DBG_CODE(cout << "formulas = " << formulas << endl;);
	num_formulas = init_num_formulas;
	// initialize "true-proposition"
	formulas[LPTrueId].kind = LP_PROPOSITION;
	formulas[LPTrueId].data.prop_var_name = TRUE_CONSTANT;
	next_formula_id = 2; // 0 is not used, and 1 is the "true-proposition".
	
	arguments = new LPFormulaId[init_num_args];
	num_arguments = init_num_args;
	next_argument_pos = 0;
}

LPFormulaManager::~LPFormulaManager()
{
	DBG_CODE(cout << "Destroying LPFormulaManager\n";);
	delete[] formulas;
	delete[] arguments;
	formulas = NULL;
	arguments = NULL;
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
LPFormulaId LPFormulaManager::create_atom(int a, int not_a)
{
	create_space_for_new_formula();
	
	LPFormula * new_formula = &(formulas[next_formula_id]);
	new_formula->kind = LP_EQ;
	new_formula->data.eq.atom = a;
	new_formula->data.eq.not_atom = not_a;

	LPFormulaPool::const_iterator it = formula_pool.find(next_formula_id);
 	if (it != formula_pool.end()) {
 		// formula was already created...
 		return *it;
 	}
 	formula_pool.insert(next_formula_id);

	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_or(unsigned int num_args, LPFormulaId * args)
{
	DBG_CODE(cout << "   create or\n";
					 cout << "num_args = " << num_args << endl;
					 for (int i = 0; i < num_args; i++)
					 cout << args[i] << " ";
					 cout << endl;);
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
	DBG_CODE(cout << "num_args = " << num_args << endl;);
	
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

	DBG_CODE(for (int i = 0; i < num_args; i++)
					 cout << args[i] << " ";
					 cout << endl;
					 cout << "new_formula->data.boolean_op.num_arguments = " << new_formula->data.boolean_op.num_arguments << endl;
					 cout << "next_formula_id  = " << next_formula_id  << endl;);
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


