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
     demoura - Mar 18, 2002: Created.
***/
#include<assert.h>
#include"LPFormula.h"
#include"queue.h"
#include"growable-vector.h"

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
// 	DBG_CODE(cout << "LPFormulaManager initialized.\n";
// 					 cout << "init_num_formulas = " << init_num_formulas << endl;);
	formulas = new LPFormula[init_num_formulas];
// 	DBG_CODE(cout << "formulas = " << formulas << endl;);
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
// 	DBG_CODE(cout << "Destroying LPFormulaManager\n";);
	// cout << ">>Destroying LPFormulaManager\n";
	delete[] formulas;
	delete[] arguments;
	formulas = NULL;
	arguments = NULL;
	// cout << "<<Destroying LPFormulaManager\n";
}

extern "C" { void icsat_atom_pp(int x); }

void LPFormulaManager::dump_formula(ostream & target, const LPFormula * f, bool negated) const
{
	if (negated)
		target << "(~ ";
	switch(f->get_kind()) {
	case LP_PROPOSITION:
		target << f->get_propositional_variable_name(); 
		break;
	case LP_EQ:
		target << "(ATOM " << f->get_atom() << ", ";
		target.flush();
		icsat_atom_pp(f->get_atom());
		target << ")";
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
			target << " " << f->get_argument(i);
			// dump_formula(target, f->get_argument(i));
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

	new_formula->data.eq.atom = not_a;
	new_formula->data.eq.not_atom = a;
	LPFormulaPool::const_iterator it2 = formula_pool.find(next_formula_id);
 	if (it2 != formula_pool.end()) {
 		// formula was already created...
		// cout << "neg case it2 " << (*it2) << " " << (0 - (*it2)) << endl;
 		return 0 - (*it2);
 	}
	
	new_formula->data.eq.atom = a;
	new_formula->data.eq.not_atom = not_a;

 	formula_pool.insert(next_formula_id);
	return next_formula_id++;
}

LPFormulaId LPFormulaManager::create_or(unsigned int num_args, LPFormulaId * args)
{
// 	cout << "   create or\n";
// 	cout << "num_args = " << num_args << endl;
// 	for (int i = 0; i < num_args; i++)
// 		cout << args[i] << " ";
// 	cout << endl;

	LPFormulaId res = create_or_aux(num_args, args);
	
// 	unsigned res_idx = absolute(res);
// 	bool sign = res < 0;
// 	const LPFormula * f = get_formula(res_idx);
// 	dump_formula(cout, f, sign);
// 	cout << "\n--------------------------\n";
	
	return res;
}

LPFormulaId LPFormulaManager::create_or_aux(unsigned int num_args, LPFormulaId * args)
{	
	static sortLPFormulaId sortFn;

	create_space_for_new_arguments(num_args);

	LPFormulaId * aux_args = &(arguments[next_argument_pos]);

	for (unsigned int i = 0; i < num_args; i ++) {
		aux_args[i] = args[i];
	}

	args = aux_args;
	
	sortFn(args, num_args);

	// remove trues, falses, duplicates, and check if it is not trivially true.
	unsigned int j = 0;
	for (; j < num_args; j++) {
		if (args[j] == LPTrueId)
			return LPTrueId; // trivially true
		if (args[j] != LPFalseId)
			break;
		// cout << "removing false...\n";
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
			// cout << "removing irrelevant...\n";
			continue;
		}
		if (args[j] == -args[i]) {
			// cout << "trivially true...\n";
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

// 	DBG_CODE(for (int i = 0; i < num_args; i++)
// 					 cout << args[i] << " ";
// 					 cout << endl;
// 					 cout << "new_formula->data.boolean_op.num_arguments = " << new_formula->data.boolean_op.num_arguments << endl;
// 					 cout << "next_formula_id  = " << next_formula_id  << endl;);
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
	cout << "[memory] Formula manager memory info: " << endl;
	cout << "  formula array size: " << num_formulas << endl;
	cout << "  number of formulas: " << (next_formula_id - 1) << endl;
	cout << "  argument array size: " << num_arguments << endl;
	cout << "  number of arguments: " << (next_argument_pos - 1) << endl;
	cout << "  pool size: " << formula_pool.size() << endl;
	// cout << "  pool max size: " << formula_pool.max_size() << endl;
	cout << "  memory consumed: " << ((sizeof(LPFormula) * num_formulas +
																		 sizeof(LPFormulaId) * num_arguments) / 1024) << " Kb\n";
}

LPFormulaId LPFormulaManager::normalize_formula(LPFormulaId f_id)
{
	int * cache;
	cache = new int[num_formulas];
	memset(cache, 0, sizeof(int) * num_formulas);

	LPFormulaId new_f_id = normalize_formula_aux(f_id, cache);

	delete[] cache;

	return new_f_id;
}

LPFormulaId LPFormulaManager::normalize_formula_aux(LPFormulaId f_id, int * cache)
{
	if (f_id == LPTrueId || f_id == LPFalseId)
		return f_id;

	const LPFormula * f = get_formula(f_id);
	if (f->is_atomic())
		return f_id;

	bool sign = f_id < 0;
	unsigned int f_idx = absolute(f_id);
	
	// value was already computed
	if (cache[f_idx] != 0) 
		return sign ? -cache[f_idx] : cache[f_idx];

	assert(f->is_boolean_op());

	if (f->is_ite()) {
		LPFormulaId n_c = normalize_formula_aux(f->get_cond(), cache);
		LPFormulaId n_t = normalize_formula_aux(f->get_then(), cache);
		LPFormulaId n_e = normalize_formula_aux(f->get_else(), cache);
		LPFormulaId new_ite = create_ite(n_c, n_t, n_e);
		cache[f_idx] = new_ite;
		return sign ? -new_ite : new_ite;
	}
	else if (f->is_iff()) {
		LPFormulaId n_l = normalize_formula_aux(f->get_iff_lhs(), cache);
		LPFormulaId n_r = normalize_formula_aux(f->get_iff_rhs(), cache);
		LPFormulaId new_iff = create_iff(n_l, n_r);
		cache[f_idx] = new_iff;
		return sign ? -new_iff : new_iff;
	}
	else {
		assert(f->is_or());
		growable_vector<LPFormulaId> n_args; // cannot be static because of the recursive calls
		static queue<LPFormulaId> to_do; // can be static, the variable is dead when the recursive call is performed.
		to_do.reset();
		int num_arguments = f->get_num_arguments();
		for(int i = 0; i < num_arguments; i++)
			to_do.push(f->get_argument(i));
		while (!to_do.is_empty()) {
			LPFormulaId curr = to_do.pop();
			if (curr < 0)
				n_args.push(curr);
			else {
				const LPFormula * curr_f = get_formula(curr);
				if (!curr_f->is_or())
					n_args.push(curr);
				else {
					int num_curr_args = curr_f->get_num_arguments();
					for (int i = 0; i < num_curr_args; i++)
						to_do.push(curr_f->get_argument(i));
				}
			}
		}
		// normalize args
		for (int i = 0; i < n_args.get_size(); i++)
			n_args.set(i, normalize_formula_aux(n_args.get(i), cache));
		// create a new OR
		LPFormulaId new_or = create_or(n_args.get_size(), n_args.get_contents());
		cache[f_idx] = new_or;
		return sign ? -new_or : new_or;
	}
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



