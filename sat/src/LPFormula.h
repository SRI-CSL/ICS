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

#ifndef LPFORMULA_H
#define LPFORMULA_H

#include<iostream.h>
#include<vector.h>
#include<hash_map.h>
#include<hash_set.h>
#include"sort.h" 
#include"error.h"
#include"icsat.h"
#include"consts.h"
#include"util.h"
#include"UString.h"
typedef enum {LP_OR, LP_IFF, LP_ITE, LP_EXISTS, LP_PROPOSITION, LP_EQ} LPFormulaKind;
typedef int LPFormulaId;

struct LPBooleanOpFields {
	unsigned int num_arguments;
	LPFormulaId * arguments;
};

struct LPEqFields {
	int atom;
	int not_atom;
};

class LPFormulaIdEqualTo;
class LPFormulaIdHashFcn;
class LPFormulaManager;

class LPFormula {
protected:
	LPFormulaKind kind;
	union {
		LPBooleanOpFields boolean_op;
		LPEqFields eq;
		const char * prop_var_name;
	} data;
	friend struct LPFormulaIdEqualTo;
	friend struct LPFormulaIdHashFcn;
	friend class LPFormulaManager;
public:
	LPFormulaKind get_kind() const { return kind; }
	bool is_iff() const { return kind == LP_IFF; }
	bool is_ite() const { return kind == LP_ITE; }
	bool is_or() const { return kind == LP_OR; }
	// bool is_exists() const { return kind == LP_EXISTS; }
	bool is_eq() const { return kind == LP_EQ; }
	bool is_proposition() const { return kind == LP_PROPOSITION; }
	bool is_atomic() const { return is_eq() || is_proposition(); }
	bool is_boolean_op() const { return is_or() || is_iff() || is_ite(); }
	unsigned int get_num_arguments() const { assert(is_boolean_op()); return data.boolean_op.num_arguments; }
	const LPFormulaId * get_arguments() const { assert(is_boolean_op()); return data.boolean_op.arguments; }
	LPFormulaId get_argument(unsigned int arg_idx) const {
		assert(is_boolean_op()); assert(arg_idx < get_num_arguments()); return data.boolean_op.arguments[arg_idx]; 
	}
	LPFormulaId get_iff_lhs() const { assert(is_iff()); return get_argument(0); }
	LPFormulaId get_iff_rhs() const { assert(is_iff()); return get_argument(1); }
	int get_atom() const { assert(is_eq()); return (data.eq.atom); }
	int get_not_atom() const { assert(is_eq()); return (data.eq.not_atom); }
	const char * get_propositional_variable_name() const { assert(is_proposition()); return data.prop_var_name; }
	LPFormulaId get_cond() const { assert(is_ite()); return get_argument(0); }
	LPFormulaId get_then() const { assert(is_ite()); return get_argument(1); }
	LPFormulaId get_else() const { assert(is_ite()); return get_argument(2); }
};

inline bool operator==(const LPFormula & f1, const LPFormula & f2) {
	return f1.get_kind() == f2.get_kind() &&
		IMPLY(f1.is_boolean_op(), f1.get_num_arguments() == f2.get_num_arguments() && f1.get_arguments() == f2.get_arguments()) &&
		IMPLY(f1.is_eq(), f1.get_atom() == f2.get_atom() && f1.get_not_atom() == f2.get_not_atom()) &&
		IMPLY(f1.is_proposition(), f1.get_propositional_variable_name() == f2.get_propositional_variable_name());
}

class LPFormulaIdEqualTo
{
	LPFormulaManager * manager;
public:
	LPFormulaIdEqualTo(LPFormulaManager * m) { manager = m; }
	bool operator()(LPFormulaId obj1, LPFormulaId obj2) const;
};

class LPFormulaIdHashFcn
{
	LPFormulaManager * manager;
public:
	LPFormulaIdHashFcn(LPFormulaManager * m) { manager = m; }
	size_t operator()(LPFormulaId obj) const;
};

typedef hash_set<LPFormulaId, LPFormulaIdHashFcn, LPFormulaIdEqualTo> LPFormulaPool; 

inline bool formula_lt(LPFormulaId arg1, LPFormulaId arg2) { return absolute(arg1) < absolute(arg2); }

struct LPFormulaIdLt {
	bool operator()(LPFormulaId arg1, LPFormulaId arg2) { return formula_lt(arg1, arg2); }
};

typedef sortFcn<LPFormulaId, LPFormulaIdLt> sortLPFormulaId;

const LPFormulaId LPNullFormulaId = 0;
const LPFormulaId LPTrueId = 1;
const LPFormulaId LPFalseId = -1;

class LPFormulaManager {
	LPFormulaIdEqualTo equal_fcn;
	LPFormulaIdHashFcn hash_fcn;
	LPFormula * formulas;
	LPFormulaId * arguments;
	// LPTermId * variables;
	LPFormulaPool formula_pool;
	unsigned int num_formulas;
	unsigned int next_formula_id;
	unsigned int num_arguments;
	unsigned int next_argument_pos;
	// unsigned int num_variables;
	// unsigned int next_variable_pos;

	void expand_formula_array() { 
		MEM_TRACE(ctrace << "[memory] expanding formula array, num_formulas = " << num_formulas << endl;);
		expand_array(formulas, num_formulas); 
		MEM_TRACE(ctrace << "         new num_formulas = " << num_formulas << endl;);
	}
	void expand_argument_array() { 
		MEM_TRACE(ctrace << "[memory] expanding argument array, num_arguments = " << num_arguments << endl;);
		// cout << "[memory] expanding argument array, num_arguments = " << num_arguments << endl;
		LPFormulaId * old = arguments;
		expand_array(arguments, num_arguments); 
		int delta = arguments - old;
		MEM_TRACE(ctrace << "         delta = " << delta << endl;);
		for (unsigned int i = 1; i < next_formula_id; i++)
			if (formulas[i].is_boolean_op())
				formulas[i].data.boolean_op.arguments += delta;
		MEM_TRACE(ctrace << "         new num_arguments = " << num_arguments << endl;);
	}

	friend ostream& operator<<(ostream& target, const LPFormulaManager & m);

	bool create_space_for_new_formula() {
		if (next_formula_id >= num_formulas) {
			expand_formula_array();
			return true;
		}
		return false;
	}
	
	bool create_space_for_new_arguments(unsigned int num_args) {
		bool expanded = false;
		while (next_argument_pos + num_args >= num_arguments) {
			expand_argument_array();
			expanded = true;
		}
		return expanded;
	}
	
public:
	LPFormulaManager(unsigned int init_num_formulas = INITIAL_NUMBER_OF_FORMULAS,
									 unsigned int init_num_args = INITIAL_NUMBER_OF_ARGUMENTS,
									 unsigned int init_pool_size = INITIAL_FORMULA_POOL_SIZE);
	~LPFormulaManager();

	const LPFormula * get_formula(LPFormulaId id) const { assert(id != 0); return &(formulas[id > 0 ? id : -id]); }
	bool is_positive(LPFormulaId id) const { assert(id != 0); return id > 0; }
	bool is_negative(LPFormulaId id) const { return ! is_positive(id); }

	void dump_formula(ostream & target, const LPFormula * f, bool negated = false) const ;
	void dump_formula(ostream & target, LPFormulaId f) const { dump_formula(target, get_formula(f), f < 0); }

	unsigned int get_num_formulas() const { return next_formula_id - 1; }
	unsigned int get_curr_formula_capacity() const { return num_formulas - 1; }
	unsigned int get_formula_id(const LPFormula * f) const { return f - formulas; }
	unsigned int get_num_atoms() const;

	LPFormulaId create_proposition(char * name);
	LPFormulaId create_or_aux(unsigned int num_args, LPFormulaId * arguments);
	LPFormulaId create_or(unsigned int num_args, LPFormulaId * arguments);
	LPFormulaId create_or(LPFormulaId arg1, LPFormulaId arg2);
	LPFormulaId create_nor(unsigned int num_args, LPFormulaId * arguments) { return -create_or(num_args, arguments); }
	LPFormulaId create_nor(LPFormulaId arg1, LPFormulaId arg2) { return -create_or(arg1, arg2); }
	LPFormulaId create_and(unsigned int num_args, LPFormulaId * arguments);
	LPFormulaId create_and(LPFormulaId arg1, LPFormulaId arg2) { return -create_or(-arg1, -arg2); }
	LPFormulaId create_nand(unsigned int num_args, LPFormulaId * arguments) { return -create_and(num_args, arguments); }
	LPFormulaId create_nand(LPFormulaId arg1, LPFormulaId arg2) { return -create_and(arg1, arg2); }
	LPFormulaId create_implies(LPFormulaId lhs, LPFormulaId rhs) { return create_or(-lhs, rhs); }
	LPFormulaId create_xor(LPFormulaId arg1, LPFormulaId arg2) { return -create_iff(-arg1, -arg2); }
	LPFormulaId create_iff(LPFormulaId lhs, LPFormulaId rhs);
	LPFormulaId create_ite(LPFormulaId c, LPFormulaId t, LPFormulaId e);
	LPFormulaId create_not(LPFormulaId f) { return -f; }
	LPFormulaId create_atom(int a, int not_a);

	void dump_mem_info();

	LPFormulaId normalize_formula(LPFormulaId f_id);
	
private:
	LPFormulaId normalize_formula_aux(LPFormulaId f_id, int * cache);
};

ostream& operator<<(ostream& target, const LPFormulaManager & m);

extern LPFormulaManager * sat_formula_manager;

#endif /* LPFORMULA_H */
