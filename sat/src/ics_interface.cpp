/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 11, 2002: Created.
***/
#include "ics_interface.h"
#include <time.h>

extern "C" {
void ics_deregister(value* r); 
}

#define ICS_VERBOSE_LEVEL 0

void ICSInterface::initialize(char **argv)
{
	ics_caml_startup(1, argv);
	// ics_caml_startup();
	// ics_init(ICS_VERBOSE_LEVEL);
	ICS_TRACE(ctrace << "[ics] initialized.\n";);
}

ICSInterface::ICSInterface(LPFormulaManager * manager)
{
	formula_manager = manager;
	ics_state_stack.push(ics_context_empty());
	lp_true = formula_manager->create_constant(TRUE_CONSTANT);
	lp_false = formula_manager->create_constant(FALSE_CONSTANT);
	value * t = ics_term_mk_true();
	value * f = ics_term_mk_false();
	to_ics_term.set_x(lp_true, t);
	to_ics_term.set_x(lp_false, f);
	ics_elapsed = 0.0;
	num_calls = 0;
	scratch_state = ics_context_empty();
	int_cnstrnt = ics_cnstrnt_of_string("int");
}

void ICSInterface::reset_scratch_state()
{
	scratch_state = ics_context_empty();
}

void ICSInterface::reset()
{
	ics_state_stack.reset();
	ics_state_stack.push(ics_context_empty());
	ics_elapsed = 0.0;
	num_calls = 0;
}

void ICSInterface::push()
{
	ICS_TRACE(ctrace << "[ics] pushing state\n";);
	ics_state_stack.push(ics_state_stack.top());
}

void ICSInterface::pop()
{
	ICS_TRACE(ctrace << "[ics] poping state\n";);
	value * s = ics_state_stack.pop();
	if (ics_state_stack.is_empty() || s != ics_state_stack.top()) {
		ics_deregister(s);
		free(s);
	}
}

bool ICSInterface::is_predicate(const LPTerm * term) const
{
	return term->is_application() && (term->get_op() == formula_manager->get_lt() ||
																		term->get_op() == formula_manager->get_le() ||
																		term->get_op() == formula_manager->get_gt() ||
																		term->get_op() == formula_manager->get_ge());
}

void ICSInterface::set_formula(unsigned int f_idx)
{
	if (f_idx >= to_ics_formula.get_size()) {
		to_ics_formula.resize(f_idx+1);
		to_ics_neg_formula.resize(f_idx+1);
	}
	if (to_ics_formula.get(f_idx) != NULL)
		return;
	const LPFormula * formula = formula_manager->get_formula(f_idx);
	assert(formula->get_kind() == LP_EQ);
	LPTermId lhs = formula->get_eq_lhs();
	LPTermId rhs = formula->get_eq_rhs();

	growable_vector<unsigned int> & found_constants = collect_constants(lhs, rhs);
	unsigned int n = found_constants.get_size();
	// update the associated_formulas mapping
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = found_constants.get(i);
		vector<unsigned int> & const_occurrences = constant_to_formulas_mapping[var_idx];
		vector<unsigned int>::const_iterator it = const_occurrences.begin();
		for (; it != const_occurrences.end(); it++) {
			unsigned int assoc_formula_idx = *it;
			if (marks.get_size() <= assoc_formula_idx || !marks.get(assoc_formula_idx)) {
				marks.set_x(assoc_formula_idx, true);
				associated_formulas[assoc_formula_idx].push_back(f_idx);
				associated_formulas[f_idx].push_back(assoc_formula_idx);
			}
		}
	}

	vector<unsigned int> & assoc_formulas = associated_formulas[f_idx];
	vector<unsigned int>::const_iterator it2 = assoc_formulas.begin();
	for (; it2 != assoc_formulas.end(); it2++) {
		marks.set_x(*it2, false);
	}

	ICS_TRACE(ctrace << "Associated formulas of [";
						formula_manager->dump_formula(ctrace, f_idx);
						ctrace << "]\n";
						vector<unsigned int> & assoc_formulas = associated_formulas[f_idx];
						vector<unsigned int>::const_iterator it2 = assoc_formulas.begin();
						for (; it2 != assoc_formulas.end(); it2++) {
							ctrace << "  ";
							formula_manager->dump_formula(ctrace, *it2);
							ctrace << endl;
						});

	// fill the constant_to_formulas_mapping
	for (unsigned int i = 0; i < n; i++) {
		unsigned int var_idx = found_constants.get(i);
		constant_to_formulas_mapping[var_idx].push_back(f_idx);
	}

	
	value * f = NULL;
	value * not_f = NULL;
	
	const LPTerm * lhs_term = formula_manager->get_term(lhs);
	const LPTerm * rhs_term = formula_manager->get_term(rhs);
	
	if (is_predicate(lhs_term)) {
		assert(rhs == lp_true || rhs == lp_false);
		value * ics_lhs = convert_to_ics_term(lhs);
		value * not_ics_lhs = create_ics_predicate_negation(lhs);
		if (rhs == lp_true) {
			f = ics_lhs;
			not_f = not_ics_lhs;
		}		
		else {
			f = not_ics_lhs;
			not_f = ics_lhs;
		}
	}
	else if (is_predicate(rhs_term)) {
		assert(lhs == lp_true || lhs == lp_false);
		value * ics_rhs = convert_to_ics_term(rhs);
		value * not_ics_rhs = create_ics_predicate_negation(rhs);
		if (lhs == lp_true) {
			f = ics_rhs;
			not_f = not_ics_rhs;
		}		
		else {
			f = not_ics_rhs;
			not_f = ics_rhs;
		}
	}
	else {
		value * ics_lhs = convert_to_ics_term(lhs);
		value * ics_rhs = convert_to_ics_term(rhs);
		f = ics_atom_mk_equal(ics_lhs, ics_rhs);
		not_f = ics_atom_mk_diseq(ics_lhs, ics_rhs);
	}
	
	ICS_TRACE(ctrace << "Sending to ICS formula: " << f_idx << " ";
						formula_manager->dump_formula(ctrace, f_idx);
						ctrace << endl;
						);

	to_ics_formula.set(f_idx, f);
	to_ics_neg_formula.set(f_idx, not_f);
}

value * ICSInterface::create_ics_predicate_negation(LPTermId t)
{
	const LPTerm * term = formula_manager->get_term(t);
	assert(term->is_application());

	if (term->get_op() == formula_manager->get_lt()) {
		return ics_atom_mk_ge(convert_to_ics_term(term->get_argument(0)), 
													convert_to_ics_term(term->get_argument(1)));
	}
	else if (term->get_op() == formula_manager->get_le()) {
		return ics_atom_mk_gt(convert_to_ics_term(term->get_argument(0)), 
													convert_to_ics_term(term->get_argument(1)));
	}
	else if (term->get_op() == formula_manager->get_gt()) {
		return ics_atom_mk_le(convert_to_ics_term(term->get_argument(0)), 
													convert_to_ics_term(term->get_argument(1)));
	}
	else if (term->get_op() == formula_manager->get_ge()) {
		return ics_atom_mk_lt(convert_to_ics_term(term->get_argument(0)), 
													convert_to_ics_term(term->get_argument(1)));
	}
	else {
		assert(false);
		return NULL;
	}
}

void ICSInterface::add_int_cnstrnt(value * term)
{
	value * aux_atom = ics_atom_mk_in(int_cnstrnt, term);
	// ics_atom_pp(aux_atom); cout << endl;
	value * curr_state = ics_state_stack.top();
	value * status = ics_process(curr_state, aux_atom);
	if (ics_is_consistent(status)) {
		value * new_state = ics_d_consistent(status);
		value * old_state = ics_state_stack.pop();
		if (ics_state_stack.is_empty() || old_state != ics_state_stack.top()) {
			ics_deregister(old_state);
			free(old_state);
		}
		ics_state_stack.push(new_state); // update the top of the stack
	}
	else if (ics_is_redundant(status)) {
		// do nothing
	}
	else {
		assert(ics_is_inconsistent(status));
		cout << "Internal error\n";
		exit(-1);
	}
	ics_deregister(status);
	free(status);
}

value * ICSInterface::convert_to_ics_term(LPTermId t) 
{
	const LPTerm * term = formula_manager->get_term(t);

	if (t >= to_ics_term.get_size())
		to_ics_term.resize(t+1);
	// term is already in the mapping...
	if (to_ics_term.get(t) != NULL) {
		value * result = to_ics_term.get(t);
		return result;
	}
	// create term
	value * result = NULL;
	switch (term->get_kind()) {
	case LP_APP: {
		if (term->get_num_arguments() == 0) {
			// constant
			const char * c_name = term->get_op()->get_name();
			result = ics_term_mk_var((char *)c_name);
			// Hack to support integers... 
			// cout << ">>> " << term->get_op()->get_sort()->get_kind() << endl;
			if (term->get_op()->get_sort()->get_kind() == LPIntegerNumberSort::kind)
				add_int_cnstrnt(result);
		}
		else if (term->get_op() == formula_manager->get_add()) {
			value * term_list = convert_to_ics_list(term->get_num_arguments(), term->get_arguments());
			result = ics_term_mk_addl(term_list);
		}
		else if (term->get_op() == formula_manager->get_sub()) {
			assert(term->get_num_arguments() == 2);
			result = ics_term_mk_sub(convert_to_ics_term(term->get_argument(0)), 
													convert_to_ics_term(term->get_argument(1)));
		}
		else if (term->get_op() == formula_manager->get_mul()) {
			value * term_list = convert_to_ics_list(term->get_num_arguments(), term->get_arguments());
			result = ics_term_mk_multl(term_list);

			// Harald removed ics_term_mk_multl from the API... :-(
// 			if (term->get_num_arguments() != 2) {
// 				cerr << "formula not supported...\n";
// 				exit(-1);
// 			}
// 			LPTermId arg_id1 = term->get_argument(0);
// 			LPTermId arg_id2 = term->get_argument(1);
// 			const LPTerm * arg1 = formula_manager->get_term(arg_id1);
// 			const LPTerm * arg2 = formula_manager->get_term(arg_id2);
// 			if (arg1->is_fix_number()) {
// 				int num1 = arg1->get_fix_number();
// 				value * n1 = ics_num_of_int(num1);
// 				result = ics_term_mk_multq(n1, convert_to_ics_term(arg_id2));
// 			}
// 			else if (arg2->is_fix_number()) {
// 				int num2 = arg2->get_fix_number();
// 				value * n2 = ics_num_of_int(num2);
// 				result = ics_term_mk_multq(n2, convert_to_ics_term(arg_id1));
// 			}
// 			else {
// 				cerr << "Only constant multiplication is supported...\n";
// 				exit(-1);
// 			}
		}
		else if (term->get_op() == formula_manager->get_div()) {
			feature_not_implemented_yet();
			result = NULL;
		}
		else if (term->get_op() == formula_manager->get_lt()) {
			result = ics_atom_mk_lt(convert_to_ics_term(term->get_argument(0)), 
															convert_to_ics_term(term->get_argument(1)));
		}
		else if (term->get_op() == formula_manager->get_le()) {
			result = ics_atom_mk_le(convert_to_ics_term(term->get_argument(0)), 
															convert_to_ics_term(term->get_argument(1)));
		}
		else if (term->get_op() == formula_manager->get_gt()) {
			result = ics_atom_mk_gt(convert_to_ics_term(term->get_argument(0)), 
															convert_to_ics_term(term->get_argument(1)));
		}
		else if (term->get_op() == formula_manager->get_ge()) {
			result = ics_atom_mk_ge(convert_to_ics_term(term->get_argument(0)), 
															convert_to_ics_term(term->get_argument(1)));
		}
		else {
			/* uninterpreted function symbol */
			value * term_list = convert_to_ics_list(term->get_num_arguments(), term->get_arguments());
			const char * c_name = term->get_op()->get_name();
			result = ics_term_mk_uninterp((char *) c_name, term_list);
			// Hack to support integers... if the given function return an integer...
			// then I should send this constraint to ICS
			if (term->get_op()->get_sort()->get_kind() == LPIntegerNumberSort::kind) 
				add_int_cnstrnt(result);
		}
		break;
	}
	case LP_FIXNUM: {
		int i = term->get_fix_number();
		value * n = ics_num_of_int(i);
		result = ics_term_mk_num(n);
		break;
	}
	case LP_VARIABLE:
	case LP_NUM:
		feature_not_implemented_yet();
	default:
		assert(false);
	}
	assert(result != NULL);
	to_ics_term.set(t, result); // cache the result

	return result;
}

value * ICSInterface::convert_to_ics_list(unsigned int n, const LPTermId * args)
{
	value * result = ics_nil();
	for (int i = n; --i >= 0;) {
		result = ics_cons(convert_to_ics_term(args[i]), result);
	}
	return result;
}

bool ICSInterface::assert_formula(LPFormulaId f, bool in_scratch) {
	ICS_TRACE(ctrace << "[ics] asserting formula " << f << " : ";
						formula_manager->dump_formula(ctrace, f);
						ctrace << endl;);
	num_calls++;
	value * curr_state = in_scratch ? scratch_state : ics_state_stack.top();
	value * formula = f < 0 ? to_ics_neg_formula.get(-f) : to_ics_formula.get(f);
	assert(formula != NULL);
	clock_t start = clock();
	value * status = ics_process(curr_state, formula);
	clock_t end = clock();
	ics_elapsed += ((double) (end - start)) / CLOCKS_PER_SEC;
	bool result;
	if (ics_is_consistent(status)) {
		ICS_TRACE(ctrace << "  consistent\n";);
		value * new_state = ics_d_consistent(status);
		if (in_scratch) { 
			ics_deregister(scratch_state);
			free(scratch_state);
			scratch_state = new_state;
		}
		else {
			value * old_state = ics_state_stack.pop();
			if (ics_state_stack.is_empty() || old_state != ics_state_stack.top()) {
				ics_deregister(old_state);
				free(old_state);
			}
			ics_state_stack.push(new_state); // update the top of the stack
		}
		result = true;
	}
	else if (ics_is_redundant(status)) {
		ICS_TRACE(ctrace << "  redundant\n";);
		result = true;
	}
	else {
		assert(ics_is_inconsistent(status));
		ICS_TRACE(ctrace << "  inconsistent\n";);
		result = false;
	}
	ics_deregister(status);
	free(status);
	return result;
}

void ICSInterface::dump_current_state() 
{
	value * curr_state = ics_current_state();
	ics_context_pp(curr_state);
	// cout << "CONSTRAINTS: " << endl;
	// ics_cnstrnt_of_pp(curr_state);
	cout << endl;
	ics_deregister(curr_state);
	free(curr_state);
}

growable_vector<unsigned int> & ICSInterface::collect_constants(unsigned int lhs, unsigned int rhs)
{
	static growable_vector<unsigned int> found_constants;
	static growable_vector<unsigned int> to_do;
	found_constants.reset();
	to_do.reset();
	unsigned int head = 0;
	unsigned int tail = 0;
	to_do.set_x(tail++, lhs);	
	to_do.set_x(tail++, rhs);	
	
	while(head != tail) {
		unsigned int curr_idx = to_do.get(head++);
		if (marks.get_size() <= curr_idx || !marks.get(curr_idx)) {
			marks.set_x(curr_idx, true);
			const LPTerm * term = formula_manager->get_term(curr_idx);
			switch (term->get_kind()) {
			case LP_APP: {
				unsigned int n = term->get_num_arguments();
				if (n == 0) {
					if (curr_idx != lp_true && curr_idx != lp_false) { // small hack...
						// constant...
						found_constants.push(curr_idx);
					}
				}
				else {
					for (unsigned int i = 0; i < n; i++) {
						unsigned int child_idx = term->get_argument(i);
						to_do.set_x(tail++, child_idx);
					}
				}
				break;
			}
			case LP_VARIABLE:
			case LP_FIXNUM: 
			case LP_NUM:
				break; // do nothing
			default:
				assert(false);
			}
		}
	}

	ICS_TRACE(ctrace << "Terms [";
						formula_manager->dump_term(ctrace, lhs);
						ctrace << "], [";
						formula_manager->dump_term(ctrace, rhs);
						ctrace << "] contains the variables:\n";
						ctrace << " ";
						unsigned int n = found_constants.get_size();
						for (unsigned int i = 0; i < n; i++) {
							ctrace << " ";
							formula_manager->dump_term(ctrace, found_constants.get(i));
						}
						ctrace << endl;);

	// reset marks...
	for (unsigned int i = 0; i < tail; i++) {
		unsigned int curr_idx = to_do.get(i);
		marks.set_x(curr_idx, false);
	}

	return found_constants;
}

void ICSInterface::dump_ics_formula(LPFormulaId f_id)
{
	if (f_id < 0)
		ics_atom_pp(to_ics_neg_formula.get(-f_id));
	else
		ics_atom_pp(to_ics_formula.get(f_id));
}


extern "C" {

void ics_error(char * funname, char * message) {
	cerr << "ICS error at " << funname << " : " << message << endl;
	exit(1);
}

}
