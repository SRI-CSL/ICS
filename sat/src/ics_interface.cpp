/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 11, 2002: Created.
***/
#include "ics_interface.h"
#include <time.h>

#define ICS_VERBOSE_LEVEL 0

ICSInterface::ICSInterface(LPFormulaManager * manager)
{
	formula_manager = manager;
	ics_elapsed = 0.0;
	num_calls = 0;
}

void ICSInterface::reset()
{
	icsat_reset();
}

void ICSInterface::push()
{
	icsat_push();
}

void ICSInterface::pop()
{
	icsat_pop();
}

void ICSInterface::set_formula(unsigned int f_idx)
{
	formulas.insert(f_idx);
}

void ICSInterface::compute_associated_formulas_info()
{
	hash_set<unsigned int>::const_iterator it1 = formulas.begin();
	while (it1 != formulas.end()) {
		unsigned int f1_idx = *it1;
		hash_set<unsigned int>::const_iterator it2 = formulas.begin();
		while (it2 != formulas.end()) {
			unsigned int f2_idx = *it2;
			if (f1_idx != f2_idx) {
				value f1_value = formula_manager->get_formula(f1_idx)->get_atom();
				value f2_value = formula_manager->get_formula(f2_idx)->get_atom();
				if (icsat_atoms_connected(f1_value, f2_value))
					associated_formulas[f1_idx].push_back(f2_idx);
			}
			it2++;
		}
		it1++;
	}
}

bool ICSInterface::assert_formula(LPFormulaId f, bool in_scratch) {
// 	ICS_TRACE(ctrace << "[ics] asserting formula " << f << " : ";
// 						formula_manager->dump_formula(ctrace, f);
// 						ctrace << endl;);
	num_calls++;
	value f_v = f < 0 ? formula_manager->get_formula(f)->get_atom() : formula_manager->get_formula(f)->get_not_atom();

	bool result;
		
	clock_t start = clock();
	if (in_scratch)
		result = icsat_assert(f_v) == 1;
	else
		result = icsat_assert_in_scratch_context(f_v) == 1;
	clock_t end = clock();

	ics_elapsed += ((double) (end - start)) / CLOCKS_PER_SEC;
	return result;
}

void ICSInterface::dump_current_state() 
{
	// value * curr_state = ics_current_state();
	// ics_context_pp(curr_state);
	// cout << "CONSTRAINTS: " << endl;
	// ics_cnstrnt_of_pp(curr_state);
	cout << "TO DO (dump_current_state): print the ICS current state\n";
}

void ICSInterface::dump_ics_formula(LPFormulaId f_id)
{
	cout << "TO DO (dump_ics_formula): print the ICS atom\n";
}

