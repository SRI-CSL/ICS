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

extern double SAT_associated_formulas_time;

void ICSInterface::compute_associated_formulas_info()
{
	clock_t start = clock();
	FormulaIdSet::const_iterator it1 = formulas.begin();
	for (;it1 != formulas.end(); it1++) {
		unsigned int f1_idx = *it1;
		FormulaIdSet::const_iterator it2 = formulas.begin();
		for (;it2 != formulas.end(); it2++) {
			unsigned int f2_idx = *it2;
			if (f1_idx != f2_idx) {
				int f1_value = formula_manager->get_formula(f1_idx)->get_atom();
				int f2_value = formula_manager->get_formula(f2_idx)->get_atom();
				if (icsat_is_connected(f1_value, f2_value))
					associated_formulas[f1_idx].push_back(f2_idx);
			}
		}
	}
	
	DBG_CODE(FormulaIdMapping::const_iterator it = associated_formulas.begin();
					 for (;it != associated_formulas.end(); it++) {
						 unsigned int curr_idx = (*it).first;
						 const vector<unsigned int> & curr_assoc = (*it).second;
						 vector<unsigned int>::const_iterator it2 = curr_assoc.begin();
						 cout << "Associated formulas  of " << curr_idx << ":\n";
						 for (; it2 != curr_assoc.end(); it2++) {
							 cout << *it2 << " ";
						 }
						 cout << endl;
					 });

	clock_t end = clock();
	SAT_associated_formulas_time = ((double) (end - start)) / CLOCKS_PER_SEC;		
}

bool ICSInterface::assert_formula(LPFormulaId f, bool in_scratch) {
	num_calls++;
	int f_v = f > 0 ? formula_manager->get_formula(f)->get_atom() : formula_manager->get_formula(f)->get_not_atom();

 	DBG_CODE(cout << endl << "[ics] asserting formula " << f << ", ID = " << f_v << endl;);

	bool result;
		
	clock_t start = clock();
	if (in_scratch)
		result = icsat_assert(f_v);
	else
		result = icsat_add_scratch_context(f_v);
	clock_t end = clock();

	ics_elapsed += ((double) (end - start)) / CLOCKS_PER_SEC;
 	DBG_CODE(cout << "   result is: " << result << endl;);
	return result == 1;
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

