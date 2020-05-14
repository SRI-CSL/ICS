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
     demoura - Apr 11, 2002: Created.
***/

#ifndef ICS_INTERFACE_H
#define ICS_INTERFACE_H

#include<hash_map.h>
#include<vector.h>
#include"LPFormula.h"
#include"growable-vector.h"



// extern "C" {
// void ics_caml_startup();
// #include<ics.h>
// }

typedef hash_set<unsigned int> FormulaIdSet;
typedef hash_map<unsigned int, vector<unsigned int> > FormulaIdMapping;
typedef hash_map<int, int> ICSAtomToFormulaId;

class ICSInterface {
 	LPFormulaManager * formula_manager;

 	double ics_elapsed;
 	unsigned int num_calls;

	bool use_ics_explanations;

	FormulaIdSet formulas;
	FormulaIdMapping associated_formulas;
	ICSAtomToFormulaId atom_to_formula_id;

 public:
 	ICSInterface(LPFormulaManager *, bool ics_explanations);
	~ICSInterface() {}
 	void set_formula(unsigned int f_idx);
	void compute_associated_formulas_info();

 	void push();
 	void pop();
 	void reset();
 	void reset_scratch_state() {
		icsat_reset_scratch_context();
	}
 	bool assert_formula(LPFormulaId f, bool in_scratch = false);

 	void dump_current_state();

 	double get_time() const { return ics_elapsed; }
 	unsigned int get_num_calls() const { return num_calls; }
 	vector<unsigned int> & get_associated_formulas(unsigned int f_idx) { return associated_formulas[f_idx]; }
 	bool assert_formula_in_scratch_state(LPFormulaId f) { return assert_formula(f, true); }
 	void dump_ics_formula(LPFormulaId f_id);
	bool use_ics_explain() { return use_ics_explanations; }
  LPFormulaId get_formula_id_of_atom_id(int atom) { return atom_to_formula_id[atom]; }

	bool is_explained();
	pair<int *,int> explain();

};


#endif /* ICS_INTERFACE_H */
