/* Copyright (c) SRI International 2002. */
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

class ICSInterface {
 	LPFormulaManager * formula_manager;

 	double ics_elapsed;
 	unsigned int num_calls;

	FormulaIdSet formulas;
	FormulaIdMapping associated_formulas;

 public:
 	ICSInterface(LPFormulaManager *);
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
};


#endif /* ICS_INTERFACE_H */
