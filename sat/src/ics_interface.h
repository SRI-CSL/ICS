/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 11, 2002: Created.
***/

#ifndef ICS_INTERFACE_H
#define ICS_INTERFACE_H

extern "C" {
	typedef long value;
	
	int ics_is_nil(value* x1);
	value* ics_head(value* x1);
	value* ics_tail(value* x1);
	
	value* ics_pair(value* x1, value* x2);
	value* ics_fst(value* x1);
	value* ics_snd(value* x1);

	value* ics_triple(value* x1, value* x2, value* x3);
	value* ics_fst_of_triple(value* x1);
	value* ics_snd_of_triple(value* x1);
	value* ics_third_of_triple(value* x1);

	int ics_prop_is_true(value * x1);
	int ics_prop_is_false(value * x1);
	int ics_prop_is_atom(value * x1);
	int ics_prop_is_var(value * x1); // needed
	int ics_prop_is_ite(value * x1);
	int ics_prop_is_conj(value * x1);
	int ics_prop_is_disj(value * x1);
	int ics_prop_is_iff(value * x1);
	int ics_prop_is_neg(value * x1);

	// val prop_d_atom : prop -> Atom.t
	value * ics_prop_d_atom(value * x1);
	// val prop_d_ite : prop -> prop * prop * prop
	value * ics_prop_d_ite(value * x1);
	// val prop_d_disj : prop -> prop list
	value * ics_prop_d_disj(value * x1);
	// val prop_d_iff : prop -> prop * prop
	value * ics_prop_d_iff(value * x1);
	// val prop_d_neg : prop -> prop
	value * ics_prop_d_neg(value * x1);

	// val atom_is_negatable : atom -> bool
	int ics_atom_is_negatable(value * x1);
	// val atom_negate : atom -> atom
	value * ics_atom_negate(value * x1);

	// val atoms_empty : unit -> atoms
	value * ics_atoms_empty();
	// val atoms_singleton : atom -> atoms
	value * ics_atoms_singleton(value * x1);
	// val atoms_add : atom -> atoms -> atoms
	value * ics_atoms_add(value * x1, value * x2); 
	// val atoms_to_list : atoms -> atom list
	value * ics_atoms_to_list(value * x1);

	
	value * ics_context_empty();
	
	// process : context -> atom -> status 
	value * ics_process(value * x1, value * x2);
	// is_consistent : status -> bool
	int ics_is_consistent(value * x1);
	// is_redundant : status -> bool 
	int ics_is_redundant(value * x1);
	// is_inconsistent : status -> bool
	int ics_is_inconsistent(value * x1);

	
}



// #include<hash_map.h>
// #include<vector.h>
// #include"LPFormula.h"

// extern "C" {
// void ics_caml_startup();
// #include<ics.h>
// }

class ICSInterface {
// 	LPFormulaManager * formula_manager;
// 	growable_vector<value *> to_ics_formula; // mapping from LPFormula to ICS formula 
// 	growable_vector<value *> to_ics_neg_formula; // negation of the previous one
// 	growable_vector<value *> to_ics_term;
// 	growable_vector<value *> ics_state_stack;

// 	unsigned int lp_true;
// 	unsigned int lp_false;
// 	value * int_cnstrnt;

// 	value * ics_current_state() const { return ics_state_stack.top(); }
// 	value * convert_to_ics_term(LPTermId t);
// 	value * convert_to_ics_list(unsigned int n, const LPTermId * args);
// 	bool is_predicate(const LPTerm * term) const;
// 	value * create_ics_predicate_negation(LPTermId t);

// 	double ics_elapsed;
// 	unsigned int num_calls;

// 	growable_vector<bool> marks;
	
// 	growable_vector<unsigned int> & collect_constants(unsigned int lhs, unsigned int rhs);

// 	hash_map<unsigned int, vector<unsigned int> > constant_to_formulas_mapping;
// 	hash_map<unsigned int, vector<unsigned int> > associated_formulas;

// 	value * scratch_state;

// 	void add_int_cnstrnt(value * term);

 public:
 	ICSInterface();
 	void push();
 	void pop();
 	void reset();
 	void reset_scratch_state();
// 	void set_formula(unsigned int f_idx);
// 	bool assert_formula(LPFormulaId f, bool in_scratch = false);
// 	void dump_current_state();
// 	double get_time() const { return ics_elapsed; }
// 	unsigned int get_num_calls() const { return num_calls; }
// 	vector<unsigned int> & get_associated_formulas(unsigned int f_idx) { return associated_formulas[f_idx]; }
// 	bool assert_formula_in_scratch_state(LPFormulaId f) { return assert_formula(f, true); }
// 	void dump_ics_formula(LPFormulaId f_id);

};

#endif /* ICS_INTERFACE_H */
