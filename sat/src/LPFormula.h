/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 18, 2002: Created.
***/

#ifndef LPFORMULA_H
#define LPFORMULA_H

#include "ics_interface.h"

typedef int LPFormulaId;

typedef enum {LP_OR, LP_IFF, LP_ITE, LP_EXISTS, LP_PROPOSITION, LP_EQ} LPFormulaKind;

// wrapper for ICS prop formulas
class LPFormula {
	value * prop;
	// argument cache... the previous interface was array based, instead of list based.
	unsigned int num_arguments;
	value ** arguments; 
public:
	LPFormula(value * p) {
		prop = p;
		if (ics_prop_is_disj 

	bool is_iff() const { return ics_prop_is_iff(prop); }
	bool is_ite() const { return ics_prop_is_ite(prop); }
	bool is_or() const { return ics_prop_is_disj(prop); }
	bool is_exists() const { UNREACHABLE_CODE; }
	bool is_eq() const { return !ics_prop_is_var(prop) && ics_prop_is_atom(prop); }
	bool is_proposition() const { return ics_prop_is_var(prop); }
	bool is_atomic() const { return ics_prop_is_atom(prop); }
	bool is_boolean_op() const { return is_or() || is_iff() || is_ite(); }
	unsigned int get_num_arguments() const { assert(is_boolean_op()); return data.boolean_op.num_arguments; }
	const LPFormulaId * get_arguments() const { assert(is_boolean_op()); return data.boolean_op.arguments; }
	
};

#include<vector.h>
#include<hash_map.h>
#include<hash_set.h>

class LPFormulaManager {
	





#endif /* LPFORMULA_H */
