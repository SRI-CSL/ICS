/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Apr 8, 2002: Created.
***/

#ifndef PARSER_H
#define PARSER_H

#include<pair.h>
#include"LPFormula.h"
#include"growable-vector.h"
#include"object-pool.h"
#include"lisp.h"

struct parser_ival {
	int pos;
	int i;
}; /* integer valued tokens */

struct parser_rval {
	int pos;
	double r;
}; /* real values tokens */

struct parser_sval {
	int pos;
	const char * str_id;
}; /* string valued tokens */

struct parser_id {
	int pos;
	const char * name_id;
}; /* identifiers */

struct parser_function_args {
	bool is_associative;
	bool is_commutative;
};

template<class T, unsigned int size>
struct resetGrowableVector {
	void operator()(growable_vector<T, size> * v) { v->reset(); }
};

#define PARSER_DEFAULT_ID_VECTOR_SIZE 4

typedef growable_vector<LPFormulaId, PARSER_DEFAULT_ID_VECTOR_SIZE> FormulaIdVector;
typedef object_pool<FormulaIdVector, resetGrowableVector<LPFormulaId, PARSER_DEFAULT_ID_VECTOR_SIZE> > FormulaIdVectorObjectPool;
typedef growable_vector<LPTermId, PARSER_DEFAULT_ID_VECTOR_SIZE> TermIdVector;
typedef object_pool<TermIdVector, resetGrowableVector<LPTermId, PARSER_DEFAULT_ID_VECTOR_SIZE> > TermIdVectorObjectPool;

typedef enum {PARSER_EXPR_AND, PARSER_EXPR_OR, PARSER_EXPR_OTHER_BOOL, PARSER_EXPR_TERM} parser_expr_kind;

struct parser_expr {
	parser_expr_kind kind;
	union {
		FormulaIdVector * formula_vector;
		unsigned int term_id;
	} data;
};

typedef lisp_list<parser_id> parser_id_list;

class LPFormulaManager;

LPRootFormulaVector * lazy_prover_parse(char * file_name, LPFormulaManager * m);

#endif /* PARSER_H */



