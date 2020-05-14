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



