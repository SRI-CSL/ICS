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
     demoura - Mar 6, 2002: Created.
***/

#ifndef CONSTS_H
#define CONSTS_H

#ifdef NDEBUG

#define MAX_STR_CONST 1024
#define DEFAULT_FORMULA_LIST_POOL_BLOCK_SIZE 8192
#define DEFAULT_TERM_LIST_POOL_BLOCK_SIZE 8192
#define INITIAL_FORMULA_VECTOR_SIZE 4
#define INITIAL_TERM_VECTOR_SIZE 4

#define INITIAL_NUMBER_OF_TERMS 131072
#define INITIAL_NUMBER_OF_ARGUMENTS 131072
#define INITIAL_TERM_POOL_SIZE 131072

#define INITIAL_NUMBER_OF_FORMULAS 131072
#define INITIAL_FORMULA_POOL_SIZE 131072

#define PARSER_INITIAL_MEMORY 32768

#define DEFAULT_SYMBOL_TABLE_SIZE 32768

#define INITIAL_NUMBER_OF_CLAUSES 131072
#define INITIAL_NUMBER_OF_LITERALS 262144

#define INT_LIST_ALLOCATOR_SIZE 32768

#else

#define MAX_STR_CONST 256
#define DEFAULT_FORMULA_LIST_POOL_BLOCK_SIZE 8192
#define DEFAULT_TERM_LIST_POOL_BLOCK_SIZE 8192
#define INITIAL_FORMULA_VECTOR_SIZE 4
#define INITIAL_TERM_VECTOR_SIZE 4

#define INITIAL_NUMBER_OF_TERMS 256
#define INITIAL_NUMBER_OF_ARGUMENTS 256
#define INITIAL_TERM_POOL_SIZE 256

#define INITIAL_NUMBER_OF_FORMULAS 256
#define INITIAL_FORMULA_POOL_SIZE 256

#define PARSER_INITIAL_MEMORY 256

#define DEFAULT_SYMBOL_TABLE_SIZE 256

#define INITIAL_NUMBER_OF_CLAUSES 256
#define INITIAL_NUMBER_OF_LITERALS 256

#define INT_LIST_ALLOCATOR_SIZE 256

#endif

#define DEFAULT_CLAUSE_RELEVANCE 100
#define DEFAULT_CLEANUP_PERIOD 4000
#define DEFAULT_LOOKAHEAD_RELEVANCE 5
#define DEFAULT_RESTART_PERIOD 20000
#define DEFAULT_RESTART_PERIOD_INCREMENT 1000
#define DEFAULT_NPC_THRESHOLD 20
#define DEFAULT_MIN_NPC_THRESHOLD 3
//
// built-in names
//
#define BOOLEAN_SORT "bool"
#define TRUE_CONSTANT "true"
#define FALSE_CONSTANT "false"
#define REAL_NUMBER_SORT "real"
#define INTEGER_NUMBER_SORT "int"
#define ADD_OP "+"
#define SUB_OP "-"
#define MUL_OP "*"
#define DIV_OP "/"
#define LE_OP "<="
#define LT_OP "<"
#define GE_OP ">="
#define GT_OP ">"

#endif /* CONSTS_H */


