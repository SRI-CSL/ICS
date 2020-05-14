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

#ifndef UTIL_H
#define UTIL_H

#include<assert.h>
#include<iostream.h>
#include<fstream.h>
#include<pair.h>
#include<stdlib.h>

// useful macros & functions

#if defined(LP_TRACE) || defined(LP_TRACE_ALL)
#define TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_PREPROCESSOR) || defined(LP_TRACE_ALL)
#define PP_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define PP_TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_MEMORY) || defined(LP_TRACE_ALL)
#define MEM_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define MEM_TRACE(code) ((void)0)
#endif

#define IMPLY(a,b) (!(a) || (b))

#if defined(LP_TRACE_SOLVER) || defined(LP_TRACE_ALL)
#define SOLVER_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define SOLVER_TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_SOLVER_EXPLAIN) || defined(LP_TRACE_ALL)
#define SOLVER_EXPLAIN_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define SOLVER_EXPLAIN_TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_SOLVER_LOOKAHEAD) || defined(LP_TRACE_ALL)
#define SOLVER_LOOKAHEAD_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define SOLVER_LOOKAHEAD_TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_RULE) || defined(LP_TRACE_ALL)
#define RULE_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define RULE_TRACE(code) ((void)0)
#endif

#if defined(LP_TRACE_ICS) || defined(LP_TRACE_ALL)
#define ICS_TRACE(code) { code } ((void) 0)
#ifndef TRACING
#define TRACING
#endif
#else
#define ICS_TRACE(code) ((void)0)
#endif

#ifndef NDEBUG
#define DBG_CODE(code) { code } ((void) 0)
#else
#define DBG_CODE(code) ((void)0)
#endif

inline int max(int x, int y)
{ return x > y ? x : y; }

//  inline int abs(int x)
//  { return x >= 0 ? x : -x; }

//  template<class T>
//  inline void swap(T & x, T & y) {
//  	T aux = x;
//  	x = y;
//  	y = x;
//  }

typedef char byte;

template<class T>
inline void expand_array(T * & array, unsigned int & curr_size, double grow_factor = 2.0, bool update_size_variable = true) {
	assert(array != NULL);
	unsigned int new_size = (unsigned int) (curr_size * grow_factor);
	T * new_array = new T[new_size];
	memset(new_array, 0, sizeof(T)*new_size);
	memcpy(new_array, array, curr_size*sizeof(T));
	if (update_size_variable)
		curr_size = (unsigned int) (curr_size * grow_factor);
	delete[] array;
	array = new_array;
}

template <class T1, class T2>
inline ostream& operator<<(ostream& target, const pair<T1, T2> & p)
{
	target << "<" << p.first << ", " << p.second << ">";
	return target;
}

#define feature_not_implemented_yet() {																								\
	cerr << "feature not implemented yet at ["<< __FILE__ << ", " << __LINE__ << "]\n";	\
	assert(false);																																			\
	exit(-1);																																						\
}

inline unsigned int absolute(int x)
{
	return x < 0 ? -x : x;
}

#ifdef TRACING
#ifndef LP_STDOUT_TRACE
extern ofstream ctrace; 
#else
extern ostream & ctrace;
#endif
#endif

#endif /* UTIL_H */
