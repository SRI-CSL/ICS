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
     demoura - Mar 22, 2002: Created.
***/

#ifndef LPSORT_H
#define LPSORT_H

class LPSort;

class LPFunctionSymbol {
	char * name;
	LPSort * sort;
	unsigned int arity;
	LPSort ** arguments_sort;
	bool is_associative;
	bool is_commutative;
public:
	LPFunctionSymbol(const char * name, LPSort * sort, int arity = 0, LPSort ** arguments_sort = NULL, 
									 bool is_associative = false, bool is_commutative = false);
	~LPFunctionSymbol();
	LPSort * get_sort() const { return sort; }
	LPSort * get_argument_sort(unsigned int arg_idx) const { assert(arg_idx < arity); return arguments_sort[arg_idx]; }
	int get_arity() const { return arity; }
	const char * get_name() const { return name; }
	bool is_associative() const { return is_associative; }
	bool is_commutative() const { return is_commutative; }
};


class LPSort {
	char * name;
public:
	LPSort(char * name);
	~LPSort();
	virtual bool uninterpreted_sort() const { return true; }
	virtual void initialize(LPSymbolManager * manager) {}
	virtual void add_function_symbol(LPFunctionSymbol * symbol) {}
};

class LPRealNumberSort : public LPSort {

};

class LPEnumerationSort : public LPSort {
	
};

typedef hash_map<UString, LPSort *, hashUString> SortMapping;
typedef hash_map<UString, LPFunctionSymbol *, hashUString> FunctionSymbolMapping;

class LPSymbolManager {
	SortMapping sort_table;
	FunctionSymbolMapping function_table;

};


#endif /* LPSORT_H */
