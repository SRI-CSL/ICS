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

#ifndef SYMBOLS_H
#define SYMBOLS_H

#include<vector.h>
#include<hash_map.h>
#include<hash_set.h>
#include"consts.h"
#include"UString.h"
#include"LPException.h"

class LPSort;
class LPFunctionSymbol; 
class LPSymbolManager;

class LPInvalidFunctionSymbol : public LPException {
protected:
	LPFunctionSymbol * f;
public:
	LPInvalidFunctionSymbol(LPFunctionSymbol * f, const char * msg);
	virtual void dump_message(ostream& target) const;
};

class LPFunctionSymbol {
	UString name;
	LPSort * sort;
	unsigned int arity;
	vector<LPSort *> arguments_sort;
	bool associative;
	bool commutative;
public:
	LPFunctionSymbol(UString & name, LPSort * sort, unsigned int arity = 0, LPSort ** arguments_sort = NULL, 
									 bool associative = false, bool commutative = false);
	~LPFunctionSymbol();
	LPSort * get_sort() const { return sort; }
	LPSort * get_argument_sort(unsigned int arg_idx) const { assert(arg_idx < arity); return arguments_sort[arg_idx]; }
	unsigned int get_arity() const { return arity; }
	const char * get_name() const { return name.cstr(); }
	bool is_associative() const { return associative; }
	bool is_commutative() const { return commutative; }
};

ostream& operator<<(ostream& target, const LPFunctionSymbol & f);
inline ostream& operator<<(ostream& target, const LPFunctionSymbol * f) { return target << *f; };

class LPSort {
	UString name;
	vector<LPFunctionSymbol *> function_symbols;
	static const char * kind;
public:
	LPSort() { 
		// TO REMOVE
	}
	LPSort(const UString & name);
	virtual ~LPSort();
	const char * get_name() const { return name.cstr(); }
	virtual bool is_uninterpreted_sort() const { return true; }
	virtual bool is_finite_sort() const { return false; } // TODO
	virtual void initialize(LPSymbolManager * manager) {}
	virtual void add_function_symbol(LPFunctionSymbol * f);
	virtual const char * get_kind() const { return kind; }
	virtual bool equals(const LPSort * s) const;
	virtual size_t hash() const;
};

struct LPSortEqualTo
{
  bool operator()(const LPSort * obj1, const LPSort * obj2) const { return obj1->equals(obj2); }
};

struct LPSortHashFcn
{
  size_t operator()(const LPSort * obj) const { return obj->hash(); }
};

ostream& operator<<(ostream& target, const LPSort & s);
inline ostream& operator<<(ostream& target, const LPSort * s) { return target << *s; };

class LPRealNumberSort : public LPSort {
	LPFunctionSymbol * add;
	LPFunctionSymbol * sub;
	LPFunctionSymbol * mul;
	LPFunctionSymbol * div;

	LPFunctionSymbol * lt;
	LPFunctionSymbol * le;
	LPFunctionSymbol * gt;
	LPFunctionSymbol * ge;
public:
	static const char * kind;
	LPRealNumberSort():LPSort(REAL_NUMBER_SORT) {}
	virtual bool is_uninterpreted_sort() const { return false; }
	virtual bool is_finite_sort() const { return false; } 
	virtual void initialize(LPSymbolManager * manager);
	virtual const char * get_kind() const { return kind; }

	LPFunctionSymbol * get_add() const { return add; }
	LPFunctionSymbol * get_sub() const { return sub; }
	LPFunctionSymbol * get_mul() const { return mul; }
	LPFunctionSymbol * get_div() const { return div; }

	LPFunctionSymbol * get_lt() const { return lt; }
	LPFunctionSymbol * get_le() const { return le; }
	LPFunctionSymbol * get_gt() const { return gt; }
	LPFunctionSymbol * get_ge() const { return ge; }
};

class LPIntegerNumberSort : public LPSort {
public:
	static const char * kind;
	LPIntegerNumberSort():LPSort(INTEGER_NUMBER_SORT) {}
	virtual bool is_uninterpreted_sort() const { return false; }
	virtual bool is_finite_sort() const { return false; } 
	virtual void initialize(LPSymbolManager * manager);
	virtual const char * get_kind() const { return kind; }
};

class LPEnumerationSort : public LPSort {
	// TODO
};

class LPBooleanSort : public LPSort {
	static const char * kind;
	LPFunctionSymbol * t;
	LPFunctionSymbol * f;
public:
	LPBooleanSort():LPSort(BOOLEAN_SORT) {}
	virtual bool is_uninterpreted_sort() const { return false; }
	virtual bool is_finite_sort() const { return true; } 
	virtual void initialize(LPSymbolManager * manager);
	virtual const char * get_kind() const { return kind; }
	LPFunctionSymbol * get_true() const { return t; }
	LPFunctionSymbol * get_false() const { return f; }
};

class LPArraySort : public LPSort {
	// TODO
};

class LPBitvectorSort : public LPSort {
	// TODO
};

class LPTupleSort : public LPSort {
	// TODO
};

typedef hash_map<UString, LPSort *, hashUString> SortMapping;
typedef hash_set<LPSort *, LPSortHashFcn, LPSortEqualTo> SortSet;
typedef hash_map<UString, LPFunctionSymbol *, hashUString> FunctionSymbolMapping;

class LPSymbolManager {
protected:
	SortMapping sort_table;
	SortSet sort_set;
	FunctionSymbolMapping function_table;
	unsigned int next_unique_id;
	friend ostream& operator<<(ostream& target, const LPSymbolManager & s);

	void add_sort(LPSort * s);
	LPBooleanSort * bool_sort;
	LPRealNumberSort * real_sort;
	LPIntegerNumberSort * int_sort;
public:
	LPSymbolManager();
	~LPSymbolManager();

	LPSort * get_boolean_sort() const { return bool_sort; }
	LPSort * get_real_number_sort() const { return real_sort; }

	bool contains_sort(const LPSort * s) const;
	bool contains_sorts(unsigned int num_sorts, const LPSort ** sorts) const;

	LPSort * get_sort(const char * n) const;
	LPSort * create_uninterpreted_sort(const char * n);
	LPSort * create_array_sort(const char * n, LPSort * index, LPSort * elem);
	LPSort * create_tuple_sort(const char * n, unsigned int arity, LPSort ** elems);
	LPSort * create_enumeration_sort(const char * n, unsigned int num_elements, const char ** elements);
	LPFunctionSymbol * get_function_symbol(const char * n) const;
	LPFunctionSymbol * create_function_symbol(const char * name, LPSort * sort, unsigned int arity = 0, LPSort ** arguments_sort = NULL, 
																						bool associative = false, bool commutative = false);
	LPFunctionSymbol * create_unique_constant(LPSort * sort);

	LPFunctionSymbol * get_add() const { return real_sort->get_add(); }
	LPFunctionSymbol * get_sub() const { return real_sort->get_sub(); }
	LPFunctionSymbol * get_mul() const { return real_sort->get_mul();}
	LPFunctionSymbol * get_div() const { return real_sort->get_div(); }

	LPFunctionSymbol * get_lt() const { return real_sort->get_lt(); }
	LPFunctionSymbol * get_le() const { return real_sort->get_le(); }
	LPFunctionSymbol * get_gt() const { return real_sort->get_gt(); }
	LPFunctionSymbol * get_ge() const { return real_sort->get_ge(); }

	LPFunctionSymbol * get_true() const { return bool_sort->get_true(); }
	LPFunctionSymbol * get_false() const { return bool_sort->get_false(); }
};

ostream& operator<<(ostream& target, const LPSymbolManager & s);
inline ostream& operator<<(ostream& target, const LPSymbolManager * s) { return target << *s; };


#endif /* SYMBOLS_H */


