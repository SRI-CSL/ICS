/* Copyright (c) SRI International 2002. */
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
