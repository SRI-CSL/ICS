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
     demoura - Mar 25, 2002: Created.
***/

#include"symbols.h"
#include"fmessage.h"

LPInvalidFunctionSymbol::LPInvalidFunctionSymbol(LPFunctionSymbol * f, const char * msg):LPException(msg)
{
	this->f = f;
}

void LPInvalidFunctionSymbol::dump_message(ostream& target) const
{
	target << "Invalid function symbol \"" << f << "\" : " << message;
}

LPFunctionSymbol::LPFunctionSymbol(UString & name, LPSort * sort, unsigned int arity = 0, LPSort ** arguments_sort = NULL, 
																	 bool associative = false, bool commutative = false) 
{
	if (arity != 2 && (associative || commutative))
		throw new LPInvalidFunctionSymbol(this, "Invalid associative and/or commutative function symbol declaration.");
	if ((associative || commutative) && (arguments_sort[0] != arguments_sort[1]))
		throw new LPInvalidFunctionSymbol(this, "Invalid arguments in associative and/or commutative function symbol declaration.");
	this->name = name;
	this->sort = sort;
	this->arity = arity;
	for (unsigned int i = 0; i < arity; i++)
		this->arguments_sort.push_back(arguments_sort[i]);
	this->commutative = commutative;
	this->associative = associative;
}

LPFunctionSymbol::~LPFunctionSymbol()
{
}

ostream& operator<<(ostream& target, const LPFunctionSymbol & f)
{
	target << f.get_name() << ": ";
	int arity = f.get_arity();
	for(int i = 0; i < arity; i++) {
		target << f.get_argument_sort(i);
		if (i < arity - 1)
			target << ",";
		target << " ";
	}
	target << "-> " << f.get_sort();
	if (f.is_associative() && f.is_commutative())
		target << " {associative, commutative}";
	else if (f.is_associative())
		target << " {associative}";
	else if (f.is_commutative())
		target << " {commutative}";
	return target;
}

const char * LPSort::kind = "Uninterpreted Sort";

LPSort::LPSort(const UString & name)
{
	this->name = name;
}

LPSort::~LPSort()
{
}

void LPSort::add_function_symbol(LPFunctionSymbol * f)
{
	function_symbols.push_back(f);
}

bool LPSort::equals(const LPSort * s) const
{
	if (this->get_kind() != s->get_kind()) return false;
	return name == s->get_name();
}

static hash<const char *> proc;

size_t LPSort::hash() const
{
	return proc(name.cstr());
}

ostream& operator<<(ostream& target, const LPSort & s)
{
	target << s.get_name();
	return target;
}

// 
// Boolean sort implementation
//
const char * LPBooleanSort::kind = "Boolean Sort";

void LPBooleanSort::initialize(LPSymbolManager * manager) {
	manager->create_function_symbol(TRUE_CONSTANT, this);
	manager->create_function_symbol(FALSE_CONSTANT, this);
}

//
// Real number sort implementation
//
const char * LPRealNumberSort::kind = "Real number sort";

void LPRealNumberSort::initialize(LPSymbolManager * manager) {
	LPSort * args[] = {this, this};
	LPSort * bool_sort = manager->get_sort(BOOLEAN_SORT);
	add = manager->create_function_symbol(ADD_OP, this, 2, args, true, true);
	sub = manager->create_function_symbol(SUB_OP, this, 2, args, false, false);
	mul = manager->create_function_symbol(MUL_OP, this, 2, args, true, true);
	div = manager->create_function_symbol(DIV_OP, this, 2, args, false, false);	

	lt = manager->create_function_symbol(LT_OP, bool_sort, 2, args);
	le = manager->create_function_symbol(LE_OP, bool_sort, 2, args);
	gt = manager->create_function_symbol(GT_OP, bool_sort, 2, args);
	ge = manager->create_function_symbol(GE_OP, bool_sort, 2, args);
}

//
// Integer number sort implementation... the following 
//
const char * LPIntegerNumberSort::kind = "Integer number sort";

void LPIntegerNumberSort::initialize(LPSymbolManager * manager) {
	// do nothing
}
	

//
// Symbol Manager implementation
//

LPSymbolManager::LPSymbolManager()
{
	next_unique_id = 1;
	// TODO: Initialize tables with bigger values...

	// initialize builtin sorts...
	bool_sort = new LPBooleanSort();
	add_sort(bool_sort); bool_sort->initialize(this);

	real_sort = new LPRealNumberSort();
	add_sort(real_sort); real_sort->initialize(this);

	int_sort = new LPIntegerNumberSort();
	add_sort(int_sort); int_sort->initialize(this);
}

LPSymbolManager::~LPSymbolManager()
{
	// TODO
}

void LPSymbolManager::add_sort(LPSort * s)
{
	sort_table[s->get_name()] = s;
	sort_set.insert(s);
}

LPSort * LPSymbolManager::get_sort(const char * n) const
{
	SortMapping::const_iterator it = sort_table.find(n);
	if (it == sort_table.end())
		throw new LPException(formatMessage("Undefined sort \"%s\"", n));
	return (*it).second;
}

LPSort * LPSymbolManager::create_uninterpreted_sort(const char * n)
{
	SortMapping::const_iterator it = sort_table.find(n);
	if (it != sort_table.end())
		throw new LPException(formatMessage("Sort \"%s\" was already defined", n));
	UString name(n);
	LPSort * new_sort = new LPSort(name);
	add_sort(new_sort);
	return new_sort;
}

LPSort * LPSymbolManager::create_array_sort(const char * n, LPSort * index, LPSort * elem)
{
	// TODO
	return NULL;
}

LPSort * LPSymbolManager::create_tuple_sort(const char * n, unsigned int arity, LPSort ** elems)
{
	// TODO
	return NULL;
}

LPSort * LPSymbolManager::create_enumeration_sort(const char * n, unsigned int num_elements, const char ** elements)
{
	// TODO
	return NULL;
}

LPFunctionSymbol * LPSymbolManager::get_function_symbol(const char * n) const
{
	FunctionSymbolMapping::const_iterator it = function_table.find(n);
	if (it == function_table.end())
		throw new LPException(formatMessage("Undefined function symbol \"%s\"", n));
	return (*it).second;
}

bool LPSymbolManager::contains_sort(const LPSort * s) const
{
	return sort_set.find((LPSort *) s) != sort_set.end();
}

bool LPSymbolManager::contains_sorts(unsigned int num_sorts, const LPSort ** sorts) const
{
	for (unsigned int i = 0; i < num_sorts; i++)
		if (!contains_sort(sorts[i]))
			return false;
	return true;
}

LPFunctionSymbol * LPSymbolManager::create_function_symbol(const char * n, LPSort * sort, unsigned int arity = 0, LPSort ** arguments_sort = NULL, 
																													 bool associative = false, bool commutative = false)
{
	assert(sort_set.find(sort) != sort_set.end());
	
	FunctionSymbolMapping::const_iterator it = function_table.find(n);
	if (it != function_table.end())
		throw new LPException(formatMessage("Function symbol \"%s\" was already defined", n));
	UString name(n);
	LPFunctionSymbol * new_f = new LPFunctionSymbol(name, sort, arity, arguments_sort, associative, commutative);
	sort->add_function_symbol(new_f);
	function_table[name] = new_f;
	return new_f;
}

LPFunctionSymbol * LPSymbolManager::create_unique_constant(LPSort * sort)
{
	static StringBuffer buffer;
	while (true) {
		buffer.reset();
		buffer.append("k");
		buffer.append(next_unique_id++);
		const char * name = buffer.cstr();
		try {
			return create_function_symbol(name, sort);
		}
		catch (LPException * ex) {
			delete ex;
		}
	}
}

ostream& operator<<(ostream& target, const LPSymbolManager & s)
{
	target << "Symbol Manager\n---------------\n";
	target << "\tSorts:\n";
	SortSet::const_iterator it = s.sort_set.begin();
	for (; it != s.sort_set.end(); it++)
		target << "\t\t" << *it << endl;
	target << "\tFunction Symbols:\n";
	FunctionSymbolMapping::const_iterator it2 = s.function_table.begin();
	for (; it2 != s.function_table.end(); it2++)
		target << "\t\t" << (*it2).second << endl;
	return target;
}



