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
     demoura - Apr 9, 2002: Created.
***/

#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include<hash_map.h>
#include<pair.h>
#include"consts.h"
#include"UString.h"
#include"fast-allocator.h" 

template<class T>
struct binder
{
  T value;
  UString prevtop;
  binder * tail;
};

template<class T>
class symbol_table
{
	typedef hash_map<UString, binder<T> *, hashUString> binder_mapping;
  binder_mapping mapping;
  UString top;
  binder<T> * marks;
	FastAllocator allocator;
	T null_element;

	binder<T> * new_binder(const T & v, UString & p, binder<T> * t) {
		binder<T> * n = (binder<T> *) allocator.allocate(sizeof(binder<T>));
		n->value = v;
		n->prevtop = p;
		n->tail = t;
		return n;
	}

	binder<T> * new_binder(UString & p, binder<T> * t) {
		binder<T> * n = (binder<T> *) allocator.allocate(sizeof(binder<T>));
		n->prevtop = p;
		n->tail = t;
		return n;
	}

public:
	symbol_table(unsigned int symbol_table_size = DEFAULT_SYMBOL_TABLE_SIZE):
		mapping(symbol_table_size),
		top(""), 
		marks(NULL), 
		allocator(symbol_table_size * sizeof(binder<T>)) {}
	  
  ~symbol_table() { 
		// do nothing...
	}

  bool contains(const UString & key) const {
		return mapping.find(key) != mapping.end();
	}

  pair<bool, T>  get(const UString & key) const {
		binder_mapping::const_iterator it = mapping.find(key);
		if (it != mapping.end())
			return pair<bool, T>(true, (*it).second->value);
		else
			return pair<bool, T>(false, null_element);
	}

  void put(const UString & key, const T & value) {
		if (contains(key))
			mapping[key] = new_binder(value, top, mapping[key]);
		else
			mapping[key] = new_binder(value, top, NULL);
		top = key;
	}
	
  void begin_scope() {
		marks = new_binder(top, marks); 
		top = "";
	}

  void end_scope() {
		while (top != "") {
			binder<T> * e = mapping[top];
			if (e->tail != NULL) 
				mapping[top] = e->tail;
			else 
				mapping.erase(top);
			top = e->prevtop;
		}
		top = marks->prevtop;
		marks = marks->tail;
	}
};



#endif /* SYMBOL_TABLE_H */
