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
     demoura - Mar 21, 2002: Created.
***/

#ifndef CIRCULAR_LIST_H
#define CIRCULAR_LIST_H

#include<function.h>
#include<assert.h>
#include"growable-vector.h"

template <class T>
struct circular_list {
	T value;
	circular_list * next;
};

template <class T>
inline void circular_list_merge(circular_list<T> * & l1, circular_list<T> * & l2) {
	if (l1 == NULL) {
		l1 = l2;
		l2 = NULL;
	}
	else if (l2 == NULL) {
		// do nothing
	}
	else {
		circular_list<T> * aux = l1->next;
		l1->next = l2->next;
		l2->next = aux;
		l2 = NULL;
	}
}

template <class T, class DoSomethingFcn>
void circular_list_for_each(circular_list<T> * l, DoSomethingFcn do_something) {
	if (l == NULL) return;
	circular_list<T> * curr = l;
	do {
		assert(curr != NULL && curr->value != NULL);
		do_something(curr->value);
		curr = curr->next;
	} while (curr != l);
}

template <class T>
void circular_list_set_singleton(circular_list<T> * l, T & v)
{
	l->value = v;
	l->next = l;
}

#endif /* CIRCULAR_LIST_H */

