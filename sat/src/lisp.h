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

#ifndef LISP_H
#define LISP_H

#include<alloc.h>
#include<assert.h>
#include"fast-allocator.h"

template<class T>
struct lisp_list {
	T data;
	lisp_list * next;
};

template<class T>
inline lisp_list<T> * cons(T data, lisp_list<T> * next) {
	lisp_list<T> * new_node = new lisp_list<T>();
	new_node->data = data;
	new_node->next = next;
	return new_node;
}

template<class T>
inline lisp_list<T> * cons(T data, lisp_list<T> * next, FastAllocator * allocator) {
	lisp_list<T> * new_node = (lisp_list<T> *) allocator->allocate(sizeof(lisp_list<T>));
	new_node->data = data;
	new_node->next = next;
	return new_node;
}

template<class T>
inline bool is_nil(lisp_list<T> * l) { return l == NULL; }

template<class T>
inline T & car(lisp_list<T> * l) { assert(l != NULL); return l->data; }

template<class T>
inline lisp_list<T> * cdr(lisp_list<T> * l) { assert(l != NULL); return l->next; }

template<class T, class DoSomethingFcn>
inline void for_each(lisp_list<T> * l, DoSomethingFcn action) {
	while (!is_nil(l)) {
		action(l->data);
		l = l->next;
	}
}

template<class T>
inline void destroy(lisp_list<T> * l) {
	while (!is_nil(l)) {
		lisp_list<T> * aux = l;
		l = l->next;
		delete aux;
	}
}

template<class T>
inline unsigned int list_length(lisp_list<T> * l) {
	unsigned int result = 0;
	while (l != NULL) {
		l = cdr(l); result++;
	}
	return result;
}

template<class T>
inline lisp_list<T> * destructive_invert(lisp_list<T> * l) {
	if (l == NULL)
		return l;
	lisp_list<T> * prev = l;
	lisp_list<T> * curr = prev->next;
	prev->next = NULL;
	while (curr != NULL) {
		lisp_list<T> * aux = curr->next;
		curr->next = prev;
		prev = curr;
		curr = aux;
	}
	return prev;
}


#endif /* LISP_H */
