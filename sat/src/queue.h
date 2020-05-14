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
     demoura - May 6, 2002: Created.
***/

#ifndef QUEUE_H
#define QUEUE_H

#include<iostream.h>

template<class T, int default_capacity = 64>
class queue {
	T * array;
	int head;
	int tail;
	int capacity;
	
	void expand() {
		int new_capacity = 0;
		if (capacity == 0) 
			new_capacity = default_capacity;
		else
			new_capacity = 2 * capacity;
		MEM_TRACE(ctrace << "[memory] expanding queue " << this << " " << capacity << " -> " << new_capacity << endl;);
		T * new_array = new T[new_capacity];
		// check to see how can be optimized...
		memset(new_array, 0, sizeof(T) * new_capacity);
		if (capacity > 0) {
			memcpy(new_array, array, capacity * sizeof(T));
			delete[] array;
		}
		array = new_array;
		capacity = new_capacity;
	}

public:
	void reset() { head = 0; tail = 0; }
	queue(int initial_capacity = 0) {
		if (initial_capacity == 0) {
			array = NULL;
			capacity = 0;
		}
		else {
			array = new T[initial_capacity];
			capacity = initial_capacity;
		}
		reset();
	}

	~queue() {
		MEM_TRACE(ctrace <<"[memory] deleting queue " << this << endl;);
		delete[] array;
	}

	bool is_empty () {
		assert(head <= tail);
		bool result = (head == tail);
		return result;
	}

	void push(T v) {
		assert(head <= tail);
		if (tail >= capacity)	expand();
		array[tail++] = v;
	}

	bool contains(T v) const {
		for (int i = head; i < tail; i++)
			if (array[i] == v)
				return true;
		return false;
	}
	
	T pop() {	assert(head < tail); return array[head++]; }
	unsigned int get_size() const { assert(head <= tail); return tail - head; }
	T get(unsigned int idx) const { assert(idx < get_size()); return array[idx + head]; }
	void set(unsigned int idx, T v) { assert(idx < get_size()); array[idx + head] = v; }

	// low-level functions... 
	unsigned int get_num_elems_inserted() const { return tail; }
	T low_level_get(unsigned int idx) const { return array[idx]; }
};




#endif /* PERSISTENT_QUEUE_H */
