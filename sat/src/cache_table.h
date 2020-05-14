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
     demoura - Apr 5, 2002: Created.
***/

#ifndef CACHE_TABLE_H
#define CACHE_TABLE_H

#include<pair.h>

template<class T, class HashFcn, class EqualKey, T NullElement>
class cache_table {
	HashFcn hash;
  EqualKey equals;
	T * buckets;
	unsigned int size;
public:
	cache_table(unsigned int s) {
		size = s;
		buckets = new T[size];
		reset();
	}

	~cache_table() {
		delete[] buckets;
	}

	void reset() {
		for (unsigned int i = 0; i < size; i++)
			buckets[i] = NullElement;
	}

	void insert(const T & v) {
		unsigned int idx = hash(v) % size;
		buckets[idx] = v;
	}
	
	void contains(const T & v) const {
		unsigned int idx = hash(v) % size;
		return equals(buckets[idx], v);
	}
	
	pair<bool, const T &> get(const T & v) const {
		unsigned int idx = hash(v) % size;
		return pair<bool, const T &>(equals(buckets[idx], v), buckets[idx]);
	} 
};



#endif /* CACHE_TABLE_H */
