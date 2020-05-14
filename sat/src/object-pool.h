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

#ifndef OBJECT_POOL_H
#define OBJECT_POOL_H

#include"growable-vector.h"

template<class T, class ResetFcn>
class object_pool
{
	ResetFcn reset;
	growable_vector<T *> free_list;
#if defined(LP_TRACE_MEMORY) || defined(LP_TRACE_ALL)
	unsigned int num_allocations;
	unsigned int num_real_allocations;
	unsigned int num_deallocations;
#endif
public:
	object_pool() {
    MEM_TRACE(num_allocations = 0;
							num_real_allocations = 0;
							num_deallocations = 0;);
	}
	
	~object_pool() {
		MEM_TRACE(ctrace << "[memory] object pool allocations: " << num_allocations << endl;
							ctrace << "[memory] object pool real allocations: " << num_real_allocations << endl;
							ctrace << "[memory] object pool deallocations: " << num_deallocations << endl;);
		MEM_TRACE(ctrace << "[memory] deleting object pool " << this << endl;);
		while (!free_list.is_empty()) {
			T * curr = free_list.pop();
			MEM_TRACE(ctrace << "[memory] deleting object pool element " << curr << endl;);
			delete curr;
		}
	}

	T * allocate() {
    MEM_TRACE(num_allocations++;);
		T * result;
		if (free_list.is_empty()) {
			MEM_TRACE(num_real_allocations++;);
			result = new T();
		}
		else {
			result = free_list.pop();
		}
		MEM_TRACE(ctrace << "[memory] allocating object pool element " << result << endl;);
		return result;
	}
	
	void recycle(T * v) {
    MEM_TRACE(num_deallocations++;);
		MEM_TRACE(ctrace << "[memory] recycling object pool element " << v << endl;);
		DBG_CODE(unsigned int n = free_list.get_size();
						 for (unsigned int i = 0; i < n; i++) {
							 if (free_list.get(i) == v)
								 assert(false); // element is already in the recycle list!
						 });
		reset(v);
		free_list.push(v);
	}
};

#endif /* OBJECT_POOL_H */
