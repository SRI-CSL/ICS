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

#ifndef FIX_SIZE_ALLOCATOR_H
#define FIX_SIZE_ALLOCATOR_H

#include<assert.h>
#include<memory.h>
#include"util.h"

template <class T>
class FixSizeAllocator
{
	byte * curr_block;
	unsigned int block_size;
	unsigned int curr_pos;
	void * free_list;
public:
	FixSizeAllocator(unsigned int size) {
		assert(sizeof(T) >= sizeof(void *));
		assert(size > sizeof(T) + sizeof(void *));
		block_size = size;
		curr_block = new byte[size];
		memset(curr_block, 0, size);
		free_list = NULL;
		curr_pos = sizeof(void*);
	}

	~FixSizeAllocator() {
		byte * curr = curr_block;
		while (curr != NULL) {
			void * prev = *((void **) &(curr[0]));
			delete[] curr;
			curr = (byte *) prev;
		}
		curr_block = NULL;
		curr_pos = 0;
		free_list = NULL;
	}

	T * allocate() {
		T * result;
		if (free_list != NULL) {
			result = (T *) free_list;
			free_list = *((void **) free_list);
			return result;
		}

		if (sizeof(T) + curr_pos > block_size) {
			// create a new block
			byte * new_block = new byte[block_size];
			void ** prev = (void **) &(new_block[0]);
			*prev = curr_block;
			curr_block = new_block;
			curr_pos = sizeof(void*);
		}

		result = (T *) &(curr_block[curr_pos]);
		curr_pos += sizeof(T); 
		return result;
	}

	void recycle(T * f) {
		void ** link = (void **) f;
		memset(f, 0, sizeof(T));
		*link = free_list;
		free_list = (void *) link;
	}

	void reset() {
		void * prev = *((void **) &(curr_block[0]));
		while (prev != NULL) {
			delete[] curr_block;
			curr_block = (byte *) prev;
			prev = *((void **) &(curr_block[0]));
		}
		memset(curr_block, 0, block_size);
		curr_pos = sizeof(void *);
		free_list = NULL;
	}
};

#endif /* FIX_SIZE_ALLOCATOR_H */
