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

#include<assert.h>
#include<memory.h>
#include"fast-allocator.h"

FastAllocator::FastAllocator(unsigned int size) {
	assert(size > sizeof(void *));
	block_size = size;
	curr_block = new byte[size];
	memset(curr_block, 0, size);
	curr_pos = sizeof(void*);
}

FastAllocator::~FastAllocator() {
	byte * curr = curr_block;
	while (curr != NULL) {
		void * prev = *((void **) &(curr[0]));
		delete[] curr;
		curr = (byte *) prev;
	}
	curr_block = NULL;
	curr_pos = 0;
}

void * FastAllocator::allocate(unsigned int size) {
	assert(size <= block_size - sizeof(void*));
	
	if (size + curr_pos > block_size) {
		// create a new block
		byte * new_block = new byte[block_size];
		void ** prev = (void **) &(new_block[0]);
		*prev = curr_block;
		curr_block = new_block;
		curr_pos = sizeof(void*);
	}

	void * result = (void *) &(curr_block[curr_pos]);
	curr_pos += size; 
	return result;
}

void FastAllocator::reset() {
	void * prev = *((void **) &(curr_block[0]));
	while (prev != NULL) {
		delete[] curr_block;
		curr_block = (byte *) prev;
		prev = *((void **) &(curr_block[0]));
	}
	memset(curr_block, 0, block_size);
	curr_pos = sizeof(void *);
}
