/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 22, 2002: Created.
***/

#ifndef FAST_ALLOCATOR_H
#define FAST_ALLOCATOR_H

#include"util.h"

class FastAllocator
{
	byte * curr_block;
	unsigned int block_size;
	unsigned int curr_pos;
public:
	FastAllocator(unsigned int size);
	~FastAllocator();
	void * allocate(unsigned int size);
	void reset();
};

#endif /* FAST_ALLOCATOR_H */
