/* Copyright (c) SRI International 2002. */
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
