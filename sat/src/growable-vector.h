/* Copyright (c) SRI International 2002. */
#ifndef GROWABLE_VECTOR_H
#define GROWABLE_VECTOR_H

#include "util.h"

template<class T, unsigned int default_capacity = 64, bool owner = true>
class growable_vector {
	T * array;
	unsigned int next_pos;
	unsigned int capacity;

	void expand() {
		unsigned int new_capacity = 0;
		if (capacity == 0) 
			new_capacity = default_capacity;
		else
			new_capacity = 2 * capacity;
		T * new_array = new T[new_capacity];
		MEM_TRACE(ctrace << "[memory] expanding vector " << this << " (" << capacity << ", " << array << ") -> (" 
							<< new_capacity << ", " << new_array << ")\n";);
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
	growable_vector(unsigned int initial_capacity = 0) {
		if (initial_capacity == 0) {
			array = NULL;
			capacity = 0;
			next_pos = 0;
		}
		else {
			array = new T[initial_capacity];
			capacity = initial_capacity;
			next_pos = 0;
		}
	}

	growable_vector(const growable_vector & v) {
		resize(v.get_size());
		memcpy(array, v.array, sizeof(T) * v.get_size());
		next_pos = v.next_pos;
	}

	~growable_vector() {
		MEM_TRACE(ctrace<<"[memory] deleting growable_vector " << this << endl;);
		if (owner)
			delete[] array;
	}

	void clean() {
		memset(array, 0, sizeof(T) * capacity);
	}

	void reset() { next_pos = 0; }
	bool is_empty() const { return next_pos == 0; }

	void push(const T & v) {
		if (next_pos >= capacity)
			expand();
		assert(next_pos < capacity);
		array[next_pos++] = v;
	}
	
	T * allocate() {
		if (next_pos >= capacity)
			expand();
		assert(next_pos < capacity);
		return &(array[next_pos++]);
	}

	T & pop() {
		assert(next_pos > 0);
		return array[--next_pos];
	}

	T & last() const {
		return array[next_pos - 1];
	}

	T & top() const { return last(); }
	
	unsigned int get_size() const { return next_pos; }

	T & get(unsigned int idx) const {
		assert(idx < next_pos);
		return array[idx];
	}

	unsigned int get_id(T * value) {
		assert(value >= array && value < &(array[next_pos]));
		return value - array;
	}

	T * get_ptr(unsigned int idx) const {
		assert(idx < next_pos);
		return &(array[idx]);
	}
	
	void set(unsigned int idx, const T & v) {
		assert(idx < next_pos);
		array[idx] = v;
	}

	void set_capacity(unsigned int new_capacity) {
		array = new T[new_capacity];
		capacity = new_capacity;
		next_pos = 0;
	}

	void resize(unsigned int new_size) {
		while (new_size >= capacity)
			expand();
		next_pos = new_size;
	}

	void set_x(unsigned int idx, const T & v) {
		if (idx >= next_pos)
			resize(idx + 1);
		array[idx] = v;
	}

	T * get_contents() const { return array; }

	bool contains(const T & v) const {
		for (unsigned int i = 0; i < next_pos; i++)
			if (array[i] == v)
				return true;
		return false;
	}

	void fill(const T & v) const {
		for (unsigned int i = 0; i < capacity; i++)
			array[i] = v;
	}
	
	void copy_contents(growable_vector & v) {
		unsigned int n = v.get_size();
		for(unsigned int i = 0; i < n; i++)
			push(v.get(i));
	}

	friend inline ostream & operator<<(ostream & target, const growable_vector & v) {
		unsigned int n = v.get_size();
		target << "[";
		for (unsigned int i = 0; i < n; i++) {
			target << v.get(i);
			if (i < n - 1) 
				target << ", ";
		}
		target << "]";
		return target;
	}

};

#endif /* GROWABLE_VECTOR_H */









