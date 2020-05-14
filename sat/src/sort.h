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

#ifndef SORT_H
#define SORT_H

template<class T, class LtFcn, unsigned int M = 10>
class sortFcn {
	LtFcn lt;
	T * array;

	void swap(unsigned int i, unsigned int j) {
		T aux = array[i];
		array[i] = array[j];
		array[j] = aux;
	}	

	void quicksort(unsigned int l, unsigned int r)
	{
		if (r - l > M) {
			unsigned int i = (r + l) / 2;
			// tri-median method
			if (lt(array[i], array[l])) // order_idx_value(l) < order_idx_value(i))
				swap(l,i);
			if (lt(array[r], array[l])) // order_idx_value(l) < order_idx_value(r))
				swap(l,r);
			if (lt(array[i], array[r])) // order_idx_value(i) < order_idx_value(r))
				swap(i,r);
			unsigned int j = r - 1;
			swap(i,j);
			i = l;
			T v = array[j]; // order_idx_value(j);
			while (true) {
				while(lt(array[++i], v)); // order_idx_value(++i)>v);
				while(lt(v, array[--j])); // order_idx_value(--j)<v);
				if (j < i) break;
				swap(i,j);
			}
			swap(i, r-1);
			quicksort(l,j);
			quicksort(i+1, r);
		}
	}
	
	void prv_insertion_sort(unsigned int lo0, unsigned int hi0)
	{
		for(unsigned int i = lo0+1; i <= hi0; i++) {
			T v = array[i];
			unsigned int j = i;
			while (j > lo0 && lt(v, array[j-1])) {
				array[j] = array[j-1];
				j--;
			}
			array[j] = v;
		}
	}

public:
	sortFcn(LtFcn f):lt(f) {
	}
	
	sortFcn() {
		lt = LtFcn();
	}

	void operator()(T * a, unsigned int size) {
		array = a;
		quicksort(0, size-1);
		prv_insertion_sort(0, size-1);
	}

	void insertion_sort(T * a, unsigned int size) {
		array = a;
		prv_insertion_sort(0, size -1);
	}

	bool is_sorted(T * a, unsigned int size) {
		for (unsigned int i = 1; i < size; i++)
			if (lt(a[i], a[i-1]))
				return false;
		return true;
	}

};

#endif /* SORT_H */
