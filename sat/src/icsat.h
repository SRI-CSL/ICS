/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Dec 6, 2002: Created.
***/

#ifndef ICSAT_H
#define ICSAT_H

extern "C" {
	typedef long value;

	void failwith (char *);
	void icsat_register(value* r);
	void icsat_deregister(value* r);
	
	void icsat_reset();
	void icsat_push(); 
	void icsat_pop();
	int icsat_assert(value f); /* returns 0 if unsatisfiable, 1 if satisfiable */
	
	void icsat_reset_scratch_context();
	int icsat_assert_in_scratch_context(value f);

	int icsat_atoms_connected(value f1, value f2);
}

#endif
