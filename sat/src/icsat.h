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
	int icsat_add_scratch_context(value f);

	int icsat_is_connected(int f1, int f2);
	void icsat_atom_pp(int x1);
	void icsat_stackpp();
}

#endif
