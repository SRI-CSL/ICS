/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Aug 8, 2002: Created.

  Comiling using static library:
     g++ -o bakery -L ../lib/i686-pc-linux-gnu/ \
                   -I ../obj/i686-pc-linux-gnu/ \
                   -lics bakery.cpp

  Compiling using dynamic library:
     g++ -o bakery -L ../lib/i686-pc-linux-gnu/ \
                   -I ../obj/i686-pc-linux-gnu/ \ 
                   -licsall -lgmp bakery.cpp
***/
#include<stdio.h>
#include<stdlib.h>
#include<iostream.h>

extern "C" {
void ics_caml_startup(int full, char** argv);
#include<ics.h>
}

extern "C" {
void ics_deregister(value* r); 
}

#define MAX_ARRAY_SIZE 1024

int CALLS_TO_ICS = 0;

value * y1_ge_0;
value * y2_ge_0;
value * ny1_eq_y1[MAX_ARRAY_SIZE];
value * ny2_eq_y2[MAX_ARRAY_SIZE];
value * ny1_eq_y2_plus_1[MAX_ARRAY_SIZE];
value * ny2_eq_y1_plus_1[MAX_ARRAY_SIZE];
value * y1_eq_0[MAX_ARRAY_SIZE];
value * y2_eq_0[MAX_ARRAY_SIZE];
value * y1_lt_y2[MAX_ARRAY_SIZE];
value * y2_lt_y1[MAX_ARRAY_SIZE];

#define BUFFER_SIZE 8192

void init_arrays(int max_depth)
{
  static char buffer[BUFFER_SIZE];
  y1_ge_0 = ics_atom_of_string("y10 >= 0");
  y2_ge_0 = ics_atom_of_string("y20 >= 0");
  
  for(int i = 0; i <= max_depth; i++) {
    sprintf(buffer, "y1%d = y1%d", i+1, i);
    ny1_eq_y1[i] = ics_atom_of_string(buffer);
    
    sprintf(buffer, "y2%d = y2%d", i+1, i);
    ny2_eq_y2[i] = ics_atom_of_string(buffer);
		
    sprintf(buffer, "y1%d = y2%d + 1", i+1, i);
    ny1_eq_y2_plus_1[i] = ics_atom_of_string(buffer);

    sprintf(buffer, "y2%d = y1%d + 1", i+1, i);
    ny2_eq_y1_plus_1[i] = ics_atom_of_string(buffer);

    sprintf(buffer, "y1%d = 0", i);
    y1_eq_0[i] = ics_atom_of_string(buffer);

    sprintf(buffer, "y2%d = 0", i);
    y2_eq_0[i] = ics_atom_of_string(buffer);
	
    sprintf(buffer, "y1%d < y2%d", i,i);
    y1_lt_y2[i] = ics_atom_of_string(buffer);
	
    sprintf(buffer, "y2%d < y1%d", i,i);
    y2_lt_y1[i] = ics_atom_of_string(buffer);
  }
}

bool process(value * state, value ** next, bool d_prev, value * atom) {
  CALLS_TO_ICS++;
  value * status = ics_process(state, atom);		
  bool result;				       					 
  
  if (ics_is_consistent(status)) {						
    *next = ics_d_consistent(status);				
    result = true;					   		  	
  }		  			   					  
  else if (ics_is_redundant(status)) {					
    *next = state;					    
    result = true;		    					
  }									    
  else if (ics_is_inconsistent(status)) {				
    result = false;		       				
  }		      								
  if (d_prev) {		  						
    ics_deregister(state);		    			
    free(state);   							
  }										  
  ics_deregister(status);							
  free(status);		    						
  return result;
}
	
int MAX_DEPTH = 0;

bool printed = false;

#define ERROR() {			  
  if (!printed) {								  
    ics_context_pp(state);
    printed = true;								
  }				    
  cout << endl;
  cout << "pc1 = " << pc1 << ", pc2 = " << pc2 << ", at depth = " << depth << endl; 
  return 0;	   
}

int bakery_step(int pc1, int pc2, int depth, value * state)
{
  if (depth >= MAX_DEPTH)
    return 1;

  if (pc1 == 3 && pc2 == 3) {
    cout << "Error detected.... pc1 = " 
         << pc1 
         << ", pc2 = " 
         << pc2 
         << " at depth = " 
         << depth
         << endl;
    return 0;
  }
  
  value * new_state;
  
  switch (pc1) {
  case 1:
    if(!process(state, &new_state, 0, ny1_eq_y2_plus_1[depth]))
      break;
    if(!process(new_state, &new_state, 1, ny2_eq_y2[depth]))
      break;
    if (!bakery_step(2, pc2, depth+1, new_state)) {
      ERROR();
    }
    break;
  case 2:
    if(!process(state, &new_state, 0, ny1_eq_y1[depth]))
      break;
    if(!process(new_state, &new_state, 1, ny2_eq_y2[depth]))
      break;
    {
      value * saved_state = new_state;
      if (process(saved_state, &new_state, 0, y2_eq_0[depth]) &&
	  !bakery_step(3, pc2, depth+1, new_state)) {
	ERROR();
      }
      if (process(saved_state, &new_state, 0, y1_lt_y2[depth]) &&
	  !bakery_step(3, pc2, depth+1, new_state)) {
	ERROR();
      }
      ics_deregister(saved_state);
      free(saved_state);
    }
    break;
  case 3:
    if (!process(state, &new_state, 0, y1_eq_0[depth+1]))
      break;
    if (!process(new_state, &new_state, 1, ny2_eq_y2[depth]))
      break;
    if (!bakery_step(1, pc2, depth+1, new_state)) {
      ERROR();
    }
    break;
  }
  
  switch(pc2) {
  case 1:
    if(!process(state, &new_state, 0, ny2_eq_y1_plus_1[depth]))
      break;
    if(!process(new_state, &new_state, 1, ny1_eq_y1[depth]))
      break;
    if (!bakery_step(pc1, 2, depth+1, new_state)) {
      ERROR();
    }
    break;
  case 2:
    if(!process(state, &new_state, 0, ny1_eq_y1[depth]))
      break;
    if(!process(new_state, &new_state, 1, ny2_eq_y2[depth]))
      break;
    {
      value * saved_state = new_state;
      if (process(saved_state, &new_state, 0, y1_eq_0[depth]) &&
	  !bakery_step(pc1, 3, depth+1, new_state)) {
	ERROR();
      }
      if (process(saved_state, &new_state, 0, y2_lt_y1[depth]) &&
	  !bakery_step(pc1, 3, depth+1, new_state)) {
	ERROR();
      }
      ics_deregister(saved_state);
      free(saved_state);
    }
    break;
  case 3:
    if (!process(state, &new_state, 0, y2_eq_0[depth+1]))
      break;
    if (!process(new_state, &new_state, 1, ny1_eq_y1[depth]))
			break;
    if (!bakery_step(pc1, 1, depth+1, new_state)) {
      ERROR();
    }
    break;
  }
  
  // ics_deregister(state);
  // free(state);
  
  return 1;
}


int main(int argc, char ** argv)
{
	ics_caml_startup(1, argv);
	cout << "depth = " << argv[1] << endl;
	int depth = atoi(argv[1]);
	cout << "ICS Started...\n";
	init_arrays(depth);
	cout << "Atoms initialized...\n";

	value * ini_state = ics_context_empty();
	process(ini_state, &ini_state, 0, y1_ge_0);
	process(ini_state, &ini_state, 0, y2_ge_0);
	
	MAX_DEPTH = depth;

	if (!bakery_step(1, 1, 0, ini_state))
		cout << "ERROR...." << endl;

	cout << "calls to ICS = " << CALLS_TO_ICS << endl;
	return 0;
}

extern "C" {

void ics_error(char * funname, char * message) {
	cerr << "ICS error at " 
	     << funname 
	     << " : " 
	     << message 
	     << endl;
	exit(1);
}

}
