/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 22, 2002: Created.
***/

#include"util.h"

#ifdef TRACING
#ifndef LP_STDOUT_TRACE
ofstream ctrace(".lazy-prover-trace"); 
//ofstream ctrace("/dev/null"); 
#else
ostream & ctrace = cout;
#endif
#endif
