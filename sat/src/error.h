/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 6, 2002: Created.
***/

#ifndef ERROR_H
#define ERROR_H

#include <assert.h>
#include <stdlib.h>

void error(char *, ...);

#ifdef __GNUC__
void fatal_error (char *, ...)  __attribute__ ((noreturn));
#else
void fatal_error (char *, ...)  __attribute__ ((noreturn));
#endif

#define UNREACHABLE_CODE (assert(0), fatal_error("bug in the code!\n"))

#define NOT_IMPLEMENTED_YET (fatal_error("feature not implemented yet!\n"))


#endif /* ERROR_H */
