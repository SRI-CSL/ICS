/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 10, 2001: Created.
***/
#ifndef SAFEALLOC_H
#define SAFEALLOC_H
#include <stdlib.h>

void memoryExhausted();
void * xmalloc(size_t size);
char * xstrdup(const char * const source);
void setNewHandler();

#endif /* SAFEALLOC_H */
