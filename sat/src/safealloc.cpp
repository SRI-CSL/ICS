/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 10, 2001: Created.
***/
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <new.h>
#include "safealloc.h"

void memoryExhausted()
{
  cerr << "Memory exhausted...\n";
  abort();
}

void * xmalloc(size_t size)
{
  void * aux = malloc(size);
  if (aux == NULL)
	memoryExhausted();
  return aux;
}

char * xstrdup(const char * const source)
{
  char * aux = strdup(source);
  if (aux == NULL)
	memoryExhausted();
  return aux;
}

void setNewHandler()
{
  set_new_handler(memoryExhausted);
}
