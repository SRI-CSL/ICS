/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 11, 2001: Created.
***/
#include <stdio.h>
#include <stdarg.h>
#include "fmessage.h"

#ifndef FORMAT_MESSAGE_BUFFER_SIZE
#define FORMAT_MESSAGE_BUFFER_SIZE 4096
#endif

const char * formatMessage(const char * message, ...)
{
  static char buffer[FORMAT_MESSAGE_BUFFER_SIZE];
  va_list ap;
  va_start(ap, message);
  vsnprintf(buffer, FORMAT_MESSAGE_BUFFER_SIZE, message, ap);
  va_end(ap);
  return buffer;
}
