/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 25, 2002: Created.
***/

#include "LPException.h"
#include "safealloc.h"

LPException::LPException(const char * msg)
{
	message = xstrdup(msg);
}

LPException::~LPException() 
{
	free(message);
}

void LPException::dump_message(ostream& target) const
{
	if (message != NULL)
		target << message;
	else
		target << "no-message";
}


