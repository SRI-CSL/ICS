/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 25, 2002: Created.
***/

#ifndef LPEXCEPTION_H
#define LPEXCEPTION_H

#include<iostream.h>

class LPException {	
protected:
  char * message;
public:
  LPException(const char * msg);
	LPException() { message = NULL; }
	virtual ~LPException();
	virtual void dump_message(ostream& target) const;
};

#endif /* LPEXCEPTION_H */
