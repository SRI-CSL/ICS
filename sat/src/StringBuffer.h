/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 16, 2001: Created.
***/

#ifndef STRINGBUFFER_H
#define STRINGBUFFER_H

#ifndef STRING_BUFFER_INITIAL_SIZE
#define STRING_BUFFER_INITIAL_SIZE 1024
#endif

#ifndef STRING_BUFFER_INITIAL_SMALL_SIZE
#define STRING_BUFFER_INITIAL_SMALL_SIZE 64
#endif

class StringBuffer
{
  char * data;
  unsigned int currPos;
  unsigned int realSize;
  void expand();
 public:
  StringBuffer();
  StringBuffer(unsigned int initialSize);
  ~StringBuffer();
  void reset();
  void append (const char *);
	void append (unsigned int i);
  void append (int i);
  void append (char c);
  void append (double d);
  void append (float f);
  void append (bool b);
	void append (long i);
  bool atBeginning() { return currPos == 0; }
  const char * cstr() const;
};

inline StringBuffer& operator<<(StringBuffer& target, const char * s)
{ target.append(s); return target; }
inline StringBuffer& operator<<(StringBuffer& target, char c)
{ target.append(c); return target; }
inline StringBuffer& operator<<(StringBuffer& target, int i)
{ target.append(i); return target; }
inline StringBuffer& operator<<(StringBuffer& target, long i)
{ target.append(i); return target; }
inline StringBuffer& operator<<(StringBuffer& target, float f)
{ target.append(f); return target; }
inline StringBuffer& operator<<(StringBuffer& target, bool b)
{ target.append(b); return target; }
inline StringBuffer& operator<<(StringBuffer& target, double d)
{ target.append(d); return target; }
inline StringBuffer& operator<<(StringBuffer& target, const StringBuffer & buffer)
{ target.append(buffer.cstr()); return target; }
typedef StringBuffer& (*stringBufferMod)(StringBuffer&);
inline StringBuffer& operator<<(StringBuffer& target, stringBufferMod mod)
{ return mod(target); }
inline StringBuffer& endl(StringBuffer& out)
{ out << "\n";   return out; }
inline StringBuffer& ends(StringBuffer& out)
{ out << '\0'; return out; }

#endif /* STRINGBUFFER_H */
