/* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

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
