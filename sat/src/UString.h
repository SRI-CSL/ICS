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
     demoura - May 14, 2001: Created.
***/

#ifndef USTRING_H
#define USTRING_H

#include <iostream.h>
#include <assert.h>
#include <hashtable.h>
#include "StringBuffer.h"

/*
  unique strings.
  the contents of s1 is equals to the contentes of s2 IFF s1 == s2.
 */
class UString
{
  const char * data;
 public:
  static const char * uniqueString(const char *);
  static const UString NullString;
  UString();
  UString(const UString & s);
  UString(const char * s);
  UString(char c);
  UString(int i);

  size_t length() const;

  char operator[](size_t pos) const {
		assert(pos < length());
		return data[pos];
  }

  UString & append (const UString & s);
  UString & append (const char * s) {
		return append(UString(s));
  }
  UString & append (char c) {
		return append(UString(c));
  }
  UString & append (int i);
  
  bool compare(const UString & s) const {
		return data == s.data;
  }
  bool compare(const char * s) const {
		return strcmp(data, s) == 0;
  }
  bool compare(char c) const {
		return data[0] == c && data[1] == 0;
  }

  int indexOf(char c);

  const char * cstr() const {
		return data;
  }

  UString & operator+=(const UString & s) {
		return append(s); 
  }
  UString & operator+=(const char * s) {
		return append(s);
  }
  UString & operator+=(char c) {
		return append(c);
  }
  UString & operator+=(int i) { 
  	return append(i);
  }

  UString& operator= (const UString & s)
  { data = s.data; return *this; }
  UString& operator= (const char * s)
  { UString tmp(s); data = tmp.data; return *this; }
  UString& operator= (char c)
  { UString tmp(c); data = tmp.data; return *this; }

  UString substr(size_t pos, size_t len) const;

  bool less(const UString & s) const;
  bool less(const char * s) const;

  static void resetMemory();

};

inline UString operator+(const UString & s1, const UString & s2)
{
  UString str(s1);
  str.append(s2);
  return str;
}

inline UString operator+(const UString & s1, const char * s2)
{
  UString str(s1);
  str.append(s2);
  return str;
}

UString operator+(const char * s1, const UString & s2);

inline UString operator+(const UString & s, char c)
{
  UString str(s);
  str.append(c);
  return str;
}

inline UString operator+(char c, const UString & s)
{
  UString str(c);
  str.append(s);
  return str;
}

inline UString operator+(const UString & s, int i)
{
  UString str(s);
  str.append(i);
  return str;
}

inline UString operator+(int i, const UString & s)
{
  UString str(i);
  str.append(s);
  return str;
}

inline bool operator==(const UString & s1, const UString & s2)
{
  return s1.compare(s2);
}

inline bool operator==(const UString & s1, const char * s2)
{
  return s1.compare(s2);
}

inline bool operator==(const char * s1, const UString & s2)
{
  return s2.compare(s1);
}

inline bool operator!=(const UString & s1, const UString & s2)
{
  return !s1.compare(s2);
}

inline bool operator!=(const UString & s1, const char * s2)
{
  return !s1.compare(s2);
}

inline bool operator!=(const char * s1, const UString & s2)
{
  return !s2.compare(s1);
}

inline bool operator<(const UString & s1, const UString & s2)
{
  return s1.less(s2); 
}

inline bool operator<(const UString & s1, const char * s2)
{
  return s1.less(s2); 
}

bool operator<(const char * s1, const UString & s2);

inline ostream& operator<<(ostream& target, const UString& toDump)
{
  target << toDump.cstr();
  return target;
}

inline StringBuffer& operator<<(StringBuffer& target, const UString& toDump)
{
  target << toDump.cstr();
  return target;
}

struct hashUString
{
	static hash<const char *> proc;
  size_t operator()(const UString & s) const { return proc(s.cstr()); }
};

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

#endif /* USTRING_H */
