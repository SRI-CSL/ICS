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
#include "StringBuffer.h"
#include "safealloc.h"
#include <string.h>
#include <stdio.h>

#define STRING_BUFFER_TMP_BUFFER_SIZE 64
char * gStringBufferTmpBuffer = (char *) xmalloc(STRING_BUFFER_TMP_BUFFER_SIZE);

StringBuffer::StringBuffer(unsigned int initialSize)
{
  data = (char *) xmalloc(initialSize);
  realSize = initialSize;
  currPos = 0;
}

StringBuffer::StringBuffer()
{
  data = (char *) xmalloc(STRING_BUFFER_INITIAL_SIZE);
  realSize = STRING_BUFFER_INITIAL_SIZE;
  currPos = 0;
}

StringBuffer::~StringBuffer()
{
  free(data);
}

void StringBuffer::reset()
{
  currPos = 0;
}

void StringBuffer::expand()
{
  char * tmp = (char *) xmalloc(realSize*2);
  memcpy(tmp, data, currPos);
  free(data);
  realSize = realSize * 2;
  data = tmp;
}

void StringBuffer::append (const char * s)
{
  unsigned int len = strlen(s);
  while (currPos + len > realSize)
		expand();
  memcpy(data + currPos, s, len);
  currPos += len;
}

void StringBuffer::append (int i)
{
  sprintf(gStringBufferTmpBuffer, "%d", i);
  append(gStringBufferTmpBuffer);
}

void StringBuffer::append (unsigned int i)
{
  sprintf(gStringBufferTmpBuffer, "%d", i);
  append(gStringBufferTmpBuffer);
}

void StringBuffer::append (long i)
{
  sprintf(gStringBufferTmpBuffer, "%ld", i);
  append(gStringBufferTmpBuffer);
}
 
void StringBuffer::append (char c)
{
  if (currPos >= realSize)
		expand();
  data[currPos] = c;
  currPos++;
}

void StringBuffer::append (double d)
{
  sprintf(gStringBufferTmpBuffer, "%f", d);
  append(gStringBufferTmpBuffer);
}

void StringBuffer::append (float f)
{
  sprintf(gStringBufferTmpBuffer, "%f", f);
  append(gStringBufferTmpBuffer);
}

void StringBuffer::append (bool b)
{
  append(b ? "true" : "false");
}

const char * StringBuffer::cstr() const
{
  StringBuffer * const localThis = (StringBuffer * const) this;
  if (currPos >= realSize)
		localThis->expand();
  localThis->data[currPos] = 0;
  return data;
}




