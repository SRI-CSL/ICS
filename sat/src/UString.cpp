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

#include "UString.h"
#include <stdio.h>
#include <string.h>
#include <hashtable.h>
#include <hash_set.h>
#include <list.h>
#include "util.h"
#include "safealloc.h"

hash_set<const char *, hash<const char *>, eqstr> gUStringSet; 
#ifndef USTRING_BLOCK_SIZE
#define USTRING_BLOCK_SIZE 8192
#endif

list<char *> gUStringMemoryBlocks;
char * gUStringCurrentBlock = (char *) xmalloc(USTRING_BLOCK_SIZE);
int gUStringCurrPosInBlock = 0;
int gUStringCurrBlockSize = USTRING_BLOCK_SIZE;

#if defined(LP_TRACE_MEMORY) || defined(LP_TRACE_ALL)
static unsigned int num_ustring_allocated_blocks = 1;
#endif


const char * UString::uniqueString(const char * s)
{
  hash_set<const char *, hash<const char *>, eqstr>::const_iterator it = gUStringSet.find(s);
  if (it != gUStringSet.end())
	return *it;
  else {
		int size = strlen(s) + 1;
		const char * data;
		if (size <= gUStringCurrBlockSize - gUStringCurrPosInBlock) {
			data = gUStringCurrentBlock + gUStringCurrPosInBlock;
			memcpy((void *) data, s, size);
			gUStringCurrPosInBlock += size;
		}
		else {
			gUStringMemoryBlocks.push_back(gUStringCurrentBlock);
			gUStringCurrBlockSize = USTRING_BLOCK_SIZE >= size ? USTRING_BLOCK_SIZE : size*2;
			gUStringCurrentBlock = (char *) xmalloc(gUStringCurrBlockSize);
			data = gUStringCurrentBlock;
			memcpy((void *) data, s, size);
			gUStringCurrPosInBlock = size;
			MEM_TRACE(num_ustring_allocated_blocks++;
								ctrace << "[memory] allocating UString block, memory allocated so far = " << 
								((USTRING_BLOCK_SIZE * num_ustring_allocated_blocks) / 1024) << " Kb\n";);
		}
		gUStringSet.insert(data);
		return data;
  }
}

const UString UString::NullString;

UString::UString()
{
  static const char * nullString = uniqueString("");
  data = nullString;
}

UString::UString(const UString & s)
{
  data = s.data;
}

UString::UString(const char * s)
{
  data = uniqueString(s);
}

char * gUStringBuffer = (char *) xmalloc(USTRING_BLOCK_SIZE);
int gUStringBufferSize = USTRING_BLOCK_SIZE;

UString::UString(char c)
{
  gUStringBuffer[0] = c;
  gUStringBuffer[1] = 0;
  data = uniqueString(gUStringBuffer);
}

UString::UString(int i)
{
  static char buffer[32];
  sprintf(buffer, "%d", i);
  data = uniqueString(buffer);
}

size_t UString::length() const
{
  return strlen(data);
}

#define CHECK_gUStringBufferSize(SIZE)					\
if (SIZE > gUStringBufferSize) {								\
  free(gUStringBuffer);													\
  gUStringBuffer = (char *) xmalloc(SIZE);			\
  gUStringBufferSize = SIZE;										\
}

UString & UString::append (const UString & s)
{
  int len1 = strlen(data);
  int len2 = strlen(s.data);
  int newSize = len1 + len2 + 1;
  
  CHECK_gUStringBufferSize(newSize);

  memcpy(gUStringBuffer, data, len1);
  memcpy(gUStringBuffer + len1, s.data, len2 + 1);
  
  data = uniqueString(gUStringBuffer);
  return *this;
}

UString operator+(const char * s1, const UString & s2)
{
  int len1 = strlen(s1);
  int len2 = strlen(s2.cstr());
  int newSize = len1 + len2 + 1;
  
  CHECK_gUStringBufferSize(newSize);

  memcpy(gUStringBuffer, s1, len1);
  memcpy(gUStringBuffer + len1, s2.cstr(), len2 + 1);
  
  return UString(gUStringBuffer);
}

UString & UString::append (int i)
{
  int len = strlen(data);
  CHECK_gUStringBufferSize(len + 32);
  sprintf(gUStringBuffer, "%s%d", data, i);
  data = uniqueString(gUStringBuffer);
  return *this;
}

UString UString::substr(size_t pos, size_t len) const
{
  int newSize = len + 1;

  CHECK_gUStringBufferSize(newSize);

  memcpy(gUStringBuffer, data + pos, len);
  gUStringBuffer[len] = 0;

  return UString(gUStringBuffer);
}

bool UString::less(const UString & s) const
{
  return strcmp(data, s.data) < 0;
}

bool UString::less(const char * s) const
{
  return strcmp(data, s) < 0;
}

bool operator<(const char * s1, const UString & s2)
{
  return strcmp(s1, s2.cstr()) < 0;
}

void UString::resetMemory()
{
  free(gUStringCurrentBlock);
  list<char *>::iterator it = gUStringMemoryBlocks.begin();
  while (it != gUStringMemoryBlocks.end()) {
		free(*it);
		it++;
  }
}

int UString::indexOf(char c)
{
  for (int i = 0; data[i] != 0; ++i) {
	if (data[i] == c)
	  return i;
  }
  return -1;
}

hash<const char *> hashUString::proc;
