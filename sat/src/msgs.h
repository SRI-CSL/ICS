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
     demoura - Mar 6, 2002: Created.
***/

#ifndef MSGS_H
#define MSGS_H

#define MAX_STRING_LENGTH_EXCEEDED_TRUNCATING_TO_CHARS "Warning: maximum string length exceeded, truncating to %d chars"
#define ILLEGAL_TOKEN "Error: illegal token `%c' in input"
#define UNTERMINATED_STRING_ESCAPE_SEQUENCE "Error: unterminated string escape sequence"
#define EOF_ENCOUNTERED_WITHIN_STRING "Error: EOF encountered within string"
#define EOF_ENCOUNTERED_WITHIN_COMMENT "Error: EOF encountered within comment"
#define UNCLOSED_STRING "Error: unclosed string"
#define MAX_STRING_LENGTH_EXCEEDED_TRUNCATING_TO_CHARS "Warning: maximum string length exceeded, truncating to %d chars"
#define STRING_LITERAL_ENDED_WITH_NEWLINE "Error: string literal ended with newline"
#define ERROR_OPENING_FILE "Error: opening file `%s'"
#define ERRORS_PARSING_FILE "Error(s): parsing file `%s'... aborting compilation..."

#endif /* MSGS_H */


