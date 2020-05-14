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

#ifndef ERRORMSG_H
#define ERRORMSG_H

#include"LPException.h"

typedef struct em_error_state_ em_error_state;

struct em_error_state_ {
  void   *line_list; /* opaque line number list */
  int    line_num;
  bool   show_errors;
  bool   any_errors;
  char*  file_name;
};

extern bool em_show_errors;
extern bool em_any_errors;
extern int  em_tokpos;

void em_newline(void);

void em_message(char * message,...);
void em_info_message(int pos, char * message,...);
void em_simple_error(char * message, ...);
void em_error(int pos, char * message, ...);
void em_show_exception(int pos, LPException & ex);
void em_reset(char * in_file_name);
void em_resetsans(void);

void em_free_error_state();
void em_save_error_state(em_error_state *es);
em_error_state * em_copy_error_state();
void em_restore_error_state(em_error_state es);

int em_get_line(int pos);
int em_get_column(int pos);
char * em_get_file_name();

#endif /* ERRORMSG_H */
