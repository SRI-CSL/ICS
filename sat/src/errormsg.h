/* Copyright (c) SRI International 2002. */
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
