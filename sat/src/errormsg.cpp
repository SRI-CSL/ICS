/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - Mar 6, 2002: Created.
***/
#include <stdio.h>
#include <stdarg.h>
#include "safealloc.h"
#include "errormsg.h"

bool em_show_errors;
bool em_any_errors; 
int  em_tokpos;

static char * file_name = NULL;
static int line_num = 0;

/* private structure to represent a chunck of a list of line numbers;
   this system takes the burden of allocating 8 bytes for each line
   number off of malloc, and also saves considerable space. */
#define LINELISTCHUNK_SIZE 32
typedef struct line_list_chunck_
{
  int number[LINELISTCHUNK_SIZE];
  int index; /* index of the next free slot in number[] */
  struct line_list_chunck_ *next;
} line_list_chunck, * line_list_chunck_ptr;

/* few functions to manipulate line lists */
static void insert_line_list(line_list_chunck_ptr * io_head, int in_position); 
static void free_line_list(line_list_chunck_ptr * io_head);
static void lookup_line_offset(line_list_chunck_ptr * io_head, int in_position, int * out_line_number, int * out_offset_on_line);

line_list_chunck_ptr line_list = NULL;

void em_newline(void)
{
  if (em_show_errors)
	{
	  line_num++;
	  insert_line_list(&line_list, em_tokpos);
	}
} /* em_newline */

void em_message(char * message, ...)
{
  va_list ap;
  va_start(ap, message);
  vfprintf(stderr, message, ap);
  va_end(ap);
}

void em_info_message(int pos, char * message,...)
{
  va_list ap;
  int line_number, offset_on_line;
  line_number = line_num; 

  if (file_name)
		fprintf(stderr, "%s:", file_name);
  else
		fprintf(stderr, "(no file name):");
  
  lookup_line_offset(&line_list, pos, &line_number,
					 &offset_on_line);
  fprintf(stderr, "%d:%d: ", line_number, offset_on_line);
  /* print the message */
  va_start(ap, message);
  vfprintf(stderr, message, ap);
  va_end(ap);
  fprintf(stderr,"\n");
}

void em_simple_error(char * message, ...)
{
  va_list ap;
  em_any_errors = true;
  if (em_show_errors)
	{
	  va_start(ap, message);
	  vfprintf(stderr, message, ap);
	  va_end(ap);
	  fprintf(stderr,"\n");
	}
}

int em_get_line(int pos)
{
  int line_number, offset_on_line;
  line_number = line_num; 
  lookup_line_offset(&line_list, pos, &line_number,
					 &offset_on_line);
  return line_number;
}

int em_get_column(int pos)
{
  int line_number, offset_on_line;
  line_number = line_num; 
  lookup_line_offset(&line_list, pos, &line_number,
					 &offset_on_line);
  return offset_on_line;
}

void em_show_exception(int pos, LPException & ex)
{
  int line_number, offset_on_line;
  line_number = line_num; 
  em_any_errors = true;

  if (em_show_errors)
	{
	  if (file_name)
			cerr << file_name << " ";
	  else
			cerr << "(no file name): ";

	  lookup_line_offset(&line_list, pos, &line_number,
											 &offset_on_line);
	  cerr << line_number << ":" << offset_on_line << ": ";
	  /* print the exception error message */
		ex.dump_message(cerr);
		cerr << ".\n";
	}
}

void em_error(int pos, char * message,...)
{
  va_list ap;
  int line_number, offset_on_line;
  line_number = line_num; 

  em_any_errors = true;

  if (em_show_errors)
	{
	  if (file_name)
			fprintf(stderr, "%s:", file_name);
	  else
			fprintf(stderr, "(no file name):");

	  lookup_line_offset(&line_list, pos, &line_number,
		  				 &offset_on_line);
	  fprintf(stderr, "%d:%d: ", line_number, offset_on_line);
	  /* print the error message */
	  va_start(ap, message);
	  vfprintf(stderr, message, ap);
	  va_end(ap);
	  fprintf(stderr,"\n");
	}

} /* em_error */

void em_reset(char * in_file_name)
{
	em_free_error_state();

  em_show_errors = true;
  em_any_errors  = false;
  em_tokpos      = 0;

	if (in_file_name != NULL)
		file_name = xstrdup(in_file_name);
	else
		file_name = NULL;
  line_num  = 0;
  line_list = NULL;
  
  em_newline();  /* we're at the first line */

} /* em_reset */

void em_resetsans(void)
{
  em_show_errors = false;
} /* em_resetsans */

void em_free_error_state()
{
	if (file_name != NULL)
		free(file_name);
	if (line_list != NULL)
		free_line_list(&line_list);
} /* em_free_error_state */

void em_save_error_state(em_error_state * es)
{
  es->line_list = line_list;
  es->line_num  = line_num;
  es->file_name = file_name;
  es->show_errors = em_show_errors;
  es->any_errors  = em_any_errors;
} /* em_save_error_state */

em_error_state * em_copy_error_state()
{
  em_error_state * es = (em_error_state *) xmalloc(sizeof(*es));
  em_save_error_state(es);
  return es;
}

void em_restore_error_state(em_error_state es)
{
  line_list = (line_list_chunck_ptr) es.line_list;
  line_num  = es.line_num;
  file_name = es.file_name;
  em_show_errors = es.show_errors;
  em_any_errors  = (es.any_errors || em_any_errors);
} /* em_restore_error_state */


static void insert_line_list(line_list_chunck_ptr * io_head, int in_position)
{
  /* if (the list head is NULL or the chunck is full) then */
  /* allocate a new chunck */
  if (*io_head == NULL || (**io_head).index >= LINELISTCHUNK_SIZE)
	{
	  line_list_chunck_ptr new_chuck = (line_list_chunck_ptr) xmalloc(sizeof(line_list_chunck));
	  new_chuck->index = 0;
	  new_chuck->next = *io_head; /* link the new chunck to the current one */
	  *io_head = new_chuck; /* set the new chunck as the current one */
	}
  /* store the position in the line list, and incremente the index */
  (**io_head).number[(**io_head).index++] = in_position;
} /* insert_line_list */

static void free_line_list(line_list_chunck_ptr * io_head)
{
  while(*io_head)
	{
	  line_list_chunck_ptr next = (**io_head).next;
	  free((void *)*io_head);
	  *io_head = next;
	}
} /* free_line_list */

static void lookup_line_offset(line_list_chunck_ptr * io_head, int in_position, 
							   int * out_line_number, int * out_offset_on_line)
{
   /* the tactic here: search backward until we find a position that's
      before the query position.  We then return the number of lines we
      counted backward to get there.  If out_offset_on_line is not NULL,
      we set it to the difference between the query position and the 
      position of the beginning of the discovered line. */
  int line_offset = 0;
  line_list_chunck_ptr chunck = *io_head;
  while (chunck)
	{
	  int index = chunck->index;
	  while (--index >= 0)
		{
		  if (chunck->number[index] <= in_position)
			{ /* found */
			  if (out_offset_on_line)
				  *out_offset_on_line = in_position - chunck->number[index];
			  if (out_line_number)
				*out_line_number -= line_offset;
			  return;
			}
		  line_offset++;
		}
	  chunck = chunck->next;
	}
  /* offsets not found */
  if (out_offset_on_line)
	*out_offset_on_line = 0;
  if (out_line_number)
	*out_line_number = 0;
} /* lookup_line_offset */

char * em_get_file_name()
{
  return file_name;
}
