#include <stdio.h>
#include <string.h>
#include "/homes/ruess/ics2.1/include/icsapi.h"

extern void ics_caml_startup(int full, char** argv);

void ics_error(char * funname, char * message) {
  fprintf(stderr, "ICS error at %s : %s", funname, message);
  exit(1);
}

int main (int argc, char ** argv) {
  termvar x, y;
  rat p, q;
  funsym f, g;
  term s, t, r;
  state st;
  context ctxt;
  ics_init();
  x = ics_var_of_string("x");
  y = ics_var_of_string("y");
  p = ics_propvar_of_string("p");
  q = ics_propvar_of_string("q");
  f = ics_funsym_of_string("f");
  g = ics_funsym_of_string("g");
  s = ics_term_mk_var(x);
  t = ics_term_mk_int(23);
  r = ics_term_mk_add(s, t);
  fprintf(stderr,"\nTerm r: %s ", ics_term_to_string(r));
  fflush(stdout);
  fprintf(stderr, "\nx: variable name %s", ics_var_to_string(x));
  fprintf(stderr, "\ny: variable name %s", ics_var_to_string(y));
  ics_process(ics_formula_mk_eq(s, t));
  st = ics_finalize();
  ctxt = ics_context();
  while (ics_context_is_empty(ctxt)) {
    fprintf(stderr, "\nCtxt: %s", 
	    ics_formula_to_string(ics_context_head(ctxt)));
    ctxt = ics_context_tail(ctxt);
  };
  fflush(stderr);
  return (int) 0;
}
