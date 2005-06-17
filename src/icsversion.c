#include <stdio.h> 
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>

char* version = VERSION;
char* arch = ARCHITECTURE;
char* date = BUILDDATE;
int debug = DEBUG;
char* flags = COMPILATION_FLAGS;

void print_version() {
  CAMLparam0();
  fprintf(stdout, "ICS %s (%s, %s)", version, arch, date); 
  if (debug > 0) {
    fprintf(stdout, "\nDebugging enabled; level = %d.", debug);
  };
  fflush(stdout);
  CAMLreturn0;
}

void eprint_version() {
  CAMLparam0();
  fprintf(stderr,"ICS %s (%s, %s)", version, arch, date); 
  if (debug > 0) {
    fprintf(stderr,"\nDebugging enabled; level = %d.", debug);
  };
  fflush(stderr);
  CAMLreturn0;
}

int debug_value () {
  CAMLparam0();
  CAMLreturn(Val_int(debug));
}
