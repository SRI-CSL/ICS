
/* Caml errors sent to Lisp. */

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <string.h>

static void (*lisp_error_function_address)();

void ocaml_error(char* s, char* msg) {
  (*lisp_error_function_address)(s,msg);
}

void register_lisp_error_function(int index) {
  lisp_error_function_address = (void (*)()) lisp_call_address(index);
}

