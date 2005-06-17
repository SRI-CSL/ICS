/* Caml errors sent to Lisp. */

/* add a global variable which is set in init and depending on */
/* its value for ics_init dispatch. */

static void (*lisp_error_function_address)();

extern int lisp_call_address(int);

void ics_error(char* funname, char* msg) {
  (*lisp_error_function_address)(funname,msg);
}

void register_lisp_error_function(int index) {
  lisp_error_function_address = (void (*)()) lisp_call_address(index);
}

/*** Some Signalling issues for Lisp ***/

#ifndef ICS_CYGWIN
#include <signal.h>
struct sigaction lisp_sigill_handler, ocaml_sigill_handler;
struct sigaction lisp_sigstksz_handler, ocaml_sigstksz_handler;
struct sigaction lisp_sigsegv_handler, ocaml_sigsegv_handler;
struct sigaction lisp_sigtrap_handler, ocaml_sigtrap_handler;
struct sigaction lisp_sigint_handler, ocaml_sigint_handler;
#endif

void set_ocaml_handlers(void) {
#ifndef ICS_CYGWIN
    sigaction(SIGILL, &ocaml_sigill_handler, &lisp_sigill_handler);
    sigaction(SIGSTKSZ, &ocaml_sigstksz_handler, &lisp_sigstksz_handler);
    sigaction(SIGSEGV, &ocaml_sigsegv_handler, &lisp_sigsegv_handler);
    sigaction(SIGTRAP, &ocaml_sigtrap_handler, &lisp_sigtrap_handler);
    sigaction(SIGINT, &ocaml_sigint_handler, &lisp_sigint_handler);
#endif
}

void restore_lisp_handlers(void) {
#ifndef ICS_CYGWIN
    sigaction(SIGILL, &lisp_sigill_handler, &ocaml_sigill_handler);
    sigaction(SIGSTKSZ, &lisp_sigstksz_handler, &ocaml_sigstksz_handler);
    sigaction(SIGSEGV, &lisp_sigsegv_handler, &ocaml_sigsegv_handler);
    sigaction(SIGTRAP, &lisp_sigtrap_handler, &ocaml_sigtrap_handler);
    sigaction(SIGINT, &lisp_sigint_handler, &ocaml_sigint_handler);
#endif
}
