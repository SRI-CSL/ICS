#include <stdio.h>
#include <signal.h>

struct sigaction lisp_sigill_handler, ocaml_sigill_handler;
struct sigaction lisp_sigstksz_handler, ocaml_sigstksz_handler;
struct sigaction lisp_sigsegv_handler, ocaml_sigsegv_handler;
struct sigaction lisp_sigtrap_handler, ocaml_sigtrap_handler;
struct sigaction lisp_sigint_handler, ocaml_sigint_handler;

void set_ocaml_handlers(void) {
  sigaction(SIGILL, &ocaml_sigill_handler, &lisp_sigill_handler);
  sigaction(SIGSTKSZ, &ocaml_sigstksz_handler, &lisp_sigstksz_handler);
  sigaction(SIGSEGV, &ocaml_sigsegv_handler, &lisp_sigsegv_handler);
  sigaction(SIGTRAP, &ocaml_sigtrap_handler, &lisp_sigtrap_handler);
  sigaction(SIGINT, &ocaml_sigint_handler, &lisp_sigint_handler);
}

void restore_lisp_handlers(void) {
  sigaction(SIGILL, &lisp_sigill_handler, &ocaml_sigill_handler);
  sigaction(SIGSTKSZ, &lisp_sigstksz_handler, &ocaml_sigstksz_handler);
  sigaction(SIGSEGV, &lisp_sigsegv_handler, &ocaml_sigsegv_handler);
  sigaction(SIGTRAP, &lisp_sigtrap_handler, &ocaml_sigtrap_handler);
  sigaction(SIGINT, &lisp_sigint_handler, &ocaml_sigint_handler);
}
