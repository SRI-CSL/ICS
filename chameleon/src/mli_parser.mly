/* A simplified parser for caml types. */

%{
  open Mli_types
%}

%token <string> IDENT
%token <string> LONGIDENT
%token INT BOOL UNIT STRING
%token QUOTE LPAR RPAR ARROW STAR COMMA EOF
%right prec_type_arrow
%type <Mli_types.mlarity> mlarity
%start mlarity

%%
mlarity:
  | core_type EOF { $1 }
;

core_type:
  | simple_core_type          { { args = []; result = $1 } }
  | core_type ARROW core_type %prec prec_type_arrow 
     { let arg = if $1.args = [] then $1.result else Abstract in
       { args = arg :: $3.args; result = $3.result } }
  | core_type_tuple           { { args = []; result = Abstract } }
;

simple_core_type:
  | INT                                  { Int }
  | BOOL                                 { Bool }
  | UNIT                                 { Unit }
  | STRING                               { String }
  | QUOTE IDENT                          { Abstract }
  | IDENT                                { Abstract }
  | simple_core_type IDENT               { Abstract }
  | LPAR core_type_comma_list RPAR IDENT { Abstract }
  | LPAR core_type RPAR                  { Abstract }

;

core_type_tuple:
  | simple_core_type STAR simple_core_type      { () }
  | core_type_tuple  STAR simple_core_type      { () }
;

core_type_comma_list:
  | core_type            COMMA core_type        { () }
  | core_type_comma_list COMMA core_type        { () }
;

%%

  
