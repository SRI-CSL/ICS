/* A simplified parser for interface declarations. */

%{
  open Mli_types
%}

%token <string> IDENT
%token <string> LONGIDENT
%token VAL COLON
%token INT BOOL UNIT STRING VALUE ARROW EOF
%right ARROW
%type <Mli_types.t> signature
%type <string * Mli_types.t> declaration
%start declaration
%start signature

%%

declaration: VAL IDENT COLON signature { ($2, $4) }

signature:
  constant EOF  { $1 } 
| functional EOF { $1 }
;

constant : ground            { { dom = []; cod = $1 } }

functional: dom ARROW ground { { dom = List.rev $1; cod = $3 } }

dom:
  ground           { [$1] }
| dom ARROW ground { $3 :: $1 }
;

ground:
  | INT                                  { Int }
  | BOOL                                 { Bool }
  | UNIT                                 { Unit }
  | STRING                               { String }
  | VALUE                                { Value }
;

%%

  
