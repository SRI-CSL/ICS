/* A parser for terms. */

%{
  open Mpa
  open Ics

  let mk_tuple = function
    | [] -> assert false
    | [t] -> t
    | l -> tup l
%}

%token CAN SOLVE ASSERT FIND CHECK LIFT USE UNIVERSE SIGMA VERBOSE RESET DROP NORM COMPARE
%token UNIFY FOL

%token <string> IDENT
%token <string> CONST
%token <string> CONSTBV

%token LPAR RPAR  LBRA RBRA LCUR RCUR
%token COLON BAR COMMA SEMI DOT ASSIGN

%token EQUAL DISEQ
%token TRUE FALSE AND OR XOR IMPLIES IFF NOT IF THEN ELSE FI
%token PLUS MINUS TIMES DIVIDE FLOOR
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token IN NOTIN EMPTY FULL COMPL UNION INTER DIFF SYMDIFF
%token BVEPS BVZERO BVONE BVCONC BVAND BVOR BVXOR
%token PROJ
%token INTEGER_PRED UNSIGNED
%token FORALL EXISTS

%right OR XOR IMPLIES
%left IFF AND
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%left UNION
%left INTER
%left DIFF SYMDIFF
%nonassoc IN NOTIN
%left MINUS PLUS
%left TIMES DIVIDE
%left LBRA
%nonassoc prec_unary

%type <unit> command
%type <Ics.term> term
%type <Ics.term * Ics.term> equation
%type <int> width

%start term
%start equation
%start command

%%

term:
  ident                               { let (x,tg) = $1 in
					match tg with
					  | All -> var x
					  | Int -> zvar x }
| CONST                               { num (num_of_string $1) }
| IDENT LPAR term_comma_list RPAR     { app (var $1) $3 }
| IF term THEN term ELSE term FI   { ite $2 $4 $6 }
| prop                                { $1 }
| arith                               { $1 }
| tuple                               { $1 }
| array                               { $1 }
| set                                 { $1 }
| bv                                  { $1 }
| LPAR term RPAR                      { $2 }
;

ident: IDENT   { $1, let c = String.get $1 0 in
		       if (c >= 'A' && c <= 'Z') then Int else All }


arith: 
  term PLUS   term                  { plus2 $1 $3 }
| term MINUS  term                  { minus $1 $3 }
| term TIMES  term                  { times [$1;$3] }
| CONST DIVIDE CONST                  { div (num_of_string $1) (num_of_string $3) }
| MINUS term %prec prec_unary        { unary_minus $2 }
;

array: 
  term LBRA term RBRA               { app $1 [$3] }
| term LBRA term ASSIGN term RBRA   { update $1 $3 $5 }
;

tuple: 
  LPAR term_comma_list RPAR           { mk_tuple $2 }
| PROJ LBRA CONST COMMA CONST RBRA  LPAR term RPAR
                             { proj (int_of_string $3) (int_of_string $5) $8 }
;

set:
  EMPTY                             { empty_set 0 }
| FULL                              { full_set 0 }
| term UNION term                   { union 0 $1 $3 }
| term DIFF term                    { diff 0 $1 $3 }
| term SYMDIFF term                 { sym_diff 0 $1 $3 }
| term INTER term                   { inter 0 $1 $3 }
| COMPL term %prec prec_unary       { compl 0 $2 }
;

bv: 
  CONSTBV                             { bv_const $1 }
| BVCONC LBRA width COMMA width RBRA LPAR term COMMA term RPAR
	                              { bv_conc ($3,$8) ($5,$10) }
| BVAND LBRA width RBRA LPAR term COMMA term  RPAR
			              { bv_and $3 $6 $8 }
| BVOR LBRA width RBRA LPAR term COMMA term  RPAR
			              { bv_or $3 $6 $8 }
| BVXOR LBRA width RBRA LPAR term COMMA term  RPAR
			              { bv_xor $3 $6 $8 }
| term LBRA width COMMA width COMMA width RBRA
                                      { bv_extr ($3,$1) $5 $7 }
;

atom:
  term EQUAL term                   { equal $1 $3 }
| term DISEQ term                   { diseq $1 $3 }
| term LESS term                    { lt $1 $3 }
| term GREATER term                 { gt $1 $3 }
| term LESSOREQUAL term             { le $1 $3 }
| term GREATEROREQUAL term          { ge $1 $3 }
| INTEGER_PRED LPAR term RPAR       { integer_pred $3 }
| term IN term                      { mem $1 $3 }
| term NOTIN term                   { neg (mem $1 $3) }
;

prop: 
  TRUE                              { ptrue() }
| FALSE                             { pfalse() }
| atom                              { $1 }
| term AND term                     { $1 & $3 }
| term OR term                      { $1 || $3 }
| term XOR term                     { xor $1 $3 }
| term IMPLIES term                 { $1 => $3 }
| term IFF term                     { $1 <=> $3 }
| NOT term %prec prec_unary         { neg $2 }
;       

fol:
  term                                    { $1 }
| FORALL LPAR ident_comma_list RPAR fol   { forall $3 $5 }
| EXISTS LPAR ident_comma_list RPAR fol   { exists $3 $5 }
;

width: CONST { int_of_string $1 } 

term_comma_list:
  term                       { [$1] }
| term COMMA term_comma_list { $1 :: $3 }
;

ident_comma_list:
  ident                        { [$1] }
| ident COMMA ident_comma_list { $1 :: $3 }
;

terms:
  term             { [$1] }
| term SEMI terms  { $1 :: $3 }
;

equation:
term EQUAL term    { ($1,$3) }
;

optterm:           { None }
| term             { Some($1) }
;

command:
  CAN term       DOT  { Cmd.can $2 }
| NORM term      DOT  { Cmd.norm $2 }
| COMPARE term COMMA term DOT { Cmd.compare ($2,$4)}
| SOLVE equation DOT { Cmd.solve $2 }
| ASSERT terms   DOT { Cmd.processl $2 }
| FIND optterm   DOT { Cmd.find $2 }
| CHECK term     DOT  { Cmd.check $2 }
| UNIVERSE optterm DOT { Cmd.universe $2 }
| USE optterm    DOT { Cmd.use $2 }
| FIND optterm   DOT { Cmd.find $2 }
| SIGMA term     DOT  { Cmd.sigma $2 }
| POLARITY term  DOT { Cmd.polarity $2 }
| VERBOSE CONST  DOT { Cmd.verbose (int_of_string $2) }
| RESET          DOT { Cmd.reset () }
| DROP           DOT { Cmd.drop () }
| UNIFY equation DOT { Cmd.unify $2 }
| FOL fol        DOT { Cmd.fol $2 }   
;

%%
