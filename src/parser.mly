/* A parser for terms. */

%{
  open Mpa
  open Ics

  let mk_tuple = function
    | [] -> assert false
    | [t] -> t
    | l -> mk_tup l

  let get_width (a,w) =
    match Ics.width_of a,w with
      | Some(n1), Some(n2) ->
          if n1 = n2 then Some n1 else None
      | None, Some(n2) -> Some n2
      | Some(n1), None -> Some n1
      | _ -> None

  let from_fixed (b,_) = b

  let bitwise op f1 f2 =
    match get_width f1, get_width f2 with
      | Some(n1), _ -> op n1 (from_fixed f1) (from_fixed f2)
      | _, Some(n2) -> op n2 (from_fixed f1) (from_fixed f2)
      | _ -> raise Parsing.Parse_error		   

%}

%token CAN SIMP SOLVE FOR ASSERT FIND CHECK LIFT USE EXT UNINTERP SIGMA VERBOSE RESET DROP NORM CMP CTXT
%token CNSTRNT HELP COMMANDS SYNTAX

%token INT REAL POS NEG NONNEG NONPOS

%token <string> IDENT
%token <int> INTCONST
%token <Ics.q> RATCONST
%token <string> BV_CONST
%token BV_OR
%token BV_XOR
%token BV_AND
%token BV_CONC
%token BV_EXTR
%token BV_COMPL
%token <int> WIDTH

%token LPAR RPAR  LBRA RBRA LCUR RCUR UNDERSCORE
%token COLON COMMA SEMI DOT DOTDOT ASSIGN

%token EQUAL DISEQ
%token TRUE FALSE AND OR XOR IMPLIES IFF NOT IF SETIF BVIF THEN ELSE END
%token PLUS MINUS TIMES DIVIDE FLOOR
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token CONV
%token IN NOTIN EMPTY FULL COMPL UNION INTER DIFF SYMDIFF SUB SETEQ
%token PROJ
%token INTEGER_PRED UNSIGNED
%token FORALL EXISTS
%right OR XOR IMPLIES
%left IFF AND
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%nonassoc SUB SETEQ
%left UNION
%left INTER
%left DIFF SYMDIFF
%nonassoc IN NOTIN
%left MINUS PLUS
%left TIMES DIVIDE
%left LBRA
%nonassoc prec_unary
%nonassoc CONV

%type <unit> command
%type <Ics.term> term
%type <Ics.term * Ics.term> equation

%start term
%start equation
%start command

%%

term:
  var                                 { $1 }
| const                               { Ics.mk_num $1 }
| var LPAR termlist RPAR              { Ics.mk_app $1 (List.rev $3) }
| array                               { $1 }
| prop                                { $1 }
| arith                               { $1 }
| tuple                               { $1 }
| set                                 { $1 }
| bv                                  { $1 }
| LPAR term RPAR                      { $2 }
;

var: IDENT { mk_var $1 }

const:
  INTCONST  { Ics.num_of_int $1 }
| RATCONST  { $1 }
;

array:
  term LBRA term ASSIGN term RBRA   { Ics.mk_update $1 $3 $5 }
| term LBRA term RBRA               { Ics.mk_app $1 [$3] }
     
arith: 
  term PLUS   term                  { Ics.mk_plus2 $1 $3 }
| term MINUS  term                  { Ics.mk_minus $1 $3 }
| term TIMES  term                  { Ics.mk_times [$1;$3] }
| term DIVIDE term                  { Ics.mk_div $1 $3 }
| MINUS term %prec prec_unary       { Ics.mk_unary_minus $2 }
;

tuple:
  LPAR termlist RPAR                  { mk_tuple(List.rev $2) }
| PROJ LBRA INTCONST COMMA INTCONST RBRA  LPAR term RPAR
                                      { mk_proj $3 $5 $8 }
;

set:
  EMPTY                               { Ics.mk_empty 0 }
| FULL                                { Ics.mk_full 0 }
| term UNION term                     { Ics.mk_union 0 $1 $3 }
| term DIFF term                      { Ics.mk_diff 0 $1 $3 }
| term SYMDIFF term                   { Ics.mk_sym_diff 0 $1 $3 }
| term INTER term                     { Ics.mk_inter 0 $1 $3 }
| term SUB term                       { Ics.mk_sub 0 $1 $3 }
| term SETEQ term                     { Ics.mk_seteq 0 $1 $3 }      
| COMPL term %prec prec_unary         { Ics.mk_compl 0 $2 }
| LCUR terms RCUR                     { Ics.mk_finite $2 }
| SETIF term THEN term ELSE term END  { Ics.mk_setite $2 $4 $6 }
| cnstrnt                             { Ics.mk_cnstrnt $1 }
;

terms:                { Ics.terms_empty() }
| term                { Ics.terms_add $1 (Ics.terms_empty()) }
| terms COMMA term    { Ics.terms_add $3 $1 }
;

cnstrnt:
  INT                               { Ics.cnstrnt_int }
| REAL                              { Ics.cnstrnt_real }
| interval                          { $1 }


interval:
  LPAR DOTDOT const RBRA          { Ics.cnstrnt_le $3 }
| LPAR DOTDOT const RPAR          { Ics.cnstrnt_lt $3 }
| LPAR const DOTDOT const RPAR    { Ics.cnstrnt_openopen $2 $4 }
| LPAR const DOTDOT const RBRA    { Ics.cnstrnt_openclosed $2 $4 }
| LBRA const DOTDOT const RBRA    { Ics.cnstrnt_closedclosed $2 $4 }
| LBRA const DOTDOT const RPAR    { Ics.cnstrnt_closedopen $2 $4 }
| LBRA const DOTDOT  RPAR         { Ics.cnstrnt_ge $2 }
| LPAR const DOTDOT  RPAR         { Ics.cnstrnt_gt $2 }
;      

prop: 
  TRUE                              { Ics.mk_true() }
| FALSE                             { Ics.mk_false() }
| atom                              { $1 }
| term AND term                     { Ics.mk_and $1 $3 }
| term OR term                      { Ics.mk_or $1 $3 }
| term XOR term                     { Ics.mk_xor $1 $3 }
| term IMPLIES term                 { Ics.mk_imp $1 $3 }
| term IFF term                     { Ics.mk_iff $1 $3 }
| NOT term %prec prec_unary         { Ics.mk_not $2 }
| IF term THEN term ELSE term END   { Ics.mk_ite $2 $4 $6 }
;

atom:
  term EQUAL term                   { Ics.mk_equal $1 $3 }
| term DISEQ term                   { Ics.mk_diseq $1 $3 }
| term LESS term                    { Ics.mk_lt $1 $3 }
| term GREATER term                 { Ics.mk_gt $1 $3 }
| term LESSOREQUAL term             { Ics.mk_le $1 $3 }
| term GREATEROREQUAL term          { Ics.mk_ge $1 $3 }
| term IN term                      { Ics.mk_in $1 $3 }
| term NOTIN term                   { Ics.mk_notin $1 $3 }
;

bv: 
  BV_CONST                                    { Ics.mk_bv_const $1 }
| BV_CONC LPAR fixed COMMA fixed RPAR         { match get_width $3,  get_width $5 with
                                                  | Some(n1), Some(n2) ->
                                                       Ics.mk_bv_conc (n1,from_fixed $3) (n2,from_fixed $5)
                                                  | _ -> raise Parsing.Parse_error }
| BV_AND LPAR fixed COMMA fixed RPAR          { bitwise Ics.mk_bv_and $3 $5 }
| BV_XOR LPAR fixed COMMA fixed RPAR          { bitwise Ics.mk_bv_xor $3 $5 }
| BV_OR LPAR fixed COMMA fixed RPAR           { bitwise Ics.mk_bv_or $3 $5 }
| BV_EXTR LBRA INTCONST COLON INTCONST RBRA LPAR fixed RPAR
                                              { match get_width $8 with
						  | Some(n) -> Ics.mk_bv_extr (n,from_fixed $8) $3 $5
						  | _ -> raise Parsing.Parse_error }
;

fixed:
  term widthopt  { ($1, $2) }

widthopt:  { None }
| WIDTH    { Some($1) }
;

termlist:
  term                { [$1] }
| termlist COMMA term { $3 :: $1 }
;

equation:
term EQUAL term    { ($1,$3) }
;

optterm:           { None }
| term             { Some($1) }
;


command:
  CAN term       DOT   { Cmd.can $2 }
| SIMP term      DOT   { Cmd.simp $2 }
| NORM term      DOT   { Cmd.norm $2 }
| term CMP term  DOT   { Cmd.less ($1,$3)}
| SOLVE equation DOT   { Cmd.solve None $2 }
| SOLVE equation FOR term DOT { Cmd.solve (Some $4) $2 }
| ASSERT term    DOT   { Cmd.process $2 }
| FIND optterm   DOT   { Cmd.find $2 }
| CHECK term     DOT   { Cmd.check $2 }
| EXT optterm DOT      { Cmd.ext $2 }
| USE optterm    DOT   { Cmd.use $2 }
| CNSTRNT optterm DOT  { Cmd.cnstrnt $2 } 
| CTXT DOT             { Cmd.ctxt () }
| UNINTERP optterm DOT { Cmd.uninterp $2 }
| SIGMA term     DOT   { Cmd.sigma $2 }
| VERBOSE INTCONST DOT { Cmd.verbose $2 }
| RESET          DOT   { Cmd.reset () }
| DROP           DOT   { Cmd.drop () }
| HELP           DOT   { Cmd.help () }
| HELP SYNTAX    DOT   { Cmd.help_syntax () }
| HELP COMMANDS  DOT   { Cmd.help_commands () }      
;

%%

