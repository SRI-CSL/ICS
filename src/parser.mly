
/*
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 */

/*s Module [Parser]: parser for ICS command interpreter. */

%{
  open Mpa
  open Ics

  let attributes = ref []

  let declare_attribute att id =
    if List.mem_assoc id !attributes then
      raise Parsing.Parse_error
    else 
      attributes := (id,att) :: !attributes

  let attribute_of id =
      List.assoc id !attributes

  let domain = ref [] 

  let declare_domain att id =
    if List.mem_assoc id !domain then
      raise Parsing.Parse_error
    else 
      domain := (id,att) :: !domain

  let domain_of id = 
    List.assoc id !domain

%}

%token CAN SIMP SOLVE NORM SOLUTION WITNESS FOR ASSERT FIND SHOW CHECK LIFT USE EXT UNINTERP SIGMA VERBOSE SHOW
%token CURRENT RESET DROP CMP CTXT CNSTRNT HELP COMMANDS SYNTAX GROEBNER UNDO DISEQS INCONSISTENT

%token ARITH BOOLEAN TUPLE EQ

%token <string> IDENT
%token <int> INTCONST
%token <Ics.q> RATCONST

%token INTDOM RATDOM BOOLDOM
%token A AC C
%token DECLARE

%token LPAR RPAR  LBRA RBRA LCUR RCUR UNDERSCORE AT
%token COLON COMMA SEMI DOT DOTDOT ASSIGN

%token EQUAL DISEQ
%token TRUE FALSE AND OR XOR IMPLIES IFF NOT IF THEN ELSE END
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token PROJ
%token INTEGER_PRED UNSIGNED
%right OR XOR IMPLIES
%left IFF AND
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%left MINUS PLUS
%left TIMES DIVIDE
%right EXPT
%nonassoc LBRA
%nonassoc LCUR
%nonassoc prec_unary

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
| IDENT LPAR termlist RPAR            { let l = List.rev $3 in
                                        let sym = Ics.mk_var $1 in
					try 
					  match attribute_of $1 with
					    | Term.AC -> Ics.mk_uninterp_ac sym l
					    | Term.A -> Ics.mk_uninterp_a sym l
				            | Term.C -> Ics.mk_uninterp_c sym l 
					with 
					    Not_found -> Ics.mk_uninterp sym l }
| array                               { $1 }
| prop                                { $1 }
| arith                               { $1 }
| tuple                               { $1 }
| LPAR term RPAR                      { $2 }
;

var: 
  IDENT     { try
		match domain_of $1 with
		  | Term.IntDom -> Ics.mk_intvar $1
		  | Term.RatDom -> Ics.mk_ratvar $1
		  | Term.BoolDom -> Ics.mk_boolvar $1 
	      with
		  Not_found -> 
		    Ics.mk_var $1 }

const:
  INTCONST  { Ics.num_of_int $1 }
| RATCONST  { $1 }
;

array:
  term LBRA term ASSIGN term RBRA   { Ics.mk_update $1 $3 $5 }
| term LBRA term RBRA               { Ics.mk_uninterp $1 [$3] }
     
arith: 
  term PLUS   term                  { Ics.mk_add2 $1 $3 }
| term MINUS  term                  { Ics.mk_sub $1 $3 }
| term TIMES  term                  { Ics.mk_mult [$1;$3] }
| term DIVIDE term                  { Ics.mk_div $1 $3 }
| term EXPT INTCONST                { Ics.mk_expt $1 $3 }
| MINUS term %prec prec_unary       { Ics.mk_unary_minus $2 }
;

tuple:
  LPAR termlist RPAR                  { mk_tuple(List.rev $2) }
| PROJ LBRA INTCONST COMMA INTCONST RBRA  LPAR term RPAR
                                      { mk_proj $3 $5 $8 }
;


terms:                { Ics.terms_empty() }
| term                { Ics.terms_add $1 (Ics.terms_empty()) }
| terms COMMA term    { Ics.terms_add $3 $1 }
;

prop: 
  TRUE                              { Ics.mk_true() }
| FALSE                             { Ics.mk_false() }
| atom                              { $1 }
| term AND term                     { Ics.mk_conjl [$1;$3] }
| term OR term                      { Ics.mk_disjl [$1;$3] }
| term XOR term                     { Ics.mk_xor $1 $3 }
| term IMPLIES term                 { Ics.mk_imp $1 $3 }
| term IFF term                     { Ics.mk_iff $1 $3 }
| NOT term %prec prec_unary         { Ics.mk_neg $2 }
| IF term THEN term ELSE term END   { Ics.mk_ite $2 $4 $6 }
;

atom:
  term EQUAL term                   { Ics.mk_equal $1 $3 }
| term DISEQ term                   { Ics.mk_diseq $1 $3 }
| term LESS term                    { Ics.mk_lt $1 $3 }
| term GREATER term                 { Ics.mk_gt $1 $3 }
| term LESSOREQUAL term             { Ics.mk_le $1 $3 }
| term GREATEROREQUAL term          { Ics.mk_ge $1 $3 }
| RATDOM LPAR term RPAR             { Ics.mk_real $3 }
| INTDOM LPAR term RPAR             { Ics.mk_int $3 }
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

theory:
  ARITH       { Term.ArithTh }
| BOOLEAN     { Term.BooleanTh }
| TUPLE       { Term.TupleTh }
| EQ          { Term.EqTh }

attribute: 
  A           { Term.A }
| AC          { Term.AC }
| C           { Term.C }
; 

domain: 
  INTDOM      { Term.IntDom }
| RATDOM      { Term.RatDom }
| BOOLDOM     { Term.BoolDom }    
;    

identlist:
  IDENT                 { [$1] }
| identlist COMMA IDENT { $3 :: $1 }
;

optint:       { None }
| INTCONST    { Some($1) }

command:
| SIGMA term     DOT   { Cmd.sigma $2 }
| CAN term       DOT   { Cmd.can $2 }
| NORM term      DOT   { Cmd.norm $2 }
| CNSTRNT term DOT     { Cmd.cnstrnt $2 }
| ASSERT term    DOT   { Cmd.process $2 }
| CTXT DOT             { Cmd.ctxt () }
| CURRENT DOT          { Cmd.curr() }
| SHOW DOT             { Cmd.show () }
| FIND theory optterm DOT { Cmd.find $2 $3}
| USE theory optterm DOT { Cmd.use $2 $3 }
| DISEQS optterm DOT { Cmd.diseqs $2 }
| CHECK term     DOT   { Cmd.check $2 }
| SOLVE theory equation DOT   { Cmd.solve $2 $3 }
| SOLUTION termlist  DOT { Cmd.solution $2 } 
| WITNESS termlist  DOT { Cmd.witness $2 } 
| EXT term DOT         { Cmd.ext $2 }
| term CMP term  DOT   { Cmd.less ($1,$3)}
| VERBOSE INTCONST DOT { Cmd.verbose $2 }
| GROEBNER       DOT   { Cmd.groebner () }
| INCONSISTENT DOT     { Cmd.inconsistent () }
| UNDO optint DOT      { Cmd.undo $2 }
| RESET          DOT   { Cmd.reset () }
| DROP           DOT   { Cmd.drop () }
| HELP           DOT   { Cmd.help () }
| HELP SYNTAX    DOT   { Cmd.help_syntax () }
| HELP COMMANDS  DOT   { Cmd.help_commands () } 
| DECLARE attribute identlist DOT 
                       { List.iter (declare_attribute $2) $3 }
| DECLARE domain identlist DOT 
                       { List.iter (declare_domain $2) $3 }
;

%%

