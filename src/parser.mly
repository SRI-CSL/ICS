
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

  let mk_tuple = function
    | [] -> assert false
    | [t] -> t
    | l -> Ics.mk_tuple l

  let get_width (a,w) =
    match Ics.width_of a,w with
      | Some(n1), Some(n2) ->
          if n1 = n2 then Some n1 else None
      | None, Some(n2) -> Some n2
      | Some(n1), None -> Some n1
      | _ -> None

  let from_fixed (b,_) = b

  let bitwise_binary op f1 f2 =
    match get_width f1, get_width f2 with
      | Some(n1), _ ->
	  if n1 > 0 then
	    op n1 (from_fixed f1) (from_fixed f2)
	  else
	    raise Parsing.Parse_error
      | _, Some(n2) ->
	  if n2 > 0 then
	    op n2 (from_fixed f1) (from_fixed f2)
	  else
	    raise Parsing.Parse_error
      | _ ->
	  raise Parsing.Parse_error


  let bitwise_unary op (a,width) =
    match width, Ics.width_of a with
      | Some(n), Some(m) ->
	  if n = m && n > 0 then
	    op n a
	  else 
	    raise Parsing.Parse_error		
      | Some(n), None ->
	  op n a
      | None, Some(m) ->
	  if m > 0 then
	    op m a
	  else
	    raise Parsing.Parse_error
      | None, None ->
	  raise Parsing.Parse_error

  let check_width n a =
    if n <= 0 then
      raise Parsing.Parse_error;
    match Ics.width_of a with
      | Some(m) when n <> m ->
	  raise Parsing.Parse_error
      | _ ->
	  ()

%}

%token CAN SIMP SOLVE SOLUTION FOR ASSERT FIND CHECK LIFT USE EXT UNINTERP SIGMA VERBOSE
%token CURRENT RESET DROP NORM CMP CTXT
%token CNSTRNT HELP COMMANDS SYNTAX

%token INT REAL NONINTREAL POS NEG NONNEG NONPOS NEGINF POSINF

%token <string> IDENT
%token <int> INTCONST
%token <Ics.q> RATCONST
%token <string> BV_CONST
%token <int option> BV_OR
%token <int option> BV_XOR
%token <int option> BV_AND
%token <int option> BV_COMPL
%token <(int * int) option> BV_CONC
%token <int option> BV_EXTR

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
%right BV_CONC
%right BV_AND
%right BV_OR
%right BV_XOR
%nonassoc LBRA
%nonassoc LCUR
%nonassoc BV_EXTR
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
| domain                                 { Ics.cnstrnt_domain $1 }
| domain LPAR NEGINF DOTDOT const RBRA   { Ics.cnstrnt_le $1 $5 }
| domain LPAR NEGINF DOTDOT const RPAR   { Ics.cnstrnt_lt $1 $5 }
| domain LPAR const DOTDOT const RPAR    { Ics.cnstrnt_openopen $1 $3 $5 }
| domain LPAR const DOTDOT const RBRA    { Ics.cnstrnt_openclosed $1 $3 $5 }
| domain LBRA const DOTDOT const RBRA    { Ics.cnstrnt_closedclosed $1 $3 $5 }
| domain LBRA const DOTDOT const RPAR    { Ics.cnstrnt_closedopen $1 $3 $5 }
| domain LBRA const DOTDOT POSINF RPAR   { Ics.cnstrnt_ge $1 $3 }
| domain LPAR const DOTDOT POSINF RPAR   { Ics.cnstrnt_gt $1 $3 }
;

domain:       
  REAL            { Ics.interval_domain_real() }
| INT             { Ics.interval_domain_int() }
| NONINTREAL      { Ics.interval_domain_nonintreal() }
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
  BV_CONST                          { Ics.mk_bv_const $1 }
| term BV_CONC term                 { match $2 with
                                        | Some(n,m) ->
					    check_width n $1;
					    check_width m $3;
					    if n > 0 && m > 0 then
                                              Ics.mk_bv_conc (n,$1) (m,$3)
					    else
					      raise Parsing.Parse_error
					| None ->
					    (match Ics.width_of $1, Ics.width_of $3 with
					       | Some(n), Some(m) ->
						   if n > 0 && m > 0 then
						     Ics.mk_bv_conc (n,$1) (m,$3)
						   else
						     raise Parsing.Parse_error
					       | _ ->
                                                   raise Parsing.Parse_error) }
| term BV_AND term                  { bitwise_binary Ics.mk_bv_and ($1,$2) ($3,$2) }
| term BV_OR term                   { bitwise_binary Ics.mk_bv_or ($1,$2) ($3,$2) }
| term BV_XOR term                  { bitwise_binary Ics.mk_bv_xor ($1,$2) ($3,$2) }
| BV_COMPL term %prec prec_unary   { bitwise_unary Ics.mk_bv_neg ($2,$1) }
| term BV_EXTR extract              { let (i,j) = $3 in
				      match $2 with
					| Some(n) ->
					    check_width n $1;
					    if 0 <= i && i <= j && j < n then
					      Ics.mk_bv_extr (n,$1) i j
				            else
					      raise Parsing.Parse_error
					| None ->
					    (match Ics.width_of $1 with
					       | Some(m) ->
						   if 0 <= i && i <= j && j < m then
						     Ics.mk_bv_extr (m,$1) i j
						   else
						     raise Parsing.Parse_error
					       | None ->
						   raise Parsing.Parse_error) }
;

extract:
  LBRA INTCONST RBRA                { ($2,$2) }
| LBRA INTCONST COLON INTCONST RBRA { ($2,$4) }


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
| NORM term      DOT   { Cmd.norm $2 }
| term CMP term  DOT   { Cmd.less ($1,$3)}
| SOLVE equation DOT   { Cmd.solve None $2 }
| SOLVE equation FOR term DOT { Cmd.solve (Some $4) $2 }
| SOLUTION term  DOT   { Cmd.solution $2 }
| ASSERT term    DOT   { Cmd.process $2 }
| CURRENT DOT          { Cmd.curr() }
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

