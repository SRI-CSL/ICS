
/*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *
 * Author: Harald Ruess
 */

/*s Module [Parser]: parser for ICS syntactic categories. */

%{
  open Mpa
  open Tools

let out = Istate.outchannel

let pr str =  Format.fprintf (out()) str

let nl () = pr "\n"

let equal_width_of a b =
 match Istate.width_of a, Istate.width_of a with
   | Some(n), Some(m) when n = m -> n
   | Some(n), None -> n
   | None, Some(n) -> n
   | Some _, Some _ ->
       raise (Invalid_argument "Argument mismatch")
   | None, None -> 
       raise (Invalid_argument (Term.to_string a ^ " not a bitvector."))


%}

%token DROP CAN ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG
%token TYPE SIGMA
%token SOLVE HELP DEF TOGGLE SET VERBOSE CMP FIND USE INV SOLUTION PARTITION
%token SHOW CNSTRNT SYNTAX COMMANDS
%token DISEQ CTXT GC
%token EOF

%token ARITH TUPLE

%token <string> IDENT
%token <int> INTCONST
%token <Mpa.Q.t> RATCONST

%token IN
%token BOT INT NONINT REAL BV TOP 
%token INF NEGINF
%token ALBRA ACLBRA CLBRA

%token LPAR RPAR LBRA RBRA LCUR RCUR UNDERSCORE AT
%token COLON COMMA DOT DDOT ASSIGN UNION TO ENDMARKER

%token <string> BVCONST 
%token <string * int> FRESH

%token CONC SUB BWITE BWAND BWOR BWXOR BWNOT
%token BVCONCI BWANDI BWORI BWXORI
%token EQUAL DISEQ
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token WITH CONS CAR CDR NIL
%token PROJ
%token UNSIGNED

%right DISJ XOR IMPL
%left BIIMPL CONJ
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%left UNION
%left MINUS PLUS
%left TIMES DIVIDE
%right EXPT
%right BVCONCI
%right BWORI BWXORI
%left BWANDI
%nonassoc TO
%nonassoc IN NOTIN
%nonassoc LCUR
%nonassoc LBRA
%nonassoc prec_unary

%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Cnstrnt.t> cnstrnteof
%type <unit> commands

%start termeof
%start atomeof
%start cnstrnteof
%start commands

%%

termeof : term EOF           { $1 }
atomeof : atom EOF           { $1 }
cnstrnteof : cnstrnt EOF     { $1 }

commands : command DOT       { $1 }
| EOF                        { raise End_of_file }

rat:
  INTCONST  { Q.of_int $1 }
| RATCONST  { $1 }
;


name: IDENT            { Name.of_string $1 }

term:
  var              { $1 }
| freshvar         { $1 }
| app              { $1 }
| arith            { $1 }
| tuple            { $1 }
| array            { $1 }
| sexpr            { $1 }
| bv               { $1 }
| boolean          { $1 }
| LPAR term RPAR   { $2 }
;

var: name  { try
		 match Symtab.lookup $1 (Istate.symtab()) with
		   | Symtab.Def(a) -> a
		   | _ -> Term.mk_var $1
	       with
		   Not_found -> Term.mk_var $1 }
;

freshvar : FRESH { let (x,k) = $1 in Term.mk_fresh (Name.of_string x) (Some(k)) }


app: name LPAR termlist RPAR       { Istate.sigma (Sym.mk_uninterp $1) $3 }
     
arith: 
  rat                              { Arith.mk_num $1 }
| term PLUS term                   { Arith.mk_add $1 $3 }
| term MINUS term                  { Arith.mk_sub $1 $3 }
| term TIMES term                  { Arith.mk_mult $1 $3 }
| MINUS term %prec prec_unary      { Arith.mk_neg $2 }
| term EXPT INTCONST               { Arith.mk_expt $3 $1 }
;

tuple:
  LPAR termlist RPAR               { Tuple.mk_tuple(List.rev $2) }
| PROJ LBRA INTCONST COMMA INTCONST RBRA  LPAR term RPAR
                                   { Tuple.mk_proj $3 $5 $8 }
;

sexpr:
  NIL                            { Tuple.mk_tuple [] }
| CONS LPAR term COMMA term RPAR { Tuple.mk_tuple [$3; $5] }
| CAR LPAR term RPAR             { Tuple.mk_proj 0 2 $3 }
| CDR LPAR term RPAR             { Tuple.mk_proj 1 2 $3 }
;


array: 
  term LBRA term ASSIGN term RBRA { Builtin.mk_update (Istate.tests()) $1 $3 $5 }
| term LBRA term RBRA             { Builtin.mk_select (Istate.tests()) $1 $3 }  
;

boolean:
  TRUE      { Boolean.mk_true }
| FALSE     { Boolean.mk_false }
;

bv:
  BVCONST             { Bitvector.mk_const (Bitv.from_string $1) }
| CONC LBRA INTCONST COMMA INTCONST RBRA LPAR term COMMA term RPAR 
                      { Bitvector.mk_conc $3 $5 $8 $10 }
| SUB LBRA INTCONST COMMA INTCONST COMMA INTCONST RBRA LPAR term RPAR
                      { Bitvector.mk_sub $3 $5 $7 $10 }
| BWITE LBRA INTCONST RBRA LPAR term COMMA term COMMA term RPAR
                      { Bitvector.mk_bitwise $3 $6 $8 $10 }
| BWAND LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bitvector.mk_bitwise $3 $6 $8 (Bitvector.mk_zero $3) }
| BWOR LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bitvector.mk_bitwise $3 $6 (Bitvector.mk_one $3) $8 }
| BWXOR LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bitvector.mk_bitwise $3 $6 
			  (Bitvector.mk_bitwise $3 $8 (Bitvector.mk_zero $3) (Bitvector.mk_one $3)) $8 }
| BWNOT LBRA INTCONST RBRA LPAR term RPAR 
                      { Bitvector.mk_bitwise $3 $6 (Bitvector.mk_zero $3) (Bitvector.mk_one $3) }
| term BVCONCI term   { match Istate.width_of $1, Istate.width_of $3 with
			  | Some(n), Some(m) -> Bitvector.mk_conc n m $1 $3
			  | Some _, _ -> raise (Invalid_argument (Term.to_string $3 ^ " not a bitvector."))
			  | _ -> raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
| term LBRA INTCONST COLON INTCONST RBRA 
                      { match Istate.width_of $1 with
			  | Some(n) -> Bitvector.mk_sub n $3 $5 $1
			  | None ->  raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
| term BWANDI term    { let n = equal_width_of $1 $3 in
		        Bitvector.mk_bitwise n $1 $3 (Bitvector.mk_zero n) }
| term BWORI term     { let n = equal_width_of $1 $3 in
		        Bitvector.mk_bitwise n $1 (Bitvector.mk_one n) $3 }
| term BWXORI term    { let n = equal_width_of $1 $3 in
		        Bitvector.mk_bitwise n $1 
			  (Bitvector.mk_bitwise n $3 (Bitvector.mk_zero n) (Bitvector.mk_one n)) $1}
;


atom:
  term EQUAL term                   { Atom.mk_equal $1 $3 }
| term DISEQ term                   { Atom.mk_diseq $1 $3 }
| term LESS term                    { Atom.mk_lt $1 $3 }
| term GREATER term                 { Atom.mk_lt $3 $1 }
| term LESSOREQUAL term             { Atom.mk_le $1 $3 }
| term GREATEROREQUAL term          { Atom.mk_le $3 $1 }
| term IN cnstrnt                   { Atom.mk_in $3 $1 }
;


cnstrnt:
| interval          { Cnstrnt.of_interval $1 }
| name              { match Istate.type_of $1 with
			| Some(c) -> c
			| None ->
			    let str = Name.to_string $1 in
			    raise (Invalid_argument ("No type definition for " ^ str)) }
;

interval: 
  INT                                  { Interval.mk_int }
| REAL                                 { Interval.mk_real }
| INT leftendpoint DDOT rightendpoint  { Interval.make (Dom.Int, $2, $4) }
| REAL leftendpoint DDOT rightendpoint { Interval.make (Dom.Real, $2, $4) }
| leftendpoint DDOT rightendpoint      { Interval.make (Dom.Real, $1, $3) }
;

leftendpoint:
  LPAR NEGINF     { Endpoint.neginf }
| LPAR rat        { Endpoint.strict $2 }
| LBRA rat        { Endpoint.nonstrict $2 }
;

rightendpoint:
  INF RPAR          { Endpoint.posinf }
| rat RPAR          { Endpoint.strict $1 }
| rat RBRA          { Endpoint.nonstrict $1 }
;

termlist:             { [] }
| term                { [$1] }
| termlist COMMA term { $3 :: $1 }
;

signature:
  BV LBRA INTCONST RBRA     { $3 }
;

command: 
  CAN atom                  { Atom.pp (out()) (Istate.can $2) }
| CAN term                  { Term.pp (out()) (Istate.cant $2) }
| ASSERT atom               { match Istate.process $2 with
				| Shostak.Valid -> pr "Valid."
				| Shostak.Inconsistent -> pr "Unsat."
				| Shostak.Satisfiable _ -> () }
| DEF name ASSIGN term      { Istate.def $2 $4 }
| SIG name COLON signature  { Istate.sgn $2 $4 }
| TYPE name ASSIGN cnstrnt  { Istate.typ $2 $4 }
| RESET                     { Istate.reset (); }
| SAVE name                 { Istate.save $2 }         
| RESTORE name              { Istate.restore $2 }
| REMOVE name               { Istate.remove $2 }
| FORGET                    { Istate.forget () }
| EXIT                      { raise End_of_file }
| DROP                      { failwith "drop" }
| SYMTAB                    { Symtab.pp (out()) (Istate.symtab()) }
| SYMTAB name               { match Istate.entry_of $2 with
				| Some(e) -> Symtab.pp_entry (out()) e
				| None -> raise (Invalid_argument (Name.to_string $2 ^ "not in symbol table")) }
| CTXT                      { Pretty.list Atom.pp (out()) (Istate.ctxt_of ()) }
| SIGMA term                { pr "val: "; Term.pp (out()) $2 }
| GC                        { Istate.compress () }
| term CMP term             { let cmp = Term.cmp $1 $3 in
			      let str = if cmp < 0 then "Less." else if cmp = 0 then "Equal." else "Greater." in
			      Pretty.string (out()) str }
| SHOW                      { Shostak.pp (out()) (Istate.current()) }
| FIND th term              { Term.pp (out()) (Istate.find $2 $3) }
| INV th term               { try Term.pp (out()) (Istate.inv $2 $3)
		 	      with Not_found -> pr "Undef." }
| USE th term               { Pretty.set Term.pp  (out()) (Term.Set.elements (Istate.use $2 $3)) }
| SOLUTION th               { Pretty.solution Term.pp (out()) (Istate.solution $2) }
| PARTITION                 { Pretty.solution Term.pp (out()) (Istate.partition ()) }
| CNSTRNT term              { match Istate.cnstrnt $2 with
				| Some(c) -> Cnstrnt.pp (out()) c
				| None -> Pretty.string (out()) "None." }
| DISEQ term                { Pretty.list Term.pp (out()) (Istate.diseq $2) }
| SOLVE ith term EQUAL term
                            { try
				let el = Th.solve $2 ($3, $5) in
				Pretty.list (Pretty.eqn Term.pp) (out()) el
                              with
				| Exc.Unsolved -> Pretty.string (out()) "Unsolved."
			        | Exc.Inconsistent -> Pretty.string (out()) "Unsat." }
| VERBOSE INTCONST          { Trace.set_verbose $2 }
| help                      { $1 }
;

ith: IDENT                  { Interp.of_name $1 }   /* may raise [Invalid_argument]. */
		
th: IDENT                   { Theories.of_name $1 } /* may raise [Invalid_argument]. */

help:
  HELP                      { Help.on_help () }
| HELP SYNTAX               { Help.syntax () }
| HELP COMMANDS             { Help.commands () }
;

%%
