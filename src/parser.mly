
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

%token DROP CAN ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG VALID UNSAT
%token TYPE SIGMA
%token SOLVE HELP DEF TOGGLE SET TRACE UNTRACE CMP FIND USE INV SOLUTION PARTITION
%token SHOW CNSTRNT SYNTAX COMMANDS SPLIT
%token DISEQ CTXT
%token EOF

%token ARITH TUPLE

%token <string> IDENT
%token <int> INTCONST
%token <Mpa.Q.t> RATCONST

%token IN
%token BOT INT NONINT REAL BV TOP 
%token INF NEGINF
%token ALBRA ACLBRA CLBRA

%token LPAR RPAR LBRA RBRA LCUR RCUR UNDERSCORE KLAMMERAFFE
%token COLON COMMA DOT DDOT ASSIGN UNION TO ENDMARKER BACKSLASH

%token <string> BVCONST 
%token <string * int> FRESH
%token <int> FREE

%token CONC SUB BWITE BWAND BWOR BWXOR BWIMP BWIFF BWNOT
%token BVCONC 
%token EQUAL DISEQ
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL  
%token UNSIGNED APPLY LAMBDA
%token WITH CONS CAR CDR NIL
%token INL INR OUTL OUTR
%token INJ OUT 
%token HEAD TAIL LISTCONS
%token DISJ XOR IMPL BIIMPL CONJ NEG
%token PROJ

%right DISJ XOR IMPL
%left BIIMPL CONJ
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%left APPLY
%left UNION
%left MINUS PLUS 
%left DIVIDE
%left TIMES
%right EXPT
%left LISTCONS
%right BVCONC
%right BWOR BWXOR BWIMP
%left BWAND BWIFF
%nonassoc TO
%nonassoc IN NOTIN
%nonassoc LCUR
%nonassoc LBRA
%nonassoc prec_unary

%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Cnstrnt.t> cnstrnteof
%type <Result.t> commands
%type <Result.t> commandseof
%type <Result.t> commandsequence


%start termeof
%start atomeof
%start cnstrnteof
%start commands
%start commandsequence
%start commandseof

%%

termeof : term EOF           { $1 }
atomeof : atom EOF           { $1 }
cnstrnteof : cnstrnt EOF     { $1 }
commandseof : command EOF    { $1 }

commands : command DOT       { $1 }
| EOF                        { raise End_of_file }

commandsequence : command DOT    { $1 }
| command DOT commandsequence    { $3 }
| EOF                            { raise End_of_file }

    
int: 
  INTCONST  { $1 }
| MINUS INTCONST %prec prec_unary { -$2 }

rat:
  int       { Q.of_int $1 }
| RATCONST  { $1 }
| MINUS RATCONST %prec prec_unary { Q.minus $2 }
;


name: IDENT            { Name.of_string $1 }

funsym: 
  name                                   { Sym.Uninterp($1) }
| PLUS                                   { Sym.Arith(Sym.Add) }
| TIMES                                  { Sym.Pp(Sym.Mult) }
| EXPT LBRA int RBRA                     { Sym.Pp(Sym.Expt($3)) }
| TUPLE                                  { Sym.Product(Sym.Tuple) }
| UNSIGNED                               { Sym.Bvarith(Sym.Unsigned) }
| PROJ LBRA INTCONST COMMA INTCONST RBRA { Sym.Product(Sym.Proj($3, $5)) }
| CONS                                   { Sym.Product(Sym.Tuple)  }
| CAR                                    { Sym.Product(Sym.Proj(0, 2)) }
| CDR                                    { Sym.Product(Sym.Proj(1, 2)) }
| CONC LBRA INTCONST COMMA INTCONST RBRA               { Sym.Bv(Sym.Conc($3, $5)) }
| SUB LBRA INTCONST COMMA INTCONST COMMA INTCONST RBRA { Sym.Bv(Sym.Sub($3, $5, $7)) }
| BWITE LBRA INTCONST RBRA                             { Sym.Bv(Sym.Bitwise($3)) }
| APPLY range                            { Sym.Fun(Sym.Apply($2)) }
| LAMBDA                                 { Sym.Fun(Sym.Abs) }
;

range:                              { None }
| LBRA cnstrnt RBRA                 { Some($2) }
;

constsym: 
  rat       { Sym.Arith(Sym.Num($1)) }
| TRUE      { Sym.Bv(Sym.Const(Bitv.from_string "1")) }
| FALSE     { Sym.Bv(Sym.Const(Bitv.from_string "0")) }  
| BVCONST   { Sym.Bv(Sym.Const(Bitv.from_string $1)) }
;



term:
  var              { $1 }
| app              { $1 }
| LPAR term RPAR   { $2 }
| arith            { $1 }     /* infix/mixfix syntax */
| array            { $1 }
| bv               { $1 }
| coproduct        { $1 }
| boolean          { $1 }
| list             { $1 }
| apply            { $1 }
;

var:
  name  { try
	    match Symtab.lookup $1 (Istate.symtab()) with
	      | Symtab.Def(a) -> a
	      | _ -> Term.mk_var $1
	  with
	      Not_found -> Term.mk_var $1 }
| FRESH  { let (x,k) = $1 in 
	   Term.mk_fresh_var (Name.of_string x) (Some(k)) }
| FREE   { Term.Var(Var.mk_free $1) }
;


app: 
  funsym LPAR termlist RPAR     { Istate.sigma $1 (List.rev $3) }
| constsym                      { Istate.sigma $1 [] }

list: 
  term LISTCONS term            { Coproduct.mk_inj 1 (Tuple.mk_tuple [$1; $3]) }
| HEAD LPAR term RPAR           { Tuple.mk_proj 0 2 (Coproduct.mk_out 1 $3) }
| TAIL LPAR term RPAR           { Tuple.mk_proj 1 2 (Coproduct.mk_out 1 $3) }
| NIL                           { Coproduct.mk_inj 0 (Tuple.mk_tuple []) }

apply: 
  term APPLY term               { Apply.mk_apply
				    (Context.sigma (Context.empty))
                                     None $1 [$3] }

     
arith:
| term PLUS term                { Arith.mk_add $1 $3 }
| term MINUS term               { Arith.mk_sub $1 $3 }
| MINUS term %prec prec_unary   { Arith.mk_neg $2 }
| term TIMES term               { Sig.mk_mult $1 $3 }
| term DIVIDE term              { Sig.mk_div $1 $3 }
| term EXPT int                 { Sig.mk_expt $3 $1 }
;

coproduct:
  INL LPAR term RPAR                    { Coproduct.mk_inl $3 }
| INR LPAR term RPAR                    { Coproduct.mk_inr $3 }
| OUTL LPAR term RPAR                   { Coproduct.mk_outl $3 }
| OUTR LPAR term RPAR                   { Coproduct.mk_outr $3 }
| INJ LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_inj $3 $6 }
| OUT LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_out $3 $6 }


array:
  term LBRA term ASSIGN term RBRA { Arr.mk_update $1 $3 $5 }
| term LBRA term RBRA        { Arr.mk_select $1 $3 }
;


bv:
  term BVCONC term   { match Istate.width_of $1, Istate.width_of $3 with
			  | Some(n), Some(m) -> 
			      if n < 0 then
				raise (Invalid_argument ("Negative length of " ^ Term.to_string $1))
			      else if m < 0 then
				raise (Invalid_argument ("Negative length of " ^ Term.to_string $3))
			      else 
				Bitvector.mk_conc n m $1 $3
			  | Some _, _ -> 
			      raise (Invalid_argument (Term.to_string $3 ^ " not a bitvector."))
			  | _ -> 
			      raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
| term LBRA INTCONST COLON INTCONST RBRA 
                      { match Istate.width_of $1 with
			  | Some(n) -> 
			      if n < 0 then
				raise(Invalid_argument ("Negative length of " ^ Term.to_string $1))
			      else if not(0 <= $3 && $3 <= $5 && $5 < n) then
				raise(Invalid_argument ("Invalid extraction from " ^ Term.to_string $1))
			      else 
				Bitvector.mk_sub n $3 $5 $1
			  | None ->  
			      raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
| term BWAND term     { Bitvector.mk_bwconj (equal_width_of $1 $3) $1 $3 }
| term BWOR term      { Bitvector.mk_bwdisj (equal_width_of $1 $3) $1 $3 }
| term BWIMP term     { Bitvector.mk_bwimp (equal_width_of $1 $3) $1 $3 }
| term BWIFF term     { Bitvector.mk_bwiff (equal_width_of $1 $3) $1 $3 }
;

boolean:
| term CONJ term            { Boolean.mk_conj $1 $3 }
| term DISJ term            { Boolean.mk_disj $1 $3 }
| term XOR term             { Boolean.mk_xor $1 $3 }
| NEG term %prec prec_unary { Boolean.mk_neg $2 }
;


atom:
  term EQUAL term         { Atom.mk_equal(Fact.mk_equal $1 $3 Fact.mk_axiom)}
| term DISEQ term         { Atom.mk_diseq(Fact.mk_diseq $1 $3 Fact.mk_axiom) }
| term LESS term          { Atom.mk_in(Fact.mk_cnstrnt (Arith.mk_sub $1 $3) (Cnstrnt.mk_neg Dom.Real) Fact.mk_axiom) }
| term GREATER term       { Atom.mk_in(Fact.mk_cnstrnt (Arith.mk_sub $3 $1) (Cnstrnt.mk_neg Dom.Real) Fact.mk_axiom) }
| term LESSOREQUAL term   { Atom.mk_in(Fact.mk_cnstrnt (Arith.mk_sub $1 $3) (Cnstrnt.mk_nonpos Dom.Real) Fact.mk_axiom) }
| term GREATEROREQUAL term{ Atom.mk_in(Fact.mk_cnstrnt (Arith.mk_sub $3 $1) (Cnstrnt.mk_nonpos Dom.Real) Fact.mk_axiom)}
| term IN cnstrnt         { Atom.mk_in (Fact.mk_cnstrnt $1 $3 Fact.mk_axiom) }
;


cnstrnt:
| interval optdiseqs { Cnstrnt.make ($1, $2) }
| name               { match Istate.type_of $1 with
			 | Some(c) -> c
			 | None ->
			    let str = Name.to_string $1 in
			    raise (Invalid_argument ("No type definition for " ^ str)) }
;

optdiseqs:                   { Cnstrnt.Diseqs.empty }
| BACKSLASH LCUR diseqs RCUR { $3 }
;

diseqs: rat          { Cnstrnt.Diseqs.singleton $1 }     
| diseqs COMMA rat   { Cnstrnt.Diseqs.add $3 $1 }



interval: 
  INT                                    { Interval.mk_int }
| REAL                                   { Interval.mk_real }
| NONINT                                 { Interval.mk_nonint }
| NONINT leftendpoint DDOT rightendpoint { Interval.make (Dom.Nonint, $2, $4) }
| INT leftendpoint DDOT rightendpoint    { Interval.make (Dom.Int, $2, $4) }
| REAL leftendpoint DDOT rightendpoint   { Interval.make (Dom.Real, $2, $4) }
| leftendpoint DDOT rightendpoint        { Interval.make (Dom.Real, $1, $3) }
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
  CAN atom                  { Result.Atom(Istate.can $2) }
| CAN term                  { Result.Term(Istate.cant $2) }
| ASSERT optname atom       { Result.Process(Istate.process $2 $3) }
| DEF name ASSIGN term      { Result.Unit(Istate.def $2 $4) }
| SIG name COLON signature  { Result.Unit(Istate.sgn $2 $4) }
| TYPE name ASSIGN cnstrnt  { Result.Unit(Istate.typ $2 $4) }
| RESET                     { Result.Unit(Istate.reset ()) }
| SAVE name                 { Result.Name(Istate.save(Some($2))) }
| SAVE                      { Result.Name(Istate.save(None)) }        
| RESTORE name              { Result.Unit(Istate.restore $2) }
| REMOVE name               { Result.Unit(Istate.remove $2) }
| FORGET                    { Result.Unit(Istate.forget()) }
| VALID optname atom        { Result.Bool(Istate.valid $2 $3) }
| UNSAT optname atom        { Result.Bool(Istate.unsat $2 $3) }
| EXIT                      { raise End_of_file }
| DROP                      { failwith "drop" }
| SYMTAB                    { Result.Symtab(Istate.symtab()) }
| SYMTAB name               { match Istate.entry_of $2 with
				| Some(e) -> Result.Entry(e)
				| None -> raise (Invalid_argument (Name.to_string $2 ^ "not in symbol table")) }
| CTXT optname              { Result.Atoms(Istate.ctxt_of $2) }
| SIGMA term                { Result.Term($2) }
| term CMP term             { Result.Int(Term.cmp $1 $3) }
| SHOW optname              { Result.Context(Istate.get_context $2) }
| FIND optname th term      { Result.Term(Istate.find $2 $3 $4) }
| INV optname th term       { try Result.Optterm(Some(Istate.inv $2 $3 $4))
		 	      with Not_found -> Result.Optterm(None) }
| USE optname th term       { Result.Terms(Istate.use $2 $3 $4) }
| SOLUTION optname th       { Result.Solution(Istate.solution $2 $3) }
| CNSTRNT optname term      { match Istate.cnstrnt $2 $3 with
				| Some(c) -> Result.Cnstrnt(Some(c))
				| None -> Result.Cnstrnt(None) }
| DISEQ optname term        { Result.Terms(Istate.diseq $2 $3) }
| SPLIT optname             { Result.Atoms(Istate.split()) }
| SOLVE th term EQUAL term  { Result.Solution(Istate.solve $2 ($3, $5)) }		
| TRACE identlist           { Result.Unit(List.iter Trace.add $2) }
| UNTRACE                   { Result.Unit(Trace.reset ()) }
| help                      { Result.Unit($1) }
;

identlist :
  IDENT                     { [$1] }
| identlist COMMA IDENT     { $3 :: $1 }

		
th: IDENT  { Th.of_string $1 } /* may raise [Invalid_argument]. */

help:
  HELP                      { Help.on_help () }
| HELP SYNTAX               { Help.syntax () }
| HELP COMMANDS             { Help.commands () }
;


optname:                    { None }
| KLAMMERAFFE name          { Some($2) }
;


%%
