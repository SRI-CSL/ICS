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

(** Command parser. *)

%}

%token DROP CAN SIMPLIFY ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG VALID UNSAT
%token TYPE SIGMA
%token SOLVE HELP DEFTERM DEFPROP DEFTHEORY TOGGLE SET GET CMP FIND USE INV SOLUTION PARTITION
%token SHOW SIGN SYNTAX COMMANDS RESOLVE SAT ECHO UNDO LOAD
%token CONFIG STATUS EVAL REGISTER
%token DISEQ CTXT 
%token IN NOTIN TT FF
%token EOF QUOTE

%token <string> IDENT
%token <string> STRING
%token <Mpa.Z.t> INTCONST
%token <Mpa.Q.t> RATCONST
%token <Name.t> PROPVAR

%token IN
%token BOT INT NONINT REAL BV TOP
%token INF NEGINF
%token ALBRA ACLBRA CLBRA

%token LPAR RPAR LBRA RBRA LCUR RCUR PROPLPAR PROPRPAR UNDERSCORE KLAMMERAFFE
%token COLON COMMA DOT DDOT ASSIGN TO ENDMARKER BACKSLASH
%token EMPTY FULL UNION INTER COMPL DIFF

%token <string> BVCONST 
%token <string * int> FRESH

%token CONC SUB BWITE BWAND BWOR BWXOR BWIMP BWIFF BWNOT
%token BVCONC 
%token EQUAL DISEQ SUBSET EQUAL2 MOD
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL  
%token UNSIGNED APPLY LAMBDA S K I C
%token WITH CONS CAR CDR NIL
%token INL INR OUTL OUTR
%token INJ OUT 
%token HEAD TAIL LISTCONS
%token PROPVAR DISJ XOR IMPL BIIMPL CONJ NEG
%token IF THEN ELSE END
%token PROJ
%token CREATE
%token SUP INF
%token THEORY SIGNATURE AXIOMS BEGIN DESCRIPTION
%token REDUCE REWRITE

%nonassoc REDUCE REWRITE
%right DISJ XOR IMPL
%left BIIMPL CONJ
%nonassoc EQUAL EQUAL2 DISEQ SUBSET LESS GREATER LESSOREQUAL GREATEROREQUAL MOD
%left APPLY
%right UNION
%right INTER, DIFF
%nonassoc COMPL
%left MINUS PLUS 
%left DIVIDE
%left TIMES
%right EXPT
%right LISTCONS
%right BVCONC
%right BWOR BWXOR BWIMP
%left BWAND BWIFF
%nonassoc TO
%nonassoc IN NOTIN
%nonassoc LCUR
%nonassoc LBRA
%nonassoc COLON
%nonassoc prec_unary

%type <Funsym.t> funsymeof
%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Prop.t> propeof
%type <unit> commands
%type <unit> commandseof
%type <unit> commandsequence

%start funsymeof
%start termeof
%start atomeof
%start propeof
%start commands
%start commandseof
%start commandsequence


%%

funsymeof : funsym EOF       { $1 }
termeof : term EOF           { $1 }
atomeof : atom EOF           { $1 }
propeof : prop EOF           { $1 }
commandseof : command EOF    { () }

commands : 
  command DOT     { () }
| EOF             { raise End_of_file }
;

commandsequence :
  command DOT commandsequence    { () }
| command DOT                    { () }
| EOF                            { raise End_of_file }


 
int: INTCONST  { $1 }

rat:
  INTCONST    { Mpa.Q.of_z $1 }
| RATCONST    { $1 }
;

name: IDENT  { Name.of_string $1 }

namelist:    
  name                { [$1] }
| name COMMA namelist { $1 :: $3 }   /* avoid reversing list */
;


term:
  var              { $1 }
| app              { $1 }
| LPAR term RPAR   { $2 }
| arith            { $1 }     /* infix/mixfix syntax */
| array            { $1 }
| bv               { $1 }
| product          { $1 }
| boolean          { $1 }
| coproduct        { $1 }
| list             { $1 }
| cl               { $1 }
| propset          { $1 }
;


var: name          { Term.mk_var $1 }

app: 
  name LPAR appargs RPAR        
    { let n = $1 and args = $3 in
	try
	  let xl, a = Symtab.Get.term n (List.length args) in
	    failwith "to do"
	    (* Term.replace_star a xl args *)
	with
	    Not_found -> 
	      (match args with
		 | [] -> U.mk_const n 
		 | [b] -> U.mk_app n b
		 | bl -> U.mk_app n (Product.mk_tuple bl)) }
| funsym LPAR appargs RPAR      { Term.sigma $1 (Term.Args.of_list $3) }
;

funsym: name LCUR theory RCUR   { Funsym.create $3 $1 }

theory: IDENT                   { Theory.of_string $1 }

appargs:                        { [] }
| term                          { [$1] }
| term COMMA appargs            { $1 :: $3 }   /* avoid reversing list. */
;

list: 
  term LISTCONS term            { Coproduct.mk_iterated_inj 1 (Product.mk_cons $1 $3) }
| HEAD LPAR term RPAR           { Product.mk_car (Coproduct.mk_iterated_out 1 $3) }
| TAIL LPAR term RPAR           { Product.mk_cdr (Coproduct.mk_iterated_out 1 $3) }
| NIL                           { Coproduct.mk_iterated_inj 0 (Bitvector.mk_eps()) }
;

cl: 
  term APPLY term               { Apply.mk_apply $1 $3 }
| S                             { Apply.mk_s () }
| K                             { Apply.mk_k () }
| I                             { Apply.mk_i () }
| C                             { Apply.mk_c () }
/*
| LAMBDA namelist COLON term    { let nl = List.rev $2 in   (* in reverse order! *)
				  let body = $4 in
				  let rec abstract_star acc = function
				    | [] -> assert false
				    | [n] -> Apply.abstract n acc
				    | n :: nl -> abstract_star (Apply.abstract n acc) nl
				  in 
				  abstract_star body nl }
 */
;

boolean: 
  TRUE                          { Boolean.mk_true() }
| FALSE                         { Boolean.mk_false() }
;
     
arith:
  rat                           { Linarith.mk_num $1 }
| term PLUS term                { Format.eprintf "\nArg1 %s" (Term.to_string $1);
				  Format.eprintf "\nArg2 %s@." (Term.to_string $3); 
                                  Linarith.mk_add $1 $3 }
| term MINUS term               { Linarith.mk_sub $1 $3 }
| MINUS term %prec prec_unary   { Linarith.mk_neg $2 }
| term TIMES term               { Nl.Nonlin.mk_mult $1 $3 }
| term DIVIDE term              { Nl.Nonlin.mk_div $1 $3 }
| term EXPT int                 { Nl.Nonlin.mk_expt $1 (Mpa.Z.to_int $3) }
;

product:
  CONS LPAR term COMMA term RPAR { Product.mk_cons $3 $5 }
| CAR LPAR term RPAR             { Product.mk_car $3 }
| CDR LPAR term RPAR             { Product.mk_cdr $3 }
;

coproduct:
  INL LPAR term RPAR                    { Coproduct.mk_in Coproduct.Left $3 }
| INR LPAR term RPAR                    { Coproduct.mk_in Coproduct.Right  $3 }
| OUTL LPAR term RPAR                   { Coproduct.mk_out Coproduct.Left  $3 }
| OUTR LPAR term RPAR                   { Coproduct.mk_out Coproduct.Right  $3 }
| INJ LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_iterated_inj (Mpa.Z.to_int $3) $6 }
| OUT LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_iterated_out (Mpa.Z.to_int $3) $6 }
;

array:
  CREATE LPAR term RPAR           { Funarr.mk_create $3 }
| term LBRA term ASSIGN term RBRA { Funarr.mk_update $1 $3 $5 }
| term LBRA term RBRA             { Funarr.mk_select $1 $3 }
;


propset:
  EMPTY                          { Propset.mk_empty() }
| FULL                           { Propset.mk_full() }
| term UNION term                { Propset.mk_union $1 $3 }
| term INTER term                { Propset.mk_inter $1 $3 }
| COMPL term %prec prec_unary    { Propset.mk_compl $2 }
;


bv: 
  BVCONST               { Bitvector.mk_const (Bitv.from_string $1)  }
| bvconc                { let n, m, a, b = $1 in Bitvector.mk_conc n m a b  }
| bvsub                 { let n, i, j, a = $1 in Bitvector.mk_sub n i j a }
;

bvsub: 
| SUB LBRA INTCONST COMMA INTCONST COMMA INTCONST RBRA LPAR term RPAR 
    { (Mpa.Z.to_int $3, Mpa.Z.to_int $5, Mpa.Z.to_int $7, $10) }
/*
| term LBRA INTCONST COLON INTCONST RBRA 
     { 
        match Istate.width_of $1 with
	 | Some(n) -> 
	     if n < 0 then
	       raise(Invalid_argument ("Negative length of " ^ Term.to_string $1))
	     else if not(0 <= $3 && $3 <= $5 && $5 < n) then
	       raise(Invalid_argument ("Invalid extraction from " ^ Term.to_string $1))
	     else 
	       Bitvector.mk_sub n $3 $5 $1
	 | None ->  
	     raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
*/
;

bvconc: 
  CONC LBRA INTCONST COMMA INTCONST RBRA LPAR term COMMA term RPAR  
    { (Mpa.Z.to_int $3, Mpa.Z.to_int $5, $8, $10) }
/*
| term BVCONC term  
    { match Istate.width_of $1, Istate.width_of $3 with
	| Some(n), Some(m) -> 
	    if n < 0 then invalid_arg ("Negative length of " ^ Term.to_string $1)
	    else if m < 0 then invalid_arg ("Negative length of " ^ Term.to_string $3)
	    else (n, m, $1, $3)
	 | Some _, _ -> invalid_arg (Term.to_string $3 ^ " not a bitvector.")
	 | _ -> invalid_arg (Term.to_string $1 ^ " not a bitvector.") }
*/
;


atom: 
  FF                            { Atom.mk_false }
| TT                            { Atom.mk_true }
| term EQUAL term               { Atom.mk_equal $1 $3 }
| term DISEQ term               { Atom.mk_diseq $1 $3 }
| term LESS term                { Linarith.Atom.mk_lt $1 $3 }
| term GREATER term             { Linarith.Atom.mk_gt $1 $3 }
| term LESSOREQUAL term         { Linarith.Atom.mk_le $1 $3 }
| term GREATEROREQUAL term      { Linarith.Atom.mk_ge $1 $3 }
| term SUBSET term              { Atom.mk_equal (Propset.mk_inter $1 $3) $1 }
| term IN cnstrnt               { Atom.mk_cnstrnt $1 $3 }
;

conjunction:                    { [] }
| term EQUAL2 term MOD INTCONST { Linarith.Atom.mk_modeq $1 $3 $5 }
| conjunction COMMA atom        { $3 :: $1 }
;

prop:
  LBRA prop RBRA                  { $2 } 
| name                            { try 
				      let _, p = Symtab.Get.prop $1 0 in p
				    with
					Not_found -> Prop.mk_var $1 }
| atom                            { Prop.mk_poslit $1 }
| prop CONJ prop                  { Prop.mk_conj2 $1 $3 }
| prop DISJ prop                  { Prop.mk_disj2 $1 $3 }
| prop BIIMPL prop                { Prop.mk_iff $1 $3 }
| prop XOR prop                   { Prop.mk_neg (Prop.mk_iff $1 $3) }
| prop IMPL prop                  { Prop.mk_disj2 (Prop.mk_neg $1) $3 }
| NEG prop %prec prec_unary       { Prop.mk_neg $2 }
| IF prop THEN prop ELSE prop END { Prop.mk_ite $2 $4 $6 }
;

cnstrnt: 
  INT                    { Cnstrnt.Int }
| NONINT                 { Cnstrnt.Nonint }
| REAL                   { Cnstrnt.Real }
| BV LBRA INTCONST RBRA  { Cnstrnt.Bitvector(Mpa.Z.to_int $3) }
;



/*** Commands ***/

command:
  CAN optname term           { Istate.do_can ($2, $3) }
| ASSERT optname atom        { Istate.do_process1 ($2, $3) }
| ASSERT optname conjunction { Istate.do_process ($2, $3) }
| DEFTERM name optargs ASSIGN term { Istate.do_define ($2, Istate.Term($3, $5)) }
| DEFPROP name optargs ASSIGN prop  { Istate.do_define ($2, Istate.Prop($3, $5)) }
| DEFTHEORY name ASSIGN spec       { Istate.do_define ($2, Istate.Spec($4)) }
| RESET                      { Istate.do_reset () }
| SAVE optname               { Istate.do_save($2) }
| RESTORE name               { Istate.do_restore $2 }
| REMOVE name                { Istate.do_remove $2 }
| FORGET                     { Istate.do_forget() }
| EXIT                       { raise End_of_file }
| DROP                       { raise (Failure "drop") }
| SYMTAB optname             { Istate.do_symtab $2 }
| CTXT optname               { Istate.do_ctxt $2 }
| CONFIG optname             { Istate.do_config $2 }
| STATUS optname             { Istate.do_status $2 }
| SIGMA term                 { Istate.do_sigma $2 }
| term CMP term              { Istate.do_cmp ($1, $3) }
| SHOW optname               { Istate.do_show ($2, None) }
| SHOW optname eqth          { Istate.do_show ($2, (Some($3))) }
| FIND optname th term       { Istate.do_find ($2, Some $3, $4) }
| INV optname th term        { Istate.do_inv ($2, $3, $4) }
| USE optname th term        { Istate.do_dep ($2, $3, $4) }
| RESOLVE optname            { Istate.do_resolve $2 }
| EVAL optname atom          { Istate.do_eval ($2, $3) }
| SOLVE term EQUAL term      { Istate.do_solve ($2, $4) }		
| SAT optname prop           { Istate.do_sat ($2, $3) }
| ECHO STRING                { Format.eprintf "%s@." $2 }
| GET optname                { Istate.do_get($2) }
| name ASSIGN value          { Istate.do_set ($1, $3)}
| UNDO                       { Istate.do_undo () }
| LOAD optname IDENT         { Istate.do_load ($2, $3) } 
| help                       { $1 }
;

optargs:                     { [] }
| LPAR namelist RPAR         { $2 }

value : IDENT                { $1 }
| TRUE                       { "true" }
| FALSE                      { "false" }
;

optname:                     { None }
| KLAMMERAFFE name           { Some($2) }
;
		
th: IDENT                   { Theory.of_string $1 } /* may raise [Invalid_argument]. */

eqth : IDENT                { if $1 = "v" then None else Some(Theory.of_string $1) }


optth:                      { None }
| th                        { Some($1) }
;

/*** Specifications ***/

spec: 
  THEORY IDENT description SIGNATURE signature AXIOMS axioms END 
    { let th = Theory.create $2 in
      let signature = $5 th in         (* evaluate funsyms and axioms *)
      let chains, rewrites = $7 th in  (* in a context [th]. *)
	Spec.make th signature rewrites chains }

description:               { "" }
| DESCRIPTION STRING       { $2 }
;

signature: 
  defsym                   { fun th -> Funsym.Set.singleton ($1 th) }
| signature COMMA defsym   { fun th -> Funsym.Set.add ($3 th) ($1 th) }
;

defsym: name               { fun th -> Funsym.create th $1 } 


axioms:                    { fun _ -> [], [] }           
| chain                    { fun th -> [$1 th], [] }
| rewrite                  { fun th -> [], [$1 th] }
| axioms COMMA chain       { fun th -> let cl, rl = $1 th in ($3 th :: cl),  rl }
| axioms COMMA rewrite     { fun th -> let cl, rl = $1 th in cl, ($3 th :: rl) }
;

chain: axiomid hyps REDUCE chainatom            
                           { fun th -> Axioms.Chain.make $1 ($2 th) ($4 th) }

rewrite : axiomid hyps REWRITE lapp EQUAL lterm 
                           { fun th -> Axioms.Rewrite.make $1 ($2 th) ($4 th) ($6 th) }

axiomid : IDENT COLON      { Name.of_string $1 }

hyps:                      { fun _ -> [] }
| chainatom                { fun th -> [$1 th ] }
| hyps COMMA chainatom     { fun th -> $3 th :: $1 th }
;

chainatom: 
| lterm EQUAL lterm        { fun th -> Axioms.Atom.mk_equal ($1 th) ($3 th) }
| lterm DISEQ lterm        { fun th -> Axioms.Atom.mk_diseq ($1 th) ($3 th) }
;

lterm:       
  lvar                     { fun _ -> Axioms.Lterm.mk_var $1 }
| lapp                     { fun th -> let f, al = $1 th in Axioms.Lterm.mk_app f al }
;
   
lvar: IDENT                  { $1 }

lapp: name LPAR lterms RPAR  { fun th -> Funsym.create th $1, ($3 th) }

lterms: list_of_lterms       { fun th -> List.rev ($1 th) }

list_of_lterms:              { fun _ -> [] }
| lterm                      { fun th -> [$1 th] }
| list_of_lterms COMMA lterm { fun th -> $3 th :: $1 th }    
;    

/*** Help command ***/

help:
  HELP                      { Istate.do_help Istate.All }
| HELP CAN                  { Istate.do_help (Istate.Command("can")) }
| HELP HELP                 { Istate.do_help (Istate.Command("help")) }
| HELP SIMPLIFY             { Istate.do_help (Istate.Command("simplify")) }
| HELP ASSERT               { Istate.do_help (Istate.Command("assert")) }
| HELP DEFTERM              { Istate.do_help (Istate.Command("def")) }
| HELP DEFPROP              { Istate.do_help (Istate.Command("def")) }
| HELP DEFTHEORY            { Istate.do_help (Istate.Command("def")) }
| HELP RESET                { Istate.do_help (Istate.Command("reset")) }
| HELP SAVE                 { Istate.do_help (Istate.Command("save")) }
| HELP RESTORE              { Istate.do_help (Istate.Command("restore")) }
| HELP REMOVE               { Istate.do_help (Istate.Command("remove")) }
| HELP FORGET               { Istate.do_help (Istate.Command("forget")) }
| HELP EXIT                 { Istate.do_help (Istate.Command("exit")) }
| HELP DROP                 { Istate.do_help (Istate.Command("drop")) }
| HELP SYMTAB               { Istate.do_help (Istate.Command("symtab")) }
| HELP CTXT                 { Istate.do_help (Istate.Command("ctxt")) }
| HELP SIGMA                { Istate.do_help (Istate.Command("sigma")) }
| HELP CMP                  { Istate.do_help (Istate.Command("cmp")) }
| HELP SHOW                 { Istate.do_help (Istate.Command("show")) }
| HELP FIND                 { Istate.do_help (Istate.Command("find")) }
| HELP INV                  { Istate.do_help (Istate.Command("inv")) }
| HELP USE                  { Istate.do_help (Istate.Command("use")) }
| HELP SOLVE                { Istate.do_help (Istate.Command("solve")) }
| HELP SAT                  { Istate.do_help (Istate.Command("sat")) }
| HELP MODEL                { Istate.do_help (Istate.Command("model")) }
| HELP RESOLVE              { Istate.do_help (Istate.Command("resolve")) }
| HELP ECHO                 { Istate.do_help (Istate.Command("echo")) }
| HELP GET                  { Istate.do_help (Istate.Command("get")) }
| HELP SUP                  { Istate.do_help (Istate.Command("sup")) }
| HELP INF                  { Istate.do_help (Istate.Command("inf")) }
| HELP ASSIGN               { Istate.do_help (Istate.Command("set")) }
| HELP LESS IDENT GREATER   { Istate.do_help (Istate.Nonterminal($3)) }
| HELP UNDO                 { Istate.do_help (Istate.Command("undo")) }
| HELP LOAD                 { Istate.do_help (Istate.Command("load")) }
| HELP THEORY               { Istate.do_help (Istate.Command("theory")) }
;


%%
