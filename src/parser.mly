
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

let out = State.outchannel

let pr str =  Format.fprintf (out()) str

let nl () = pr "\n"

let pr_cnstrnt str =
  let cnstrnt = State.cnstrnt_of () in
  if not(Ptmap.is_empty cnstrnt) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.map Number.pp (out()) (State.cnstrnt_of ())
    end

let pr_diseq str =
  let dm = State.diseq_of () in
  if not(Ptmap.is_empty dm) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.map Pretty.tset (out()) dm
    end

let pr_find =
  let ths = [Theory.u; Theory.a; Theory.t; Theory.bv; Theory.nla] in
  fun () -> 
    let prnt th =
      let m = State.find_of th in
      if not(Ptmap.is_empty m) then
	(pr "\n%s: " (Theory.name_of th); Pretty.tmap (out()) m)
    in
    List.iter prnt ths

let pr_prop str = 
  let p = State.prop_of () in
  if not(Prop.eq p Prop.mk_tt) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.prop (out()) p
    end

let pr_use () =
  let ths = [Theory.u; Theory.a; Theory.t; Theory.bv; Theory.nla] in
  let prnt th = 
    let m = State.use_of th in
    if not(Ptmap.is_empty m) then
      (pr "\n%s: " (Theory.name_of th); Pretty.map Pretty.tset (out()) m)
  in
  List.iter prnt ths

(*s Get help texts from files with ending [.help] in directory [helppath]. *)

let helppath = "/homes/ruess/ics/help/"

let help filename =
  let inch = open_in (helppath ^ filename ^ ".help") in
  let outch = State.outchannel () in
  try
    while true do
      Format.pp_print_char outch (input_char inch)
    done
  with
      End_of_file -> State.flush ()


(*s Getting the width of bitvector terms from the signature. *)

let width_of a =
  match Bv.width a with
    | Some(n) -> n
    | _ -> raise (Invalid_argument "Not a bitvector")

let equal_width_of (a,b) =
  let n = width_of a and m = width_of b in
  if n <> m then
    raise (Invalid_argument "Bitvector terms with different lengths")
  else
    n

%}

%token DROP CAN ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG
%token TYPE SIGMA
%token SOLVE HELP DEF TOGGLE SET VERBOSE PRETTY CMP FIND PROP SHOW CNSTRNT 
%token DISEQ USE CTXT SAT
%token EOF

%token ARITH TUPLE ENUM

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
%token CONC SUB BWITE BWAND BWOR BWXOR BWNOT
%token BVCONCI BWANDI BWORI BWXORI
%token EQUAL DISEQ
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token WITH CONS CAR CDR NIL
%token PROJ
%token IF THEN ELSE END
%token NEG CONJ DISJ XOR IMPL BIIMPL
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

%type <Sym.uninterp> funsymeof
%type <Arity.t> arityeof
%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Prop.t> propeof
%type <Type.t> cnstrnteof
%type <unit> commands

%start funsymeof
%start arityeof
%start termeof
%start atomeof
%start propeof
%start cnstrnteof
%start commands

%%

funsymeof : funsym EOF       { $1 }
arityeof : arity EOF         { $1 }
termeof : term EOF           { $1 }
atomeof : atom EOF           { $1 }
cnstrnteof : cnstrnt EOF     { $1 }
propeof : prop EOF           { $1 }

commands : command DOT       { $1 }
| EOF                        { raise End_of_file }

rat:
  INTCONST  { Q.of_int $1 }
| RATCONST  { $1 }
;

funsym: 
  name                 { State.funsym_of 0 $1 }
| name LCUR arity RCUR { ($1, $3) }

name: IDENT            { Name.of_string $1 }

arity: 
  cnstrnt                          { Arity.mk_constant $1 }
| LBRA cnstrntlist TO cnstrnt RBRA { Arity.mk_functorial $2 $4 }
;

term:
  const            { $1 }
| app              { $1 }
| arith            { $1 }
| tuple            { $1 }
| boolean          { $1 }
| sexpr            { $1 }
| bv               { $1 }
| enum             { $1 }
| builtin          { $1 }
| LPAR term RPAR   { $2 }
;

const: name               { State.constant_of $1 }
| name LCUR arity RCUR    { Uninterp.mk_uninterp ($1,$3) [] }

app: 
  name LCUR arity RCUR LPAR termlist RPAR { Uninterp.mk_uninterp ($1,$3) (List.rev $6) }
| name LPAR termlist RPAR                 { let args = List.rev $3 in
					    let n = List.length args in
					    Uninterp.mk_uninterp (State.funsym_of n $1) $3 }

builtin: 
  UNSIGNED LPAR term RPAR                 { Builtin.mk_unsigned $3 }
;
     
arith: 
  rat                              { Linarith.mk_num $1 }
| term PLUS term                   { Linarith.mk_add $1 $3 }
| term MINUS term                  { Linarith.mk_sub $1 $3 }
| term TIMES term                  { Nonlin.mk_mult ($1,$3) }
| MINUS term %prec prec_unary      { Linarith.mk_neg $2 }
| term EXPT INTCONST               { Nonlin.mk_expt $3 $1 }
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

bv:
  BVCONST             { Bv.mk_const (Bitv.from_string $1) }
| CONC LBRA INTCONST COMMA INTCONST RBRA LPAR term COMMA term RPAR 
                      { Bv.mk_conc $3 $5 $8 $10 }
| SUB LBRA INTCONST COMMA INTCONST COMMA INTCONST RBRA LPAR term RPAR
                      { Bv.mk_sub $3 $5 $7 $10 }
| BWITE LBRA INTCONST RBRA LPAR term COMMA term COMMA term RPAR
                      { Bv.mk_bitwise $3 $6 $8 $10 }
| BWAND LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bv.mk_bitwise $3 $6 $8 (Bv.mk_zero $3) }
| BWOR LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bv.mk_bitwise $3 $6 (Bv.mk_one $3) $8 }
| BWXOR LBRA INTCONST RBRA LPAR term COMMA term RPAR
                      { Bv.mk_bitwise $3 $6 (Bv.mk_bitwise $3 $8 (Bv.mk_zero $3) (Bv.mk_one $3)) $8 }
| BWNOT LBRA INTCONST RBRA LPAR term RPAR 
                      { Bv.mk_bitwise $3 $6 (Bv.mk_zero $3) (Bv.mk_one $3) }
| term BVCONCI term   { Bv.mk_conc (width_of $1) (width_of $3) $1 $3 }
| term LBRA INTCONST COLON INTCONST RBRA 
                      { Bv.mk_sub (width_of $1) $3 $5 $1 } 
| term BWANDI term    { let n = equal_width_of ($1, $3) in
			Bv.mk_bitwise n $1 $3 (Bv.mk_zero n) }
| term BWORI term     { let n = equal_width_of ($1, $3) in
		        Bv.mk_bitwise n $1 (Bv.mk_one n) $3 }
| term BWXORI term    { let n = equal_width_of ($1, $3) in
		        Bv.mk_bitwise n $1 (Bv.mk_bitwise n $3 (Bv.mk_zero n) (Bv.mk_one n)) $1 }
;

enum:
  ENUM LBRA name RBRA LPAR name RPAR
                      { Enum.mk_enum (State.enumtype_of $3) $6 }

boolean:  
  TRUE               { Bool.mk_tt }
| FALSE              { Bool.mk_ff }


atom:
  term EQUAL term                   { Atom.mk_equal $1 $3 }
| EQUAL LPAR term COMMA term RPAR   { Atom.mk_equal $3 $5 }
| term DISEQ term                   { Atom.mk_diseq $1 $3 }
| term LESS term                    { Atom.mk_lt $1 $3 }
| term GREATER term                 { Atom.mk_lt $3 $1 }
| term LESSOREQUAL term             { Atom.mk_le $1 $3 }
| term GREATEROREQUAL term          { Atom.mk_le $3 $1 }
| term IN numcnstrnt                { Atom.mk_in $3 $1 }
;

prop :
  atom                       { Prop.mk_poslit $1 }
| NEG LPAR atom RPAR         { Prop.mk_neglit $3 }
| prop CONJ prop             { Prop.mk_conj  $1 $3 }
| prop DISJ prop             { Prop.mk_disj  $1 $3 }
| prop XOR prop              { Prop.mk_xor  $1 $3 }
| prop IMPL prop             { Prop.mk_imp  $1 $3 }
| prop BIIMPL prop           { Prop.mk_iff  $1 $3 }
| IF atom THEN prop ELSE prop END { Prop.mk_ite  $2 $4 $6 }
| LPAR prop RPAR             { $2 }
;

cnstrnt:
  numcnstrnt    { Type.mk_number $1 }
| nonnumcnstrnt { $1 }

nonnumcnstrnt:
  BOT                     { Type.mk_bot }
| LCUR idents RCUR        { Type.mk_enumerative $2 }
| name                    { State.type_of $1 }
| BV LBRA INTCONST RBRA   { Type.mk_bitvector (Some($3)) }
| TOP                     { Type.mk_top }
;

numcnstrnt:
  INT                     { Number.mk_int }
| REAL                    { Number.mk_real }
| INT intervals           { Number.make (Dom.Int, Intervals.of_list Dom.Int $2) }
| NONINT intervals        { Number.make (Dom.Nonint, Intervals.of_list Dom.Int $2) }
| intervals               { Number.make ( Dom.Real, Intervals.of_list Dom.Real $1) }
;

idents: 
  IDENT              { Name.Set.singleton (Name.of_string $1) }
| idents COMMA IDENT { Name.Set.add (Name.of_string $3) $1 }

cnstrntlist:
  cnstrnt                   { [$1] }
| cnstrntlist COMMA cnstrnt { $3 :: $1 }
;

intervals:         
  interval                  { [$1] }
| intervals UNION interval  { $3 :: $1 }
;

interval: 
   leftendpoint DDOT rightendpoint { Interval.make Dom.Real $1 $3 }

leftendpoint:
  LPAR NEGINF     { Interval.neginf }
| LPAR rat        { Interval.strict $2 }
| LBRA rat        { Interval.nonstrict $2 }
;

rightendpoint:
  INF RPAR          { Interval.posinf }
| rat RPAR          { Interval.strict $1 }
| rat RBRA          { Interval.nonstrict $1 }
;

termlist:
  term                { [$1] }
| termlist COMMA term { $3 :: $1 }
;

optterm:      { None }
| term        { Some($1) }
;

eqtheory: IDENT     { Theory.of_name $1 }
| BV                { Theory.bv }

command:
  CAN term                  { Pretty.term (out()) (State.can $2) }
| CAN prop                  { let s = State.current () in
			      Pretty.prop (out()) (Can.prop s $2) }
| ASSERT prop               { match State.process $2 with
				| Process.Valid -> pr "Valid."
				| Process.Inconsistent -> pr "Unsat."
				| Process.Satisfiable _ -> () }
| DEF name ASSIGN term      { State.def $2 $4 }
| SIG name COLON arity      { State.sgn $2 $4 }
| TYPE name ASSIGN cnstrnt  { State.typ $2 $4 }
| RESET                     { State.reset (); }
| SAVE name                 { State.save $2 }         
| RESTORE name              { State.restore $2 }
| REMOVE name               { State.remove $2 }
| FORGET                    { State.forget () }
| SUB name name             { match State.sub $2 $3 with
				| Three.Yes -> pr "Yes."
				| Three.No -> pr "No."
				| Three.X -> pr "X." }
| EXIT                      { raise End_of_file }
| DROP                      { failwith "drop" }
| symtab                    { $1 }
| CTXT                      { Pretty.list Pretty.atom (out()) (Atom.Set.elements (State.ctxt_of ())) }
| SIGMA term                { pr "val: "; Pretty.term (out()) $2 }
| SAT prop                  { match Check.sat (State.current ()) $2 with
				| None -> pr "Unsat."
				| Some(s) -> Dp.pp (out()) s }
| term CMP term             { if Term.(<<<) $1 $3 then pr "Yes." else pr "No." }
| find                      { $1 }
| diseq                     { $1 }
| PROP                      { pr_prop "" }
| SHOW                      { pr_find ();
                              pr_cnstrnt "\nc: ";  
                              pr_diseq "\nd: ";
                              pr_prop "\np: " }
| CNSTRNT term              { Pretty.cnstrnt (out()) (State.cnstrnt $2) }
| CNSTRNT                   { pr_cnstrnt "" }
| use                       { $1 }
| SOLVE LPAR eqtheory RPAR term EQUAL term
                            { match State.solve $3 ($5, $7) with
			       | Some(sl) -> Pretty.tmap (out()) (Ptmap.of_list sl)
			       | None ->  pr "Unsat." }
| VERBOSE INTCONST          { Tools.set_verbose $2 }
| TOGGLE togglevars         { State.toggle $2 }
| help                      { $1 }
;


togglevars: IDENT           { match $1 with
				| "printall" -> State.Printall
				| _ -> raise (Invalid_argument "Invalid toggle variable") }
find:
 FIND LPAR eqtheory RPAR optterm
			    { match $5 with
				| None -> Pretty.tmap (out()) (State.find_of $3)
				| Some(x) -> Pretty.term (out()) (State.find $3 x) }
| FIND                      { pr_find () }

diseq:
  DISEQ term                { Pretty.tset (out()) (State.diseq $2) }
| DISEQ                     { pr_diseq "" }

use:
 USE LPAR eqtheory RPAR optterm
			    { match $5 with
				| None -> Pretty.map Pretty.tset (out()) (State.use_of $3)
				| Some(x) -> Pretty.tset (out()) (State.use $3 x) }
| USE                       { pr_use () }



symtab:      
  SYMTAB                    { Symtab.pp (out()) (State.symtab()) }
| SYMTAB SIG                { Symtab.pp (out()) (Symtab.arity (State.symtab())) }
| SYMTAB DEF                { Symtab.pp (out()) (Symtab.def (State.symtab())) }
| SYMTAB TYPE               { Symtab.pp (out()) (Symtab.typ (State.symtab())) }
| SYMTAB SAVE               { Symtab.pp (out()) (Symtab.state (State.symtab())) }
| SYMTAB name               { try
				Symtab.pp_entry (out()) (Symtab.lookup $2 (State.symtab()))
			      with 
				  Not_found -> pr "\nNot found." }

help:
  HELP                      { help "help" }
| HELP HELP                 { help "help" }
| HELP CAN                  { help "can" }
| HELP IDENT                { help $2 }  
| HELP ASSERT               { help "assert" }
| HELP SIGMA                { help "sigma" }
| HELP SOLVE                { help "solve" }
| HELP FIND                 { help "find" }
| HELP CMP                  { help "cmp" }
| HELP VERBOSE              { help "verbose" }
| HELP RESET                { help "reset" }
| HELP DROP                 { help "drop" }
| HELP DEF                  { help "def" }
| HELP SIG                  { help "sig" } 
| HELP TYPE                 { help "type" } 
| HELP EXIT                 { help "exit" } 
| HELP SAVE                 { help "save" }
| HELP RESTORE              { help "restore" }
| HELP REMOVE               { help "remove" }
| HELP SUB                  { help "sub" }
| HELP SYMTAB               { help "symtab" }
| HELP CTXT                 { help "ctxt" }
| HELP CNSTRNT              { help "cnstrnt" }
| HELP USE                  { help "use" }
| HELP TOGGLE               { help "toggle" }
