
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

let term_to_string =
  Tools.pp_to_string Pretty.term

let bverror a =
  raise (Invalid_argument ("Term " ^ term_to_string a ^ " not a bitvector"))

let pr_cnstrnt str =
  let cnstrnt = State.cnstrnt_of () in
  if not(Term.Map.empty == cnstrnt) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.map Number.pp (out()) (State.cnstrnt_of ())
    end

let pr_diseq str =
  let dm = State.diseq_of () in
  if not(Term.Map.empty == dm) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.map Pretty.tset (out()) dm
    end

let find = function
  | "u" -> State.u()
  | "v" -> State.v()
  | "a" -> State.a()
  | "t" -> State.t()
  | "bv" -> State.bv()
  | "nla" -> State.nla()
  | str -> raise (Invalid_argument ("No find for " ^ str))

let pr_find str =
  let m = find str in
  if not(m == Term.Map.empty) then
    (pr "\n%s: " str; Pretty.map Pretty.term (out()) m)

let pr_find_all () =
  pr_find "u"; pr_find "v"; pr_find "a"; pr_find "t";
  pr_find "bv"; pr_find "nla"

let pr_prop str = 
  let p = State.prop_of () in
  if not(Prop.eq p Prop.mk_tt) then
    begin
      Format.fprintf (out()) "%s" str;
      Pretty.prop (out()) p
    end

(*s Type from the symbol table. *)

let type_of n =
  match Symtab.lookup n (State.symtab()) with
    | Symtab.Type(c) -> c
    | _ -> 
	let str = Tools.pp_to_string Name.pp n in
	raise (Invalid_argument ("No type definition for " ^ str))


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

let length_of a =
  match Bv.width a with
    | Some(n) -> Some(n)
    | None -> 
	let f, l = Term.destruct a in
	(match Sym.destruct f, l with
	   | Sym.Uninterp(x), [] ->
	       (try
		 (match Symtab.lookup x (State.symtab()) with
		    | Symtab.Arity(n) -> Some(n)
		    | _ -> None)
	       with
		   Not_found -> None)
	   | _ -> None)

let equal_length_of a b =
 match length_of a, length_of a with
   | Some(n), Some(m) when n = m -> n
   | Some(n), None -> n
   | None, Some(n) -> n
   | Some _, Some _ ->
       raise (Invalid_argument "Argument mismatch")
   | None, None -> 
       raise (Invalid_argument ("Term " ^ term_to_string a ^ " not a bitvector"))


%}

%token DROP CAN ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG
%token TYPE SIGMA
%token SOLVE HELP DEF TOGGLE SET VERBOSE PRETTY CMP FIND PROP SHOW CNSTRNT 
%token DISEQ CTXT SAT CHECK COMPRESS
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
%token CONC SUB BWITE BWAND BWOR BWXOR BWNOT
%token BVCONCI BWANDI BWORI BWXORI
%token EQUAL DISEQ
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token WITH CONS CAR CDR NIL
%token PROJ
%token IF THEN ELSE END TT FF
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

%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Prop.t> propeof
%type <Number.t> cnstrnteof
%type <unit> commands

%start termeof
%start atomeof
%start propeof
%start cnstrnteof
%start commands

%%

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


name: IDENT            { Name.of_string $1 }

term:
  const            { $1 }
| app              { $1 }
| arith            { $1 }
| tuple            { $1 }
| boolean          { $1 }
| sexpr            { $1 }
| bv               { $1 }
| builtin          { $1 }
| LPAR term RPAR   { $2 }
;

const: name  { try
		 match Symtab.lookup $1 (State.symtab()) with
		   | Symtab.Def(a) -> a
		   | _ -> Uninterp.mk_uninterp $1 []
	       with
		   Not_found -> 
		     Uninterp.mk_uninterp $1 [] }

app: name LPAR termlist RPAR       { Uninterp.mk_uninterp $1 $3 }

builtin: 
  UNSIGNED LPAR term RPAR          { Builtin.mk_unsigned $3 }
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
| term BVCONCI term   { match length_of $1, length_of $3 with
			  | Some(n), Some(m) -> Bv.mk_conc n m $1 $3
			  | Some _, _ -> bverror $3
			  | _ -> bverror $1 }
| term LBRA INTCONST COLON INTCONST RBRA 
                      { match length_of $1 with
			  | Some(n) -> Bv.mk_sub n $3 $5 $1
			  | None ->  bverror $1 }
| term BWANDI term   { let n = equal_length_of $1 $3 in
		       Bv.mk_bitwise n $1 $3 (Bv.mk_zero n) }
| term BWORI term    { let n = equal_length_of $1 $3 in
		       Bv.mk_bitwise n $1 (Bv.mk_one n) $3 }
| term BWXORI term   { let n = equal_length_of $1 $3 in
		       Bv.mk_bitwise n $1 (Bv.mk_bitwise n $3 (Bv.mk_zero n) (Bv.mk_one n)) $1}
;

boolean:  
  TRUE               { Bool.mk_tt }
| FALSE              { Bool.mk_ff }
;


atom:
  term EQUAL term                   { Atom.mk_equal $1 $3 }
| EQUAL LPAR term COMMA term RPAR   { Atom.mk_equal $3 $5 }
| term DISEQ term                   { Atom.mk_diseq $1 $3 }
| term LESS term                    { Atom.mk_lt $1 $3 }
| term GREATER term                 { Atom.mk_lt $3 $1 }
| term LESSOREQUAL term             { Atom.mk_le $1 $3 }
| term GREATEROREQUAL term          { Atom.mk_le $3 $1 }
| term IN cnstrnt                   { Atom.mk_in $3 $1 }
;

prop :
  TT                         { Prop.mk_tt }
| FF                         { Prop.mk_ff }
| atom                       { Prop.mk_poslit $1 }
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
  INT               { Number.mk_int }
| REAL              { Number.mk_real }
| INT intervals     { Number.make (Dom.Int, Intervals.of_list Dom.Int $2) }
| NONINT intervals  { Number.make (Dom.Nonint, Intervals.of_list Dom.Int $2) }
| intervals    { Number.make (Dom.Real, Intervals.of_list Dom.Real $1) }
| name              { match Symtab.lookup $1 (State.symtab()) with
			| Symtab.Type(c) -> c
			| _ -> 
			    let str = Tools.pp_to_string Name.pp $1 in
			    raise (Invalid_argument ("No type definition for " ^ str)) }
;

intervals:         
  interval                  { [$1] }
| intervals UNION interval  { $3 :: $1 }
;

interval: 
   leftendpoint DDOT rightendpoint { Interval.make Dom.Real $1 $3 }
;

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

signature:
  BV LBRA INTCONST RBRA     { $3 }

command: 
  CAN term                  { Pretty.term (out()) (State.can_t $2) }
| CAN atom                  { Pretty.atom (out()) (State.can_a $2) }
| ASSERT prop               { match State.process_p $2 with
				| Dp.Valid -> pr "Valid."
				| Dp.Inconsistent -> pr "Unsat."
				| Dp.Satisfiable _ -> () }
| DEF name ASSIGN term      { State.def $2 $4 }
| SIG name COLON signature  { State.sgn $2 $4 }
| TYPE name ASSIGN cnstrnt  { State.typ $2 $4 }
| RESET                     { State.reset (); }
| SAVE name                 { State.save $2 }         
| RESTORE name              { State.restore $2 }
| REMOVE name               { State.remove $2 }
| FORGET                    { State.forget () }
| EXIT                      { raise End_of_file }
| DROP                      { failwith "drop" }
| symtab                    { $1 }
| CTXT                      { Pretty.list Pretty.atom (out()) (Atom.Set.elements (State.ctxt_of ())) }
| SIGMA term                { pr "val: "; Pretty.term (out()) $2 }
| SAT prop                  { match Check.sat (State.current ()) $2 with
				| None -> pr "Unsat."
				| Some(s) -> Dp.pp (out()) s }
| CHECK prop                { Pretty.prop (out()) (Check.prop (State.current ()) $2) }
| CHECK                     { Pretty.prop (out()) (Check.prop (State.current ()) Prop.mk_ff) }
| COMPRESS                  { State.compress () }
| term CMP term             { if Term.(<<<) $1 $3 then pr "Yes." else pr "No." }
| find                      { $1 }
| diseq                     { $1 }
| PROP                      { pr_prop "" }
| SHOW                      { pr_find_all();
                              pr_cnstrnt "\nc: ";  
                              pr_diseq "\nd: ";
                              pr_prop "\np: " }
| CNSTRNT term              { match State.cnstrnt $2 with
				| Some(c) -> Pretty.number (out()) c
				| None -> Format.fprintf (out()) "None." }
| CNSTRNT                   { pr_cnstrnt "" }
| SOLVE LPAR eqth RPAR term EQUAL term
                            { try
				let el = Th.solve $3 ($5, $7) in
				Pretty.list Pretty.eqn (out()) el
                              with
			        Exc.Inconsistent -> Format.fprintf (out()) "Unsat." }
| VERBOSE INTCONST          { Trace.set_verbose $2 }
| TOGGLE togglevars         { State.toggle $2 }
| help                      { $1 }
;

eqth: IDENT                 { match $1 with
				| "a" -> Th.LA
				| "bv" -> Th.BV
				| "nla" -> Th.NLA
				| "t" -> Th.T
				| name -> raise (Invalid_argument (name ^ "not an interpreted theory name.")) }

togglevars: IDENT           { match $1 with
				| "printall" -> State.Printall
				| var -> raise (Invalid_argument (var ^ " not a toggle variable.")) }
find:
 FIND LPAR IDENT RPAR optterm
			    { match $5 with
				| None -> pr_find $3
				| Some(x) -> 
				    try
				      Pretty.term (out()) (Term.Map.find x (find $3))
				    with
					Not_found -> pr "Not found" }
| FIND                      { pr_find_all () }

diseq:
  DISEQ term                { Pretty.tset (out()) (State.diseq $2) }
| DISEQ                     { pr_diseq "" }
;

symtab:      
  SYMTAB                    { Symtab.pp (out()) (State.symtab()) }
| SYMTAB SIG                { Symtab.pp (out()) (Symtab.arity (State.symtab())) }
| SYMTAB DEF                { Symtab.pp (out()) (Symtab.def (State.symtab())) }
| SYMTAB TYPE               { Symtab.pp (out()) (Symtab.typ (State.symtab())) }
| SYMTAB SAVE               { Symtab.pp (out()) (Symtab.state (State.symtab())) }
| SYMTAB name               { try
				Symtab.pp_entry (out()) (Symtab.lookup $2 (State.symtab()))
			      with 
				  Not_found ->
				    let str = Name.to_string $2 in
				    pr ("\nNot found.") }
;

help:
  HELP                      { help "help" }
| HELP IDENT                { help $2 }  
