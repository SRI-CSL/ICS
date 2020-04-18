%{

  open SmtBench
%}

%token <Big_int.big_int> NUMERAL
%token <SmtBench.Ast.Sym.t> VAR  
%token <SmtBench.Ast.Sym.t> FVAR
%token <SmtBench.Ast.Sym.t> SYM
%token <string> STRING
%token <SmtBench.Ast.Sym.t> AR_SYMB
%token <string> USER_VAL
%token <SmtBench.Ast.Sym.t> ATTRIBUTE

%token INT
%token REAL
%token ARRAY

%token ADD
%token SUB
%token MULT
%token DIV
%token STORE
%token SELECT

%token LE
%token LT
%token GE
%token GT

%token TRUE
%token FALSE
%token ITE
%token NOT
%token IMPLIES
%token IF_THEN_ELSE
%token AND
%token OR
%token XOR
%token IFF
%token EXISTS
%token FORALL
%token LET
%token FLET
%token NOTES
%token SORTS
%token FUNS
%token PREDS
%token EXTENSIONS
%token DEFINITION
%token AXIOMS
%token SOURCE
%token LOGIC
%token COLON
%token LPAREN
%token RPAREN
%token SAT
%token UNSAT
%token UNKNOWN
%token ASSUMPTION
%token FORMULA
%token STATUS
%token BENCHMARK
%token EXTRASORTS
%token EXTRAFUNS
%token EXTRAPREDS
%token LANGUAGE
%token EQUALS
%token DISTINCT
%token SEMICOLON
%token EOF

%start benchmark
%type <unit> benchmark

%%
benchmark: LPAREN BENCHMARK name attributes RPAREN { () }

name: SYM  { $1 }

attributes:
    attributes attribute     { () }
  | attribute                { () }
;

attribute:
  | ASSUMPTION formula                         { Fill.addAssumption $2 }
  | FORMULA formula                            { Fill.setConclusion $2 }
  | STATUS status                              { Fill.setStatus $2 }
  | LOGIC logic                                { Fill.setLogic $2 }
  | EXTRASORTS LPAREN sortDecls RPAREN         { () }
  | EXTRAFUNS LPAREN funsymDecls RPAREN        { () }
  | EXTRAPREDS LPAREN predsymDecls RPAREN      { () }
  | NOTES STRING                               { () }
  | SOURCE USER_VAL                            { () }
  | annotation                                 { () }
;

logic: SYM  { try Logic.of_sym $1 with Not_found -> 
		invalid_arg("No such logic defined: " ^ Ast.extern $1) }
status:
    SAT     { Ast.Sat }
  | UNSAT   { Ast.Unsat }
  | UNKNOWN { Ast.Unknown }
;


sortDecls:
    SYM                  { Fill.addSortDecl $1 }
  | sortDecls SYM        { Fill.addSortDecl $2 }
;

sorts :
    sort                 { [$1] }
  | sort sorts       { $1 :: $2 }
;

sort : 
    INT     { Ast.Int }
  | ARRAY   { Ast.Array }
  | REAL    { Ast.Real }
  | SYM     { Ast.Extra($1) }
;

funsymDecls:
    funsymDecl                { () }
  | funsymDecls funsymDecl    { () }
;


funsymDecl:
  | LPAREN funsym sort annotations RPAREN
       { Fill.addFunsymDecl $2 {Ast.dom = []; Ast.cod = $3}  } 
  | LPAREN funsym sort RPAREN
       { Fill.addFunsymDecl $2 {Ast.dom = []; Ast.cod = $3}  } 
  | LPAREN funsym sort sorts annotations RPAREN 
       { Fill.addFunsymDecl $2 {Ast.dom = $4; Ast.cod = $3}  }
  | LPAREN funsym sort sorts RPAREN 
       { Fill.addFunsymDecl $2 {Ast.dom = $4; Ast.cod = $3}  }
;


funsym: 
  | SYM         { $1 }
  | AR_SYMB     { $1 }
;

predsymDecls:
  | predsymDecl              { () }
  | predsymDecls predsymDecl { () }
;
  

predsymDecl:
  | LPAREN predsym annotations RPAREN 
      { Fill.addPredsymDecl $2 [] } 
  | LPAREN predsym sorts annotations RPAREN  
      { Fill.addPredsymDecl $2 $3 }
  | LPAREN predsym RPAREN 
      { Fill.addPredsymDecl $2 [] } 
  | LPAREN predsym sorts RPAREN  
      { Fill.addPredsymDecl $2 $3 }
;

predsym: 
  | SYM      { $1 }
  | AR_SYMB  { $1 }
;

formulas:
  | formula                { [$1] }
  | formulas formula   { $2 :: $1 } 
;
   
formula:
    propatom                                      { $1 }
  | LPAREN propatom annotations RPAREN            { $2 }
  | LPAREN complexformula annotations RPAREN      { $2 }
  | LPAREN propatom RPAREN                        { $2 }
  | LPAREN complexformula RPAREN                  { $2 }
;


complexformula: 
  | predsym terms                           { Ast.Predapply($1, $2) }
  | LT term term                            { Ast.Lt($2, $3) }
  | LE term term                            { Ast.Le($2, $3) }
  | GT term term                            { Ast.Gt($2, $3) }
  | GE term term                            { Ast.Ge($2, $3) }
  | EQUALS term term                        { Ast.Equal($2, $3) }
  | DISTINCT term terms                     { Ast.Distinct($2 :: $3) }
  | NOT formula                             { Ast.Neg $2 }
  | IMPLIES formula formulas                { Ast.Impl($2:: $3) }
  | IF_THEN_ELSE formula formula formula    { Ast.Ite($2, $3, $4) }
  | AND formula formulas                    { Ast.Conj($2 :: $3) }
  | OR formula formulas                     { Ast.Disj($2 :: $3) }
  | XOR formula formulas                    { Ast.Xor($2 :: $3) }
  | IFF formula formulas                    { Ast.Equiv($2 :: $3) }
  | EXISTS qvars formula                    { Ast.Exists($2, $3) }
  | FORALL qvars formula                    { Ast.Forall($2, $3) }
  | LET LPAREN VAR term RPAREN formula      { Ast.Let($3, $4, $6) }
  | FLET LPAREN FVAR formula RPAREN formula { Ast.Flet($3, $4, $6) }
;           

propatom: 
    TRUE                       { Ast.True }
  | FALSE                      { Ast.False }
  | FVAR                       { Ast.Fvar($1) }
  | predsym                    { Ast.Predapply($1, []) }
;

terms:
  | term       { [$1] }
  | term terms { $1 :: $2 }
;

term:
  | basicterm                              { $1 }
  | LPAREN application annotations RPAREN  { $2 }
  | LPAREN basicterm annotations RPAREN    { $2 }     
  | LPAREN application RPAREN              { $2 }
  | LPAREN basicterm RPAREN                { $2 }    
; 

basicterm: 
  | VAR        { Ast.Var($1) }
  | NUMERAL    { Ast.Num($1) }
  | funsym     { Ast.Apply($1, []) }
;

application:
    funsym terms           { Ast.Apply($1, $2) } 
  | ADD term terms         { Ast.Add($2 :: $3) }
  | SUB term term          { Ast.Sub($2, $3) }
  | MULT term terms        { Ast.Mult($2 :: $3) }
  | DIV term term          { Ast.Div($2, $3) }
  | ITE formula term term  { Ast.Cond($2, $3, $4) }
  | SELECT term term       { Ast.Select($2, $3) }
  | STORE term term term   { Ast.Store($2, $3,$4) }

;

qvars:
  | qvar        { [$1] }
  | qvars qvar  { $2 :: $1 } 
;

qvar: 
    LPAREN VAR sort RPAREN  { ($2, $3) }

annotation:    /* ignore annotations */
  ATTRIBUTE optuserval     { () }

optuserval:   { () }
  | USER_VAL  { () } 
;

annotations:     
    annotation             { [] } 
  | annotations annotation { [] } 
;



