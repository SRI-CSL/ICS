/* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* Module [Parser]: parser for ICS terms, formulas, and commands. */

%{

  let fmt = Format.std_formatter

  (** Symbol table with bindings [i |-> s], where [i] is
    an index and [s] a decision procedure state. *)
  module Symtab = struct
    let symtab = Hashtbl.create 7
    let max = ref 0
    let reset () = 
      max := 0;
      Hashtbl.clear symtab
    let extend s = 
      let i = !max in
	incr max;
	Hashtbl.add symtab i s;
	i
    let find i = Hashtbl.find symtab i
    let pp_index i = 
      Format.fprintf fmt "s!%d" i
    let pp_binding i s = 
      pp_index i;
      Format.fprintf fmt " |-> ";
      Ics.pp fmt s
    let pp () =
      Format.fprintf fmt "@[";
      Hashtbl.iter pp_binding symtab;
      Format.fprintf fmt "@]"
    let remove i = 
      Hashtbl.remove symtab i
  end

  (** Undo stack holds previous states. *)
  module Undo = struct
    let stack = Stack.create()
    let reset() = Stack.clear stack
    let push s = Stack.push s stack
    let pop () = Stack.pop stack
    let is_empty() = Stack.is_empty stack
  end

  module Ebnf = struct
    type nt = 
      | Digit | Alpha | Int | Rat | Ident | Funsym | Var | Term
      | Propvar | Predsym | Theory | Fml | Termseq

    let  (nt_to_string, string_to_nt) = 
      let l = [
	Digit, "digit";
	Alpha, "alpha";
	Int, "int";
	Rat, "rat";
	Ident, "ident";
	Funsym, "funsym";
	Var, "var";
	Term, "term";
	Propvar, "propvar";
	Predsym, "predsym";
	Theory, "theory";
	Fml, "fml";
	Termseq, "termseq"
      ]
      in
	((fun nt -> List.assoc nt l), 
	 (fun s -> 
	    let rec loop = function
	      | [] -> raise Not_found
	      | (nt, s') :: l' -> if s = s' then nt else loop l'
	    in
	      loop l))

    type t =
      | None
      | Nt of nt
      | String of string
      | Char of char
      | Range of char * char
      | Opt of t
      | Seq of t list
      | Choice of t list
      | Star of t
      | Plus of t

    let rec to_string = function
      | Nt(nt) -> Format.sprintf "<%s>" (nt_to_string nt)
      | Char(c) -> Format.sprintf "'%c'" c
      | String(s) -> Format.sprintf "\"%s\"" s
      | Opt(t) -> "[" ^ to_string t ^"]"
      | Star(t) -> Format.sprintf "(%s)*" (to_string t)
      | Plus(t) -> Format.sprintf "(%s)+" (to_string t)
      | Range(l, h) -> Format.sprintf "[%c..%c]" l h
      | Seq(tl) ->
	  let rec loop = function
	    | [] -> ""
	    | [t] -> to_string t
	    | t :: tl -> to_string t ^ " ; " ^ loop tl
	  in
	    "(" ^ loop tl ^ ")"
      | Choice(tl) -> 
	  let rec loop = function
	    | [] -> ""
	    | [t] -> to_string t
	    | t :: tl -> to_string t ^ " | " ^ loop tl
	  in
	    "(" ^ loop tl ^ ")"
      | None -> "_"

    let digit = Range('0','9')

    let alpha = Seq [Range('a','z'); Range('A','Z')]

    let int = 
      Seq [Choice [Nt Digit; Nt Alpha]; Star (Nt Digit)]

    let rat = 
      Seq [Nt Int; Char '/'; Nt Int]

    let ident = 
      Seq [Nt Alpha; Star (Choice [Nt Alpha; Nt Digit])]

    let funsym = Nt Ident

    let var = 
      Choice [Nt Ident; Seq [Char '!'; Nt Int]]

    let term = 
      Choice [
	Nt Var;
	Seq [Nt Funsym; Char '('; Nt Termseq; Char ')'];
        Seq [Char '('; Nt Term; Char ')']; 
        Nt Int;
	Nt Rat;
        Seq [Nt Term; Char '+'; Nt Term];
	Seq [Nt Term; Char '-'; Nt Term];
	Seq [Nt Term; Char '-'; Nt Term];
	Seq [Char '-'; Nt Term];	 
	Seq [Nt Int; Char '*'; Nt Term];
	Seq [Nt Rat; Char '*'; Nt Term];
	Seq [Char '<'; Plus (Nt Term); Char '>'];
	Seq [String "proj"; Char '['; Nt Int; Char ','; Nt Int; Char ']'; Char '('; Nt Term; Char ')'];
	Seq [Nt Term; Char '['; Nt Term; Char ']'];
        Seq [Nt Term; Char '['; Nt Term; String ":="; Nt Term; Char ']']
      ]

    let termseq = 
      Seq [Nt Term; Star(Seq[Char ','; Nt Term])]

    let theory = Choice [Char 'a'; Char 'f'; Char 'u'; Char 't']

    let propvar = Nt Ident

    let predsym = Nt Ident

    let fml = 
      Choice [
	Nt Propvar;
	Seq [Nt Predsym; Char '('; Nt Termseq; Char ')'];
        Seq [String "real"; Char '('; Nt Term; Char ')' ];
	Seq [Nt Term; Char '='; Nt Term];
	Seq [Nt Term; String "<>"; Nt Term];
	Seq [Nt Term; Char '>'; Nt Term];
	Seq [Nt Term; Char '<'; Nt Term];
	Seq [Nt Term; String ">="; Nt Term];
	Seq [Nt Term; String "<="; Nt Term];
	Seq [Nt Term; String "<="; Nt Term];
	Seq [Char '['; Nt Fml; Char ']'];
	Seq [Nt Fml; Char '&'; Nt Fml];
	Seq [Nt Fml; Char '|'; Nt Fml];
	Seq [Nt Fml; String "<=>"; Nt Fml];
	Seq [Nt Fml; String "#"; Nt Fml];
	Seq [Nt Fml; String "=>"; Nt Fml];
	Seq [String "~"; Nt Fml];
	Seq [String "if"; Nt Fml; String "then"; Nt Fml; String "else"; Nt Fml; String "end"]
      ]

    type descr = {
      explain : string;
      ebnf : t;
    }

    let descriptions = function
      | Digit -> { 
	  explain = "Digit";
	  ebnf = digit
	}
      | Alpha -> { 
	  explain = "Characters";
	  ebnf = alpha
	}
      | Int -> { 
	  explain = "Natural numbers (< max_int)";
	  ebnf = int
	}
      | Rat ->{ 
	  explain = "Rational numbers";
	  ebnf = rat
	}
      | Ident ->{ 
	  explain = "Identifiers";
	  ebnf = ident
	}
      | Funsym -> { 
	  explain = "Uninterpreted function symbols";
	  ebnf = funsym
	}
      | Var -> { 
	  explain = "A variable is either an external or an internal variable. \
                     External variables are identifiers, whereas internal variables are integer indices. \
                     Internal variables are usually generated by running the ICS inference system.";
	  ebnf = var
	}
      | Term -> { 
	  explain = "A term is either a variable or an application of a theory-specific function symbols to term arguments.";
	  ebnf = term
	}
      | Propvar -> { 
	  explain = "Propositional variables (predicate symbols of arity [0]. ";
	  ebnf = propvar
	}
      | Predsym -> { 
	  explain = "Uninterpreted predicate symbols of arity [1].";
	  ebnf = predsym
	}
      | Theory -> { 
	  explain = "Theories supported by ICS: \
                     'a' is linear arithmetic; \
                     't' the theory of tuples and projections; \
                     'f' is the theory of functional arrays; \
                     'u' is the theory of equality on uninterpreted function symbols.";
	  ebnf = theory
	}
      | Fml -> { 
	  explain = "Propositional constraint formulas";
	  ebnf = fml
	}
      | Termseq ->  {
	  explain = "Nonempty term sequence";
	  ebnf = termseq
	}

    let description nt = 
      let name = nt_to_string nt in
      let descr = descriptions nt in
	Format.fprintf fmt "NONTERMINAL\n   <%s>\n" name;
	Format.fprintf fmt "DEFINITION\n    %s\n" (to_string descr.ebnf);
	Format.fprintf fmt "DESCRIPTION\n    %s\n" (descr.explain);
	Format.fprintf fmt "@?"
    end

  module Cmd = struct
	    
    type description = {
      short: string;
      args : Ebnf.t;
      description : string;
      examples : string list;
      seealso : string list;
    }

    let descriptions = [
      "assert", {
	short = "Conjoin current context with argument formula";
	args = Ebnf.Nt Ebnf.Fml;
	description =  
          "The argument formula <fml> is conjoined with the current context. \
         There are three possible outcomes. First, <fml> is found to be \
         inconsistent with the current context. In this case, [assert]  \
         leaves the current context unchanged and outputs [:unsat]. \
         Second, if <fml> is detected to be valid in the current context, \
         then [:valid] is output.  Third, in case <fml> has neither been \
         demonstrated to be valid nor inconsistent in the specified context, \
         the current context is modified in such a way that the configuration \
         is equivalent to <fml> conjoined with the old logical context. \
         Notice that [assert] is incomplete in detecting inconsistencies, \
         and the [status] of modified contexts might be [:unknown]. In these \
         cases [resolve] must be called explicitly to resolve whether the \
         current context is satisfiable or not. ";
	examples = [ "assert f(v) = v."; 
                     "assert f(u) = u - 1.";
                     "assert u = v."];
	seealso = ["resolve"; "status"];
      };
      "can", {
	short = "Canonize term wrt current context";
	args = Ebnf.Nt Ebnf.Term;
	description = "";
	examples = [];
	seealso = [];
      };
      "inf", {
	short = "Return an infimum of argument term";
	args = Ebnf.Nt Ebnf.Term;	
	description = "";
	examples = [];
	seealso = [];
      };
      "sup", {
	short = "Return a supremum of argument term";
	args = Ebnf.Nt Ebnf.Term;
	description = "";
	examples = [];
	seealso = [];
      };
      "alias", {
	short = "Return a name for argument term";
	args = Ebnf.Nt Ebnf.Term;	
	description = "";
	examples = [];
	seealso = [];
      };
      "reset", {
	short = "Reinitialize to empty configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "save", {
	short =  "Save current state in symbol table";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "restore", {
	short = "Set current context to argument context";
	args = Ebnf.Nt Ebnf.Int;
	description = "";
	examples = [];
	seealso = [];
      };
      "remove", {
	short = "Remove argument state from symbol table";
	args = Ebnf.Nt Ebnf.Int;
	description = "";
	examples = [];
	seealso = [];
      };
      "forget", {
	short = "Reinitialize current context to empty context";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "simplify", {
	short = "Simplify argument formula wrt current context";
	args = Ebnf.Nt Ebnf.Fml;	
	description = "";
	examples = [];
	seealso = [];
      };
      "config", {
	short = "Return current configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "find", {
	short = "Return theory-specific interpretation of argument variabe";
	args = Ebnf.Seq([Ebnf.Nt Ebnf.Theory; Ebnf.Nt Ebnf.Var]);
	description = "";
	examples = [];
	seealso = [];
      };
      "inv", {
	short = "Inverse of find";
	args = Ebnf.Nt Ebnf.Term;	
	description = "";
	examples = [];
	seealso = [];
      };
      "context", {
	short = "Return current context";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "diseqs", {
	short = "Disequalities of current configuration";
	args = Ebnf.Choice([Ebnf.None; Ebnf.Nt Ebnf.Var]);
	description = "";
	examples = [];
	seealso = [];
      };
      "equals", {
	short = "Equalities of current configuration";
	args = Ebnf.Choice([Ebnf.None; Ebnf.Nt Ebnf.Var; Ebnf.Nt Ebnf.Theory]);
	description = "";
	examples = [];
	seealso = [];
      };
      "literals", {
	short = "Valid literals in current configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "renames", {
	short = "Renames in current configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "prop", {
	short = "Propositional formula of current configuration";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "slacks", {
	short = "Slack variables of current configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "constants", {
	short = "Constant equalities of current configuration";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "regular", {
	short = "Regular arithmetic solution set of current configuration";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "tableau", {
	short = "Tableau solution set of current configuration";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "exit", {
	short = "Exit ICS interpreter";
	args = Ebnf.None;	
	description = "";
	examples = [];
	seealso = [];
      };
      "drop", {
	short = "Drop into Ocaml interpreter (bytecode only)";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "echo", {
	short = "Print argument string";
	args = Ebnf.Nt Ebnf.Ident;
	description = "";
	examples = [];
	seealso = [];
      };
      "valid", {
	short = "Test if argument formula is valid in current context";
	args = Ebnf.Nt Ebnf.Fml;	
	description = "";
	examples = [];
	seealso = [];
      };
      "unsat", {
	short = "Test if argument formula is unsatisfiable in current context";
	args = Ebnf.Nt Ebnf.Fml;
	description = "";
	examples = [];
	seealso = [];
      };
      "symtab", {
	short = "Examine symbol table";
	args = Ebnf.Opt(Ebnf.Nt Ebnf.Int);
	description = "";
	examples = [];
	seealso = [];
      };
      "resolve", {
	short = "Run inference system to completion";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "undo", {
	short = "Undo last modification of current context";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      };
      "help", {
	short = "Print some help information";
	args = Ebnf.Opt(Ebnf.Nt Ebnf.Ident);
	description = "";
	examples = [];
	seealso = [];
      };
      "status", {
	short = "Return status of current context";
	args = Ebnf.None;
	description = "";
	examples = [];
	seealso = [];
      }
    ]

  let short_description cmd = (List.assoc cmd descriptions).short


  let[@warning "-32"] description cmd = 
    let descr = List.assoc cmd descriptions in
      Format.fprintf fmt "NAME\n   %s --- %s \n" cmd descr.short;
      Format.fprintf fmt "SYNOPSIS\n   %s %s\n" cmd (Ebnf.to_string descr.args);
      let long_descr = descr.description in
      let see_also = descr.seealso in
      let examples = descr.examples in
	if long_descr <> "" then
	  Format.fprintf fmt "DESCRIPTION\n %s\n" long_descr;
	if see_also <> [] then
	  begin
	    Format.fprintf fmt "SEE ALSO\n   ";
	    let rec loop = function
	      | [] -> ()
	      | [str] -> Format.fprintf fmt "%s" str
              | str :: strl -> Format.fprintf fmt "%s" str; Format.fprintf fmt ", "; loop strl
	    in
	      loop see_also
	  end;
	if examples <> [] then
	  begin  
	    Format.fprintf fmt "EXAMPLES";
	    let rec loop = function
	      | [] -> ()
              | str :: strl -> Format.fprintf fmt "\n  %s" str; loop strl
	    in
	      loop examples
	  end;
	Format.fprintf fmt "@?"

end 

  let status_to_string = function
    | Ics.Sat(impl) -> if Ics.Formula.is_true impl then "sat" else Format.sprintf "sat(%s)" (Ics.Formula.to_string impl)
    | Ics.Unsat _ -> "unsat"
    | Ics.Unknown -> "unknown"

  let string_to_theory = function
    | "a" | "A" -> Ics.A
    | "t" | "T" -> Ics.T
    | "f" | "F" -> Ics.F
    | "u" | "U" -> Ics.U
    | str -> invalid_arg("No such theory: " ^ str)

  let do_process fml = 
    try 
      Ics.process fml;
      (match Ics.status() with
	| Ics.Sat _ -> Format.fprintf fmt ":sat@?"
	| _ -> Format.fprintf fmt ":ok@?")
    with
	Ics.Unsatisfiable -> 
	  Format.fprintf fmt ":unsat@?"
%}

%token EOF
%token <string> IDENT
%token <int> INTCONST

%token LPAR RPAR LBRA RBRA
%token COLON COMMA BANG ASSIGN

%token PLUS MINUS TIMES DIVIDE EXPT
%token CONS CAR CDR
%token DEF

%token EQUAL DISEQ
%token LESS GREATER LESSOREQUAL GREATEROREQUAL REAL INTEGER
%token DISJ XOR IMPL BIIMPL CONJ NEG
%token IF THEN ELSE END
%token PROJ

%nonassoc LBRA
%right DISJ XOR IMPL
%left BIIMPL CONJ
%nonassoc EQUAL DISEQ LESS GREATER LESSOREQUAL GREATEROREQUAL
%left MINUS PLUS 
%left DIVIDE
%left TIMES
%right EXPT
%nonassoc prec_unary

%type <Ics.Term.t> termeof
%type <Ics.Formula.t> fmleof
%type <Ics.Term.t> term
%type <Ics.Formula.t> fml
%type <unit> command
%type <unit> commands
%type <unit> commandseof
%type <unit> commandsequence

%token DOT
%token ASSERT CAN INF SUP ALIAS
%token RESET SAVE RESTORE REMOVE FORGET
%token SIMPLIFY CONFIG FIND INV CONTEXT
%token ECHO EXIT DROP
%token VALID SYMTAB RESOLVE STATUS UNDO HELP 
%token EQUALS DISEQS LITERALS RENAMES PROP SLACKS CONSTANTS REGULAR TABLEAU

%token DEF PROP SIG SAT


%start termeof
%start fmleof
%start term
%start fml
%start command
%start commands
%start commandseof
%start commandsequence


%%

termeof : term EOF           { $1 }
fmleof : fml EOF             { $1 }
commandseof : command EOF    { () }


/*** Terms ***/

term:
  var              { Ics.var $1 }
| app              { $1 }
| LPAR term RPAR   { $2 }
| arith            { $1 }
| tuple            { $1 }
| array            { $1 }
;

var: IDENT         { Ics.Var.of_string $1 }
| BANG INTCONST    { Ics.Var.internal $2 }

app: IDENT LPAR termlist RPAR 
                   { let f = Ics.Funsym.of_string $1 in
		     let t = Ics.tuple $3 in
		       Ics.apply f t }

arith:
  INTCONST                                              { Ics.constz $1 }
| INTCONST DIVIDE INTCONST                           { Ics.constq $1 $3 }
| term PLUS term                                        { Ics.add $1 $3 }
| term MINUS term                                       { Ics.sub $1 $3 }
| MINUS term %prec prec_unary                            { Ics.minus $2 }
| INTCONST TIMES term                  { Ics.multq (Ics.Q.of_int $1) $3 }
| INTCONST DIVIDE INTCONST TIMES term { Ics.multq (Ics.Q.make $1 $3) $5 }
;

tuple:
  CONS LPAR term COMMA term RPAR          { Ics.pair $3 $5 }
| LESS termlist GREATER                  { Ics.tuple $2 }
| CAR LPAR term RPAR                     { Ics.proj 0 2 $3 }
| CDR LPAR term RPAR                     { Ics.proj 1 2 $3 }
| PROJ LPAR INTCONST COMMA INTCONST RPAR LPAR term RPAR  
                                       { Ics.proj $3 $5 $8 }
;

termlist:                                 { [] }
| term                                  { [$1] }
| term COMMA termlist               { $1 :: $3 }  
;                    /* avoid reversing list. */

array: 
  term LBRA term RBRA                   { Ics.lookup $1 $3 }
| term LBRA term ASSIGN term RBRA    { Ics.update $1 $3 $5 }
;


/*** Formulas ***/

atom: 
  IDENT             { Ics.posvar (Ics.Propvar.of_string $1) }
| IDENT LPAR termlist RPAR 
                        { let p = Ics.Predsym.uninterp $1 in
		          let t = Ics.tuple $3 in
			    Ics.poslit p t                  }
| REAL LPAR term RPAR                        {Ics.is_real $3 }
| INTEGER LPAR term RPAR                    {Ics.is_integer $3 }
| term EQUAL term                            { Ics.eq $1 $3 }
| term DISEQ term                           { Ics.deq $1 $3 }
| term LESS term                             { Ics.lt $1 $3 }
| term GREATER term                          { Ics.gt $1 $3 }
| term LESSOREQUAL term                      { Ics.le $1 $3 }
| term GREATEROREQUAL term                   { Ics.ge $1 $3 }
;


fml:
  LBRA fml RBRA                                    { $2 } 
| atom                                             { $1 }
| fml CONJ fml                      { Ics.andthen $1 $3 }
| fml DISJ fml                       { Ics.orelse $1 $3 }
| fml BIIMPL fml                      { Ics.equiv $1 $3 }
| fml XOR fml                           { Ics.xor $1 $3 }
| fml IMPL fml                      { Ics.implies $1 $3 }
| NEG fml %prec prec_unary                 { Ics.neg $2 }
| IF fml THEN fml ELSE fml END       { Ics.ite $2 $4 $6 }
;


/*** Commands ***/

commands : 
  command DOT     { () }
| EOF             { raise End_of_file }
;

commandsequence :
  command DOT commandsequence    { () }
| command DOT                    { () }
| EOF                            { raise End_of_file }

command:
  CAN term         { Format.fprintf fmt ":term ";
                     Ics.Term.pp fmt $2;
		     Format.fprintf fmt "@?" }
| SIMPLIFY fml     { Format.fprintf fmt ":fml ";
                     Ics.Formula.pp fmt $2;
		     Format.fprintf fmt "@?" }
| VALID fml        { if Ics.valid $2 then
		       Format.fprintf fmt ":true@?"
		     else
		       Format.fprintf fmt ":false@?" }
| ASSERT fml       { do_process $2 }
| RESOLVE          { let st = Ics.resolve() in
		     let res = status_to_string st in
		       Format.fprintf fmt ":%s@?" res }
| INF term         { try
		       let inf = Ics.inf $2 in
			 Format.fprintf fmt ":inf %s @?" (Ics.Q.to_string inf)
		     with
			 Not_found -> Format.fprintf fmt ":none@?" }
| SUP term         { try
		       let sup = Ics.sup $2 in
			 Format.fprintf fmt ":sup%s @?" (Ics.Q.to_string sup)
		     with
			 Not_found-> Format.fprintf fmt ":none@?" }
| ALIAS term       { Format.fprintf fmt ":alias ";
                     Ics.Var.pp fmt (Ics.alias $2);
		     Format.fprintf fmt "@?" }
| RESET            { Ics.reset(); 
		     Symtab.reset(); 
		     Undo.reset();
		     Format.fprintf fmt ":unit@?" }
| SAVE             { let s = Ics.current() in
                     let i = Symtab.extend s in
		       Undo.push s;
		       Format.fprintf fmt ":state ";
		       Symtab.pp_index i;
		       Format.fprintf fmt "@?" }
| RESTORE index    { try
		       let s = Symtab.find $2 in
			 Undo.push (Ics.current());
			 Ics.initialize s;
			 Format.fprintf fmt ":unit@?"
		     with
			 Not_found -> 
			   Format.fprintf fmt 
			      ":error(symtab) unknown state %i@?" $2}
| REMOVE index     { Symtab.remove $2;
		     Format.fprintf fmt ":unit@?" }
| FORGET           { let s = Ics.current() in
		       Undo.push s;
		       Ics.reset();
		       Format.fprintf fmt ":unit@?" }
| UNDO             { if Undo.is_empty() then 
		       Format.fprintf fmt ":none@?" 
		     else
		       let s = Undo.pop() in
			 Ics.initialize s;
			 Format.fprintf fmt ":unit@?" }
| STATUS           { let res = status_to_string (Ics.status()) in
		       Format.fprintf fmt ":%s@?" res }
| CONTEXT          { Format.fprintf fmt ":context ";
		     Ics.pp_context();
		     Format.fprintf fmt "@?" }
| CONFIG           { Format.fprintf fmt ":config\n";
		     Ics.pp_config ();
		     Format.fprintf fmt "@?" }
| EQUALS IDENT     { try
		       let th = string_to_theory $2 in
			 Format.fprintf fmt ":formulas ";
			 Ics.Formulas.pp fmt (Ics.theory_equals th);
			 Format.fprintf fmt "@?"
		     with
			 Invalid_argument _ -> 
			   let xs = Ics.V.eqs (Ics.Var.of_string $2) in
			     Format.fprintf fmt ":vars ";
			     Ics.V.Varset.pp fmt xs;
			     Format.fprintf fmt "@?" }
| EQUALS           { Format.fprintf fmt "\n:map"; 
		     Ics.Vareqs.pp fmt (Ics.var_equals());
		     Format.fprintf fmt "@?" }
| DISEQS var       { let xs = Ics.V.deqs $2 in
		       Format.fprintf fmt ":vars ";
		       Ics.V.Varset.pp fmt xs;
		       Format.fprintf fmt "@?" }
| DISEQS           { Format.fprintf fmt "\n:formulas"; 
		     Ics.Formulas.pp fmt (Ics.var_diseqs());
		     Format.fprintf fmt "@?" }
| LITERALS         { Format.fprintf fmt "\n:formulas"; 
		     Ics.Formulas.pp fmt (Ics.literals());
		     Format.fprintf fmt "@?" }
| RENAMES          { Format.fprintf fmt "\n:formulas"; 
		     Ics.Rename.pp fmt (Ics.renames());
		     Format.fprintf fmt "@?" }
| PROP             { Format.fprintf fmt "\n:formula"; 
		     Ics.Formula.pp fmt (Ics.prop());
		     Format.fprintf fmt "@?" }
| SLACKS           { Format.fprintf fmt "\n:vars"; 
		     Ics.Vars.pp fmt (Ics.slacks());
		     Format.fprintf fmt "@?" }
| CONSTANTS        { Format.fprintf fmt "\n:formulas"; 
		     Ics.Formulas.pp fmt (Ics.constant_equals());
		     Format.fprintf fmt "@?" }
| REGULAR          { Format.fprintf fmt "\n:formulas"; 
		     Ics.Formulas.pp fmt (Ics.regular_equals());
		     Format.fprintf fmt "@?" }
| TABLEAU          { Format.fprintf fmt "\n:formulas"; 
		     Ics.Formulas.pp fmt (Ics.tableau_equals());
		     Format.fprintf fmt "@?" }
| FIND theory var  { try
		       let t = Ics.find $2 $3 in
			 Format.fprintf fmt ":term ";
			 Ics.Term.pp fmt t;
			 Format.fprintf fmt "@?"
		     with
			 Not_found -> 
			   Format.fprintf fmt ":none@?" }
| INV term         { try
		       let x = Ics.inv $2 in
			 Format.fprintf fmt ":term ";
			 Ics.Var.pp fmt x;
			 Format.fprintf fmt "@?"
		     with
			 Not_found -> 
			   Format.fprintf fmt ":none@?" }
| SYMTAB           { Format.fprintf fmt ":symtab ";
		     Symtab.pp();
		     Format.fprintf fmt "@?" }
| SYMTAB index    { try
		       let s = Symtab.find $2 in
			 Format.fprintf fmt ":state ";
			 Ics.pp fmt s;
			 Format.fprintf fmt "@?"
		     with
			 Not_found -> 
			   Format.fprintf fmt ":none@?" }
| EXIT             { raise End_of_file }
| DROP             { failwith "drop" }
| ECHO IDENT       { Format.fprintf fmt "%s@?" $2 }
| HELP IDENT       { try
		       let short = Cmd.short_description $2 in
			 Format.fprintf fmt ":string %s@?" short
		     with
			 Invalid_argument _ -> 
			   Format.fprintf fmt ":none@?" }
| HELP HELP        { Format.fprintf fmt 
		       "Use 'help cmd' for help on command 'cmd'@?" }
| HELP nt          { Ebnf.description $2 }
| HELP             { failwith "help: to do" }
| oldcmd           { $1 }
;

index : INTCONST   { $1 }

theory : IDENT     { match $1 with
		       | "a" | "A" -> Ics.A
		       | "t" | "T" -> Ics.T
		       | "f" | "F" -> Ics.F
		       | "u" | "U" -> Ics.U
		       | _ -> invalid_arg("No such theory: " ^ $1) }
;

nt : LESS IDENT GREATER      { try Ebnf.string_to_nt $2 with Not_found -> 
				 invalid_arg(Format.sprintf "No such nonterminal <%s>" $2) }

/* Following included for compatibility with older ICS. */
oldcmd:
| DEF IDENT ASSIGN term     { do_process (Ics.eq (Ics.var (Ics.Var.of_string $2)) $4) }
| PROP IDENT ASSIGN fml    { let p = Ics.Propvar.of_string $2 in
			       do_process (Ics.equiv (Ics.posvar p) $4) }
| SIG idents COLON REAL      { List.iter 
				 (fun x -> 
				    do_process 
				    (Ics.is_real 
				       (Ics.var (Ics.Var.of_string x))))
			         $2 }
| SAT fml                    { do_process $2 }
;

idents:                                { [] }
| IDENT                              { [$1] }
| idents COMMA IDENT             { $3 :: $1 }  
;              
