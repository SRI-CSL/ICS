(*
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
 *)

(** Lexical analysis for ICS syntactic categories such as terms.
  @author Jean-Christophe Filliatre
  @author Harald Ruess
*)

{
open Parser

(** Lexical analysis. *)

let keyword =
  let kw_table = Hashtbl.create 31 in
  List.iter 
    (fun (s,tk) -> Hashtbl.add kw_table s tk)
    [ "in", IN; "notin", NOTIN;
      "int", INT; "real", REAL; "nonint", NONINT;
      "bitvector", BV; "with", WITH;
      "proj", PROJ;
      "cons", CONS; "car", CAR; "cdr", CDR;
      "conc", CONC; "sub", SUB; "ite", BWITE;
      "drop", DROP; "simplify", SIMPLIFY; "can", CAN; "assert", ASSERT; "exit", EXIT; 
      "valid", VALID; "unsat", UNSAT;
      "save", SAVE; "restore", RESTORE; "remove", REMOVE; "forget", FORGET;
      "reset", RESET; "sig", SIG; "type", TYPE; 
      "def", DEFTERM; "defterm", DEFTERM; 
      "prop", DEFPROP; "defprop", DEFPROP;
      "deftheory", DEFTHEORY;
      "sigma", SIGMA; "solve", SOLVE; "help", HELP;
      "set", SET; "toggle", TOGGLE; "get", GET; 
      "find", FIND; "inv", INV; "dep", USE; "solution", SOLUTION; "partition", PARTITION;
      "syntax", SYNTAX; "commands", COMMANDS; "ctxt", CTXT; "diseq", DISEQ; "echo", ECHO;
      "undo", UNDO;
      "config", CONFIG; "status", STATUS; "eval", EVAL; "register", REGISTER;
      "theory", THEORY; "signature", SIGNATURE; "axioms", AXIOMS; "begin", BEGIN; "description", DESCRIPTION;
      "show", SHOW; "symtab", SYMTAB; "sign", SIGN; "resolve", RESOLVE; "sat", SAT; "load", LOAD;
      "true", TRUE; "false", FALSE;
      "empty", EMPTY; "full", FULL; "union" , UNION; "inter", INTER; "diff", DIFF;
      "sub", SUBSET;
      "tt", TT; "ff", FF;
      "S", S; "K", K; "I", I; "C", C;
      "inr", INR; "inl", INL; "outr", OUTR; "outl", OUTL;
      "inj", INJ; "out", OUT;
      "hd", HEAD; "tl", TAIL;
      "mod", MOD;
      "lambda", LAMBDA;
      "if", IF; "then", THEN; "else", ELSE; "end", END;
      "create", CREATE;
      "sup", SUP;
      "inf", INF
    ];
  fun s ->
    try Hashtbl.find kw_table s with Not_found -> IDENT s

}

let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9']*

let int =  ['0'-'9']+  

rule token = parse 
    [' ' '\t'] { token lexbuf }
  | '\n'       { Tools.linenumber := !Tools.linenumber + 1;
  	         token lexbuf }
  | '%' [^ '\n']* {token lexbuf }
  | ident      { keyword (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { INTCONST (Mpa.Z.of_string (Lexing.lexeme lexbuf)) }
  | ['0'-'9']+ '/' ['0'-'9']+ 
               { RATCONST (Mpa.Q.of_string (Lexing.lexeme lexbuf)) }
  | '-' ['0'-'9']+ '/' ['0'-'9']+ 
               { let s = Lexing.lexeme lexbuf in
		   RATCONST(Mpa.Q.minus (Mpa.Q.of_string (String.sub s 1 (String.length s - 1)))) }
  | "0b" ['0'-'1']*
               { let s = Lexing.lexeme lexbuf in 
		 BVCONST (String.sub s 2 (String.length s - 2)) }
  | ident '!' int  { let s = Lexing.lexeme lexbuf in
		     let n = String.length s in
                     let i = String.rindex s '!' in
                     let x = String.sub s 0 i in
		     let k = int_of_string (String.sub s (i + 1) (n - i - 1)) in
		       FRESH (x, k) }
  | '"' [^ '"']* '"' { STRING(Lexing.lexeme lexbuf) }
  | ','        { COMMA }
  | '('        { LPAR }
  | ')'        { RPAR }
  | '['        { LBRA }
  | ']'        { RBRA }
  | '{'        { LCUR }
  | '}'        { RCUR }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIVIDE }
  | '\\'       { BACKSLASH }
  | '='        { EQUAL }
  | "=="       { EQUAL2 }
  | ":="       { ASSIGN }
  | "<>"       { DISEQ }
  | "<"        { LESS }
  | "<="       { LESSOREQUAL }
  | ">"        { GREATER }
  | ">="       { GREATEROREQUAL }
  | "->"       { TO }
  | ':'        { COLON }
  | '^'        { EXPT }
  | ".."       { DDOT }
  | "++"       { BVCONC }
  | '&'        { CONJ }
  | '|'        { DISJ }
  | '#'        { XOR }
  | "<=>"      { BIIMPL }
  | "=>"       { IMPL }
  | '~'        { NEG }
  | '_'        { UNDERSCORE } 
  | "<<"       { CMP }
  | "::"       { LISTCONS }
  | "[]"       { NIL }
  | '.'        { DOT }
  | '$'        { APPLY }
  | '@'        { KLAMMERAFFE }
  | "'"        { QUOTE }
  | "==>"      { REDUCE }
  | "-->"      { REWRITE }
  | eof        { EOF }
  | _          { raise Parsing.Parse_error }
