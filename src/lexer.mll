
(*i
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
 i*)

(*s Module [Lexer]: lexical analysis for ICS command interpreter. *)

(*i*)
{
open Lexing
open Parser
(*i*)

(*s A lexer for terms. *)

let keyword =
  let kw_table = Hashtbl.create 17 in
  List.iter 
    (fun (s,tk) -> Hashtbl.add kw_table s tk)
    [ "can", CAN; "simp", SIMP; "sigma", SIGMA; "solve", SOLVE; 
      "witness", WITNESS; "solution", SOLUTION; "reset", RESET;
      "drop", DROP; "assert", ASSERT; "find", FIND; "current", CURRENT; "show", SHOW; "undo", UNDO;
      "use", USE; "uninterp", UNINTERP; "groebner", GROEBNER; "diseqs", DISEQS; "inconsistent", INCONSISTENT;
      "check", CHECK; "verbose", VERBOSE; "ctxt", CTXT; "ext", EXT;
      "arith", ARITH; "boolean", BOOLEAN; "tuple", TUPLE; "eq", EQ;
      "commands", COMMANDS; "syntax", SYNTAX; "declare", DECLARE;
      "cnstrnt", CNSTRNT; "help", HELP;
      "int", INTDOM; "rat", RATDOM; "bool", BOOLDOM; "real", RATDOM;
      "A", A; "AC", AC; "C", C;
      "proj", PROJ;
      "unsigned", UNSIGNED;
      "true", TRUE; "false", FALSE; "if", IF; "then", THEN; "else", ELSE; "end", END;
      "integer", INTEGER_PRED
    ];
  fun s ->
    try Hashtbl.find kw_table s with Not_found -> IDENT s

(*i*)
}
(*i*)

(*s The lexer it-self is quite simple. *)

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '\'' '0'-'9']*

let space = [' ' '\t' '\r' '\n']

rule token = parse
  | space+     { token lexbuf }
  | '%' [^ '\n']* {token lexbuf }
  | ident      { keyword (lexeme lexbuf) }
  | ['0'-'9']+ { INTCONST (int_of_string (lexeme lexbuf)) }
  | ['0'-'9']+ '/' ['0'-'9']+ { RATCONST (Ics.num_of_string (lexeme lexbuf)) }
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
  | '='        { EQUAL }
  | ":="       { ASSIGN }
  | "<>"       { DISEQ }
  | "<"        { LESS }
  | "<="       { LESSOREQUAL }
  | ">"        { GREATER }
  | ">="       { GREATEROREQUAL }
  | '&'        { AND }
  | '|'        { OR }
  | '#'        { XOR }
  | '~'        { NOT }
  | "=>"       { IMPLIES }
  | "<=>"      { IFF }
  | "<<"       { CMP }
  | ":"        { COLON }
  | ';'        { SEMI }
  | '.'        { DOT }
  | '^'        { EXPT }
  | '_'        { UNDERSCORE }
  | _          { raise Parsing.Parse_error }
  | eof        { raise End_of_file }

(*i*)
{
(*i*)

(*s Parse terms and equations from strings and channels. *)

  let term_of_string s =
    let lb = from_string s in Parser.term token lb

  let term_of_channel c =
    let lb = from_channel c in Parser.term token lb

  let eqn_of_string s = 
    let lb = from_string s in Parser.equation token lb

  let eqn_of_channel c =
    let lb = from_channel c in Parser.equation token lb

(*i*)
}
(*i*)
