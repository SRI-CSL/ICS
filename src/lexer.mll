(* 
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
 *)

(** Lexical analysis for ICS syntactic categories such as terms.
  @author Jean-Christophe Filliatre
  @author Harald Ruess
*)

{
open Parser

let linenumber = ref 0

(** Lexical analysis. *)

let key = 
 [ "cons", CONS; 
   "car", CAR; 
   "cdr", CDR;
   "if", IF; 
   "then", THEN; 
   "else", ELSE; 
   "end", END;
   "proj", PROJ;
   "real", REAL;
   "int", INTEGER
]

let command = [
  "assert", ASSERT;
  "process", ASSERT;
  "can", CAN;
  "inf", INF;
  "sup", SUP;
  "alias", ALIAS;
  "reset", RESET;
  "save",  SAVE;
  "restore", RESTORE;
  "remove", REMOVE;
  "forget", FORGET;
  "simplify", SIMPLIFY;
  "config", CONFIG;
  "show", CONFIG; 
  "find", FIND;
  "inv", INV;
  "ctxt", CONTEXT;
  "context", CONTEXT;
  "exit", EXIT;
  "drop", DROP;
  "echo", ECHO;
  "valid", VALID;
  "symtab", SYMTAB;
  "resolve", RESOLVE;
  "check", RESOLVE;
  "undo", UNDO;
  "help", HELP;
  "equals", EQUALS;
  "diseqs", DISEQS;
  "status", STATUS;
  "def", DEF;    (* Following for compatibility with older ICS *)
  "prop", PROP;
  "sig", SIG;
  "sat", SAT;
  "literals", LITERALS;
  "renames", RENAMES;
  "prop", PROP;
  "slacks", SLACKS;
  "constants", CONSTANTS;
  "regular",  REGULAR;
  "tableau", TABLEAU;
  "renames", RENAMES;
]

let keyword =
  let kw_table = Hashtbl.create 31 in
  let install (s, tk) =  Hashtbl.add kw_table s tk in
    List.iter install key;
    List.iter install command;
    fun s ->
      try Hashtbl.find kw_table s with Not_found -> IDENT s

let endmarker = ref "."

let withEndmarker s f a =
  let save = !endmarker in
    endmarker := s;
    try
      let b = f a in
	endmarker := save;
	b
    with
	exc -> endmarker := save; raise exc
}

let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9']*

let int =  ['0'-'9']+  

rule token = parse 
    [' ' '\t'] { token lexbuf }
  | '\n'       { linenumber := !linenumber + 1;
  	         token lexbuf }
  | '%' [^ '\n']* {token lexbuf }
  | ident      { keyword (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { INTCONST (int_of_string (Lexing.lexeme lexbuf)) }
  | ','        { COMMA }
  | '('        { LPAR }
  | ')'        { RPAR }
  | '['        { LBRA }
  | ']'        { RBRA }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIVIDE }
  | '='        { EQUAL }
  | "<>"       { DISEQ }
  | "<"        { LESS }
  | "<="       { LESSOREQUAL }
  | ">"        { GREATER }
  | ">="       { GREATEROREQUAL }
  | '^'        { EXPT }
  | '&'        { CONJ }
  | '|'        { DISJ }
  | '#'        { XOR }
  | "<=>"      { BIIMPL }
  | "=>"       { IMPL }
  | '~'        { NEG }
  | '!'        { BANG }
  | ":="       { ASSIGN }
  | eof        { EOF }
  | _          { if Lexing.lexeme lexbuf = !endmarker then EOF else
		   raise Parsing.Parse_error }
