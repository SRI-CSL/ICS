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

(** Lexer for parsing declaration. *)

{
  open Lexing
  open Mli_parser
}

let space = [' ' '\010' '\013' '\009' '\012']

let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9']

rule token = parse
  | space+     { token lexbuf }
  | "int"      { INT }
  | "bool"     { BOOL }
  | "unit"     { UNIT }
  | "string"   { STRING }
  | "value"    { VALUE }
  | "val"      { VAL }
  | identchar+ { IDENT (lexeme lexbuf) }
  | "->"       { ARROW }
  | ":"        { COLON }
  | _          { raise Parsing.Parse_error }
  | eof        { EOF }

{
  let signature_of_string s =
    let lb = from_string s in 
      Mli_parser.signature token lb

  let declaration_of_string s =
    let lb = from_string s in 
      Mli_parser.declaration token lb
}
