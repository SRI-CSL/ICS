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
