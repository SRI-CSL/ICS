
{
  open Lexing
  open Mli_parser
}

let space = [' ' '\010' '\013' '\009' '\012']

let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
  | space+     { token lexbuf }
  | "int"      { INT }
  | "bool"     { BOOL }
  | "unit"     { UNIT }
  | "string"   { STRING }
  | (identchar+ ".")* identchar+ { IDENT (lexeme lexbuf) }
  | '('        { LPAR }
  | ')'        { RPAR }
  | '*'        { STAR }
  | "'"        { QUOTE }
  | "->"       { ARROW }
  | ','        { COMMA }
  | _          { raise Parsing.Parse_error }
  | eof        { EOF }

{
  let mltyp_of_string s =
    let lb = from_string s in Mli_parser.mlarity token lb
}
