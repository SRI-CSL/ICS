
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
    [ "can", CAN; "sigma", SIGMA; "solve", SOLVE; "reset", RESET;
      "drop", DROP; "assert", ASSERT; "find", FIND; "universe", UNIVERSE; "use", USE;
      "check", CHECK; "verbose", VERBOSE; "norm", NORM; "compare", COMPARE;
      "sign", POLARITY; "typ", TYP;
      "proj", PROJ; "floor", FLOOR;
      "int", INT; "real", REAL; "neg", NEG; "nonneg", NONNEG; "pos", POS;
      "nonpos", NONPOS;
      "in", IN; "notin", NOTIN; "compl", COMPL; "inter", INTER; "union", UNION;
      "diff", DIFF; "symdiff", SYMDIFF; "empty", EMPTY; "full", FULL;
      "unsigned", UNSIGNED;
      "bw_and", BVAND; "bw_or", BVOR; "bw_xor", BVXOR;  "conc", BVCONC; 
      "true", TRUE; "false", FALSE; "if", IF; "then", THEN; "else", ELSE; "fi", FI;
      "integer", INTEGER_PRED;
      "forall", FORALL; "exists", EXISTS
    ];
  fun s ->
    try Hashtbl.find kw_table s with Not_found -> IDENT s

(*i*)
}
(*i*)

(*s The lexer it-self is quite simple. *)

let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']*

let space = [' ' '\t' '\r' '\n']

rule token = parse
  | space+     { token lexbuf }
  | '%' [^ '\n']* {token lexbuf }
  | ident      { keyword (lexeme lexbuf) }
  | ['0'-'9']+ { CONST (lexeme lexbuf) }
  | "0b" ['0'-'1']+ { let s = lexeme lexbuf in 
		      CONSTBV (String.sub s 2 (String.length s - 2)) }
  | ','        { COMMA }
  | '('        { LPAR }
  | ')'        { RPAR }
  | '['        { LBRA }
  | ']'        { RBRA }
  | '{'        { LCUR }
  | '}'        { RCUR }
  | '|'        { BAR }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIVIDE }
  | '='        { EQUAL }
  | "::"       { CONV }
  | ":="       { ASSIGN }
  | "<>"       { DISEQ }
  | "<"        { LESS }
  | "<="       { LESSOREQUAL }
  | ">"        { GREATER }
  | ">="       { GREATEROREQUAL }
  | "&" | "&&" { AND }
  | "|" | "||" { OR }
  | "##"       { XOR }
  | "~"        { NOT }
  | "=>"       { IMPLIES }
  | "->"       { IMPLIES }
  | "<=>"      { IFF }
  | "<->"      { IFF }
  | ":"        { COLON}
  | ';'        { SEMI }
  | '.'        { DOT }
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
