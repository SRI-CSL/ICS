(** Lexical analysis for ICS syntactic categories such as terms.
  @author Harald Ruess
*)

{
open SmtParser

let linenumber = ref 0
let trace = ref false

let intern = SmtBench.Ast.intern

let attribute =
  let table = Hashtbl.create 31 in
  let install (s, tk) =  Hashtbl.add table s tk in
    List.iter install [
      ":assumption", ASSUMPTION;
      ":source", SOURCE;
      ":logic", LOGIC;
      ":formula", FORMULA;
      ":status", STATUS;
      ":extrasorts", EXTRASORTS;
      ":extrafuns", EXTRAFUNS;
      ":extrapreds", EXTRAPREDS;
      ":notes", NOTES
    ];
    fun s ->  
      if !trace then Format.eprintf "\nAttribute: %s@." s;
      try Hashtbl.find table s with Not_found -> ATTRIBUTE (intern s)

let keyword =
  let table = Hashtbl.create 31 in
  let install (s, tk) =  Hashtbl.add table s tk in
    List.iter install [
      "true", TRUE;
      "false", FALSE;
      "ite", ITE;
      "not", NOT;
      "implies", IMPLIES;
      "if_then_else", IF_THEN_ELSE;
      "and",  AND;
      "or", OR;
      "xor", XOR;
      "iff", IFF;
      "exists", EXISTS;
      "forall", FORALL;
      "let", LET;
      "flet", FLET;
      "sorts", SORTS;
      "funs", FUNS;
      "preds", PREDS;
      "extensions", EXTENSIONS;
      "definition", DEFINITION;
      "axioms", AXIOMS;
      "sat", SAT;
      "unsat", UNSAT;
      "unknown", UNKNOWN;
      ":assumption", ASSUMPTION;
      "benchmark", BENCHMARK;    
      "language", LANGUAGE;  
      "distinct", DISTINCT;
      "int", INT;
      "Int", INT;
      "real", REAL;
      "Real", REAL;  
      "array", ARRAY;
      "Array", ARRAY;
      "store", STORE;
      "select", SELECT
    ];
    fun s ->
      if !trace then Format.eprintf "\nKeyword: %s@." s;
      try Hashtbl.find table s with Not_found -> SYM (intern s)
}

let special = ['=' '<' '>' '&' '@' '#' '+' '-' '*' '/' '%' '|' '~']+

let sym = ['a'-'z' 'A'-'Z' '/']['a'-'z' 'A'-'Z' '.' '-' '+' '/' '_' '\'' '0'-'9']* 

let var = '?' ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '.' '_' '0'-'9']*

let  fvar = '$' sym

let attr = ':' sym    

let num =  ['0'-'9']+  

rule token = parse  
  | [' ' '\t' '\r']      {token lexbuf}
  | '\n'                 {linenumber := !linenumber + 1; token lexbuf }
  | '"' [^ '"']* '"'     {STRING(Lexing.lexeme lexbuf)}
  | '{' [^ '{' '}']* '}' {USER_VAL(Lexing.lexeme lexbuf)}
  | sym                  {keyword (Lexing.lexeme lexbuf)}
  | num                  {NUMERAL(Z.of_string(Lexing.lexeme lexbuf))}
  | var                  {VAR(intern(Lexing.lexeme lexbuf))}
  | fvar                 {FVAR(intern(Lexing.lexeme lexbuf))}
  | attr                 {attribute (Lexing.lexeme lexbuf)}
  | '('                  {LPAREN}
  | ')'                  {RPAREN}
  | '='                  {EQUALS}
  | ';' [^'\n']* "\n"    {token lexbuf}  
  | '+'                  {ADD}
  | '-'                  {SUB}
  | '*'                  {MULT}
  | '/'                  {DIV}
  | "<="                 {LE}
  | ">="                 {GE}
  | '<'                  {LT}
  | '>'                  {GT}
  | special              {AR_SYMB(intern(Lexing.lexeme lexbuf))}
  | eof                  {EOF}
  | _                    {token lexbuf}


