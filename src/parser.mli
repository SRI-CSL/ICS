type token =
    CAN
  | SIMP
  | SOLVE
  | FOR
  | ASSERT
  | FIND
  | CHECK
  | LIFT
  | USE
  | EXT
  | UNINTERP
  | SIGMA
  | VERBOSE
  | RESET
  | DROP
  | NORM
  | CMP
  | CTXT
  | CNSTRNT
  | HELP
  | COMMANDS
  | SYNTAX
  | INT
  | REAL
  | POS
  | NEG
  | NONNEG
  | NONPOS
  | IDENT of (string)
  | INTCONST of (int)
  | RATCONST of (Ics.q)
  | BV_CONST of (string)
  | BV_OR
  | BV_XOR
  | BV_AND
  | BV_CONC
  | BV_EXTR
  | BV_COMPL
  | WIDTH of (int)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | LCUR
  | RCUR
  | UNDERSCORE
  | COLON
  | COMMA
  | SEMI
  | DOT
  | DOTDOT
  | ASSIGN
  | EQUAL
  | DISEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | XOR
  | IMPLIES
  | IFF
  | NOT
  | IF
  | SETIF
  | BVIF
  | THEN
  | ELSE
  | END
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | FLOOR
  | LESS
  | GREATER
  | LESSOREQUAL
  | GREATEROREQUAL
  | CONV
  | IN
  | NOTIN
  | EMPTY
  | FULL
  | COMPL
  | UNION
  | INTER
  | DIFF
  | SYMDIFF
  | SUB
  | SETEQ
  | PROJ
  | INTEGER_PRED
  | UNSIGNED
  | FORALL
  | EXISTS

val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val term :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ics.term
val equation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ics.term * Ics.term
