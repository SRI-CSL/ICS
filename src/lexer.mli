
(* A lexer for terms and equations. *)

(*i*)
open Ics
(*i*)

val token : Lexing.lexbuf -> Parser.token

(*s We provide functions to parse terms and equations from strings and 
    channels. *)

val term_of_string : string -> term
val term_of_channel : in_channel -> term

val eqn_of_string : string -> term * term
val eqn_of_channel : in_channel -> term * term
