open Base

exception ParseError of string

val parse : Lexer.token list -> Exp.t
val parse_file : string -> Exp.t
