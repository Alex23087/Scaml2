exception ParseError of string

val parse : Lexer.token list -> Exp.t
