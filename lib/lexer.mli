open Base

type token = TInt of int
           | TString of string
           | TIde of Ide.t
           | TParOpen | TParClosed | TSemicolon
           | TLet | TEquals
           | TPlus | TMinus | TStar | TSlash
           | TLess | TMore | TLessEq | TMoreEq
           | TBackslash | TDot
           | TCons

type lexer_error = string * int * int

val equal_token : token -> token -> bool

val tokenize : Stdio.In_channel.t -> (token list, lexer_error) Result.t
