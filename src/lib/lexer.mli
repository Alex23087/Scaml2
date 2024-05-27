open Base

type token = TIde of Ide.t
           (* literals *)
           | TInt of int
           | TString of string
           | TBool of bool

           (* delimiters *)
           | TParOpen | TParClosed
           | TSquareOpen | TSquareClosed
           | TCurlyOpen | TCurlyClosed
           | TComma | TSemicolon | TDot | TColon
           | TBackslash

           (* operators *)
           | TEquals
           | TPlus | TMinus | TStar | TSlash
           | TLess | TMore | TLessEq | TMoreEq
           | TLogAnd | TOr | TNot
           | TFix | TFixs

           (* keywords *)
           | TLet | TRec | TAnd | TIn | TLetAttr of Let_attr.t
           | TIf | TThen | TElse | TEnd
           | TWith | THandle | TDo
           | TModule | TTrusted | TPlugin | TExport
           | TAssert | THasAttr | TDeclassify | TEndorse
           | TPrint | TDie

           (* types *)
           | TTint | TTstring | TTbool | TTarrow | TAny
[@@deriving equal, sexp]

type lexer_error = string * int * int

val equal_token : token -> token -> bool
val token_to_string : token -> string
val toks_to_string : token list -> string

val tokenize : Stdio.In_channel.t -> (token list, lexer_error) Result.t

val error_to_string : lexer_error -> string

