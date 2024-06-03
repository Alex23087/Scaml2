open Base
open Stdio

open Let_attr

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
           | THasAttr | TDeclassify | TEndorse | TDeclassifyPC | TEndorsePC
           | TDie | TAssert

           (* types *)
           | TTint | TTstring | TTbool | TArrow | TAny
[@@deriving equal, sexp]

type lexer_error = string * int * int

type action = Tok of token
            | Fun of (string -> token)
            | Ignore

let unescape = String.Escaping.unescape_gen_exn
                 ~escape_char:'\\'
                 ~escapeworthy_map:[('\n', 'n'); ('\t', 't')]
               |> Staged.unstage

let tok_re = (List.map ~f:(fun (s, a) -> (Str.regexp_string s, a))
                [("->", Tok TArrow);

                 ("(", Tok TParOpen  );  (")", Tok TParClosed);
                 ("[", Tok TSquareOpen); ("]", Tok TSquareClosed);
                 ("{", Tok TCurlyOpen);  ("}", Tok TCurlyClosed);

                 (",", Tok TComma);
                 (";", Tok TSemicolon);
                 (".", Tok TDot);
                 (":", Tok TColon);
                 ("\\", Tok TBackslash);

                 ("=", Tok TEquals);
                 ("+", Tok TPlus);
                 ("-", Tok TMinus);
                 ("*", Tok TStar);
                 ("/", Tok TSlash);
                 ("<=", Tok TLessEq);
                 (">=", Tok TMoreEq);
                 ("<", Tok TLess);
                 (">", Tok TMore);
                 ("&&", Tok TLogAnd);
                 ("||", Tok TOr);
                 ("!", Tok TNot);
                 ("fix*", Tok TFixs)])
             @ (List.map ~f:(fun (s, a) -> (Str.regexp s, a))
                  [(" +", Ignore);

                   ("#.*", Ignore);

                   ("[0-9]+",
                    Fun (fun s -> TInt (Int.of_string s)));

                   ({|"\(\\.\|[^"]\)*"|},
                    Fun (fun s ->
                        let s = String.(drop_suffix (drop_prefix s 1) 1)
                                |> unescape
                        in TString s));

                   ("[a-zA-Z_][a-zA-Z0-9_]*'*",
                    Fun (function
                        | "true" -> TBool true
                        | "false" -> TBool false

                        | "int" -> TTint
                        | "string" -> TTstring
                        | "bool" -> TTbool
                        | "any" -> TAny

                        | "fix" -> TFix

                        | "let" -> TLet
                        | "rec" -> TRec
                        | "and" -> TAnd
                        | "in" -> TIn

                        | "if" -> TIf
                        | "then" -> TThen
                        | "else" -> TElse
                        | "end" -> TEnd

                        | "with" -> TWith
                        | "handle" -> THandle
                        | "do" -> TDo

                        | "module" -> TModule
                        | "trusted" -> TTrusted
                        | "plugin" -> TPlugin
                        | "export" -> TExport

                        | "has_attr" -> THasAttr
                        | "declassify_pc" -> TDeclassifyPC
                        | "endorse_pc" -> TEndorsePC
                        | "declassify" -> TDeclassify
                        | "endorse" -> TEndorse

                        | "die" -> TDie
                        | "assert" -> TAssert

                        | "public" -> TLetAttr Public
                        | "secret" -> TLetAttr Secret
                        | "tainted" -> TLetAttr Tainted
                        | "untainted" -> TLetAttr Untainted

                        | s -> TIde (Ide.of_string s)))])

let rec tokenize_line l lnum i toks =
  let try_re (re, act) =
    if Str.string_match re l i then
      Some ((match act with
             | Tok t -> Some t
             | Fun f -> Some (f (Str.matched_string l))
             | Ignore -> None),
            Str.match_end ())
    else None
  in

  match List.find_map tok_re ~f:try_re with
  | Some (Some t, i') -> tokenize_line l lnum i' (t :: toks)
  | Some (None, i') -> tokenize_line l lnum i' toks
  | None -> if i = String.length l
            then Ok toks
            else Error ("unexpected character", lnum, i)

let tokenize ch =
  let rec aux lnum toks =
    match In_channel.input_line ch with
    | Some l -> Result.bind (tokenize_line l lnum 0 toks) ~f:(aux (lnum + 1))
    | None -> Ok (List.rev toks)
  in
  aux 1 []

let error_to_string (msg, l, c) =
  Printf.sprintf "Lexer error at line %d, column %d: %s" l c msg

let token_to_string tok = sexp_of_token tok |> Sexp.to_string_hum

let toks_to_string toks =
  List.map toks ~f:token_to_string
  |> String.concat ~sep:" "
