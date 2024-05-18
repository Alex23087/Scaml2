open Base
open Stdio

type token = TInt of int
           | TString of string
           | TIde of Ide.t
           | TParOpen | TParClosed | TSemicolon
           | TLet | TEquals
           | TPlus | TMinus | TStar | TSlash
           | TLess | TMore | TLessEq | TMoreEq
           | TBackslash | TDot
           | TCons
[@@deriving equal]

type lexer_error = string * int * int

type action = Tok of token
            | Fun of (string -> token)
            | Ignore

let tok_re = (List.map ~f:(fun (s, a) -> (Str.regexp_string s, a))
                [("("   , Tok TParOpen  );
                 (")"   , Tok TParClosed);
                 (";"   , Tok TSemicolon);
                 ("let" , Tok TLet      );
                 ("="   , Tok TEquals   );
                 ("+"   , Tok TPlus     );
                 ("-"   , Tok TMinus    );
                 ("*"   , Tok TStar     );
                 ("/"   , Tok TSlash    );
                 ("<="  , Tok TLessEq   );
                 (">="  , Tok TMoreEq   );
                 ("<"   , Tok TLess     );
                 (">"   , Tok TMore     );
                 ("\\"  , Tok TBackslash);
                 ("."   , Tok TDot      );
                 ("::"  , Tok TCons     )])
             @ (List.map ~f:(fun (s, a) -> (Str.regexp s, a))
                  [(" +", Ignore);

                   ("[0-9]+",
                    Fun (fun s -> TInt (Int.of_string s)));

                   ({|"\(\\.|[^\"]\)*"|},
                    Fun (fun s -> TString (String.(drop_suffix (drop_prefix s 1) 1))));

                   ("[a-zA-Z_][a-zA-Z0-9_]*",
                    Fun (fun s -> TIde (Ide.of_string s)))])

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
