open Base

open Lexer

(* Chain of calls (low to high precedence):
   - parse
   - parse_expr (let, with-handle, lambda, ...)
   - parse_ops (various precedence levels)
   - parse_app_like (application, fix, print, ...)
   - parse_field_access (m.x, t[i])
   - parse_atom (actual atoms and delimited terms: parentheses, modules, plugins)

   The essence of encoding precedence and associativity for operators:

       <e{i}> ::= <e{i+1}>
       | <uop> <e{i}>
       | <e{i+1}> <bopr{i}> <e{i}>
       | <e{i+1}> (<bopl{i}> <e{i+1}>)+
 *)


(* let ( let* ) x f = Option.bind ~f x *)
(* let ( let+ ) x f = Option.map ~f x *)
let ( let< ) x f = Option.value_or_thunk ~default:f x

let tts = token_to_string

exception ParseError of string

let ops =
  Bop.(Uop.([([(TMinus, Minus);
               (TNot, Not)],
              [],
              []);

             ([],
              [(TStar, Multiplication);
               (TSlash, Division)],
              []);

             ([],
              [(TPlus, Addition);
               (TMinus, Subtraction)],
              []);

             ([],
              [(TEquals, Equals);
               (TLess, Less);
               (TMore, More);
               (TLessEq, LessEq);
               (TMoreEq, MoreEq)],
              []);

             ([],
              [],
              [(TLogAnd, And)]);

             ([],
              [],
              [(TOr, Or)]);

             ([],
              [],
              [(TSemicolon, Seq)]);
  ]))

let expect t toks =
  match toks with
  | t' :: toks ->
     if equal_token t t' then toks
     else raise (ParseError ("expected " ^ tts t ^ ", found " ^ tts t'))
  | _ -> raise (ParseError ("expected " ^ tts t ^ ", found EOF"))

let parse_attrs toks =
  let (attrs, toks) = List.split_while toks ~f:(function
                    | TLetAttr _ -> true
                    | _ -> false)
  in
  let attrs = List.map attrs ~f:(function
            | TLetAttr a -> a
            | _ -> failwith "unreachable")
  in
  (attrs, toks)

let split_ides toks =
  let xs, toks = List.split_while toks ~f:(function TIde _ -> true | _ -> false) in
  let xs = List.map xs ~f:(function TIde x -> x | _ -> failwith "unreachable") in
  (xs, toks)

let rec make_lam args body =
  match args with
  | [] -> body
  | x :: args -> Exp.Lam (x, make_lam args body)

let rec parse_ops toks =
  let rec aux ops toks =
    let parse_uop (t, op) =
      match toks with
      | t' :: toks when equal_token t t' ->
         let (e, toks) = aux ops toks in
         Some (Exp.Uop (op, e), toks)
      | _ -> None
    in

    let parse_bopr e1 toks (t, op) =
      match toks with
      | t' :: toks when equal_token t t' ->
         let (e2, toks) = aux ops toks in
         Some (Exp.Bop (op, e1, e2), toks)
      | _ -> None
    in

    let rec parse_bopls ops' e1 toks bopls =
      List.find_map bopls ~f:(fun (t, op) ->
            match toks with
            | t' :: toks when equal_token t t' ->
               let (e2, toks) = aux ops' toks in
               parse_bopls ops (Exp.Bop (op, e1, e2)) toks bopls
            | _ -> None)
    in

    match ops with
    | (uops, bopls, boprs) :: ops' ->
       let< () = List.find_map uops ~f:parse_uop in
       let (e1, toks) = aux ops' toks in
       let< () = List.find_map boprs ~f:(parse_bopr e1 toks) in
       let< () = parse_bopls ops' e1 toks bopls in
       raise (ParseError "TODO")

    | [] -> parse_app_like toks

  in aux ops toks

and parse_app_like = function
  | TPrint :: toks -> let e, toks = parse_field_access toks in (Exp.Print e, toks)
  | TFix :: toks -> let e, toks = parse_field_access toks in (Exp.Fix e, toks)
  | TFixs :: toks -> let e, toks = parse_field_access toks in (Exp.Fixs e, toks)
  | TAssert :: toks -> let e, toks = parse_field_access toks in (Exp.Assert e, toks)
  | THasAttr :: TLetAttr a :: toks -> (* TODO precedenza maggiore per fare assert has_attr senza parentesi? *)
     let e, toks = parse_field_access toks in (Exp.HasAttr (a, e), toks)

  | toks ->
     let e1, toks = parse_field_access toks in
     try
       let e2, toks = parse_field_access toks in
       (Exp.App (e1, e2), toks)
     with ParseError _ -> (e1, toks)

and parse_field_access toks =
  let e1, toks = parse_atom toks in
  match toks with
  | TDot :: TIde x :: toks -> (Exp.Field (e1, x), toks)
  | TSquareOpen :: toks ->
     let e2, toks = parse_expr toks in
     let toks = expect TSquareClosed toks in
     (TupleField (e1, e2), toks)
  | _ -> (e1, toks)

and parse_atom = function
  | TParOpen :: toks ->
     let e, toks = parse_expr toks in
     let toks = expect TParClosed toks in
     (e, toks)

  (* | TSquareOpen -> TODO parse tupla *)

  | TInt x :: toks -> (Exp.Lit (Val.int x), toks)
  | TString x :: toks -> (Exp.Lit (Val.string x), toks)
  | TBool x :: toks -> (Exp.Lit (Val.bool x), toks)
  | TIde x :: toks -> (Exp.Var x, toks)

  | t :: _ -> raise (ParseError ("expected atom, found " ^ tts t))
  | _ -> raise (ParseError "expected atom, found EOS")

and parse_binding toks =
  let attrs, toks = parse_attrs toks in
  match toks with
  | TIde x :: toks ->
     let args, toks = split_ides toks in
     let toks = expect TEquals toks in
     let e, toks = parse_expr toks in
     ((attrs, x, make_lam args e), toks)
  | t :: _ -> raise (ParseError ("expected identifier, found " ^ tts t))
  | _ -> raise (ParseError "expected identifier, found EOF")

and parse_let_and toks =
  let rec aux toks res =
    let b, toks = parse_binding toks in
    let res = b :: res in
    match toks with
    | TIn :: toks -> List.rev res, toks
    | TAnd :: toks -> aux toks res
    | t :: _ -> raise (ParseError ("expected 'in' or 'and', found " ^ tts t))
    | _ -> raise (ParseError "expected 'in' or 'and', found EOF")
  in
  aux toks []

and parse_handler toks =
  let rec aux ops ret toks =
    match split_ides toks with
    | ([ret; x], toks) when String.equal "return" (Ide.to_string ret) ->
       let toks = expect TEquals toks in
       let e, toks = parse_expr toks in
       aux ops (Some (x, e)) toks

    | (op :: args, toks) ->
       let toks = expect TEquals toks in
       let e, toks = parse_expr toks in
       aux ((op, args, e) :: ops) ret toks

    | ([], TCurlyClosed :: toks) ->
       let default = let x = Ide.of_string "x" in (x, Exp.Var x) in
       let ret = Option.value ret ~default in
       (Handler.{ops; ret}, toks)

    | ([], t :: _) -> raise (ParseError ("expected '}', found " ^ tts t))
    | ([], []) -> raise (ParseError "expected '}', found EOF")
  in

  let toks = expect TCurlyOpen toks in
  aux [] None toks

and parse_expr = function
  | TLet :: TRec :: toks ->
     let bindings, toks = parse_let_and toks in
     let body, toks = parse_expr toks in
     (Exp.LetRec (bindings, body), toks)

  | TLet :: toks ->
     let (attrs, x, e), toks = parse_binding toks in
     let toks = expect TIn toks in
     let b, toks = parse_expr toks in
     (Exp.Let (attrs, x, e, b), toks)

  | TBackslash :: TIde x :: toks ->
     let toks = expect TDot toks in
     let e, toks = parse_expr toks in
     (Exp.Lam (x, e), toks)
  | TBackslash :: _ -> raise (ParseError "expected identifier after \\")

  | TWith :: toks ->
     let h, toks = parse_handler toks in
     let toks = expect THandle toks in
     let e, toks = parse_expr toks in
     (Exp.Handle (h, e), toks)

  | toks -> parse_ops toks

let parse toks =
  let e, toks = parse_expr toks in
  match toks with
  | [] -> e
  | t :: _ -> raise (ParseError ("expected EOF, found " ^ tts t))
