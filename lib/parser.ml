open Base

open Lexer
open Expr

(* GRAMMAR

   <e{i}> ::= <e{i+1}>
            | <uop> <e{i}>
            | <e{i+1}> <bopr{i}> <e{i}>
            | <e{i+1}> (<bopl{i}> <e{i+1}>)+

   <bopl0> ::= " "

   <uop1> ::= "-"

   <bopl2> ::= "*" | "/"

   <bopl3> ::= "+" | "-"

   <bopr4> ::= "::"




   <e> ::= let <id> = <e>; <e>
 *)


let ( let* ) x f = Option.bind ~f x
let ( let+ ) x f = Option.map ~f x
let ( let< ) x f = Option.value_or_thunk ~default:f x

type 'repr expr_cons_1 = (('repr expr -> 'repr) -> 'repr expr -> 'repr)
type 'repr expr_cons_2 = (('repr expr -> 'repr) -> ('repr expr -> 'repr) -> 'repr expr -> 'repr)
type 'repr expr_really = 'repr expr -> 'repr

exception ParseError

let ops : ((token * 'repr expr_cons_1) list
           * (token * 'repr expr_cons_2) list
           * (token * 'repr expr_cons_2) list) list
  = [ (* funapp *)
    ([(TMinus, neg)],
     [],
     []);

    ([],
     [(TStar, mul);
      (TSlash, div)],
     []);

    ([],
     [(TPlus, add);
      (TMinus, sub)],
     []);

    ([],
     [],
     [TCons, cons])]

let expect t toks =
  match toks with
  | t' :: toks when equal_token t t' -> toks
  | _ -> raise ParseError

let try_parse_delim parse ?(cons = Fn.id) tok_start tok_end toks =
  match toks with
  | t :: toks when equal_token t tok_start ->
     let e = parse toks in
     (match toks with
      | t :: toks when equal_token t tok_end -> Some (cons e, toks)
      | _ -> raise ParseError)
  | _ -> None

let rec parse_ops toks =
  let rec aux ops toks =
    let parse_uop (t, (m : 'repr expr_cons_1)) =
      match toks with
      | t' :: toks when equal_token t t' ->
         let (e, toks) = aux ops toks in
         Some (m e, toks)
      | _ -> None
    in

    let parse_bopr e1 toks (t, m) =
      match toks with
      | t' :: toks when equal_token t t' ->
         let (e2, toks) = aux ops toks in
         Some ((m e1 e2), toks)
      | _ -> None
    in

    let rec parse_bopls ops' e1 toks bopls =
      List.find_map bopls ~f:(fun (t, m) ->
            match toks with
            | t' :: toks when equal_token t t' ->
               let (e2, toks) = aux ops' toks in
               parse_bopls ops (m e1 e2) toks bopls
            | _ -> None)
    in

    match ops with
    | (uops, bopls, boprs) :: ops' ->
       let< () = List.find_map uops ~f:parse_uop in
       let (e1, toks) = aux ops' toks in
       let< () = List.find_map boprs ~f:(parse_bopr e1 toks) in
       let< () = parse_bopls ops' e1 toks bopls in
       raise ParseError

    | [] -> parse_app_or_atom toks

  in aux ops toks

and parse_app_or_atom toks =
  let (e1, toks) = Option.value_or_thunk (try_parse_atom toks)
               ~default:(fun () -> raise ParseError)
  in

  match try_parse_atom toks with
  | Some (e2, toks') -> (app e1 e2, toks')
  | None -> (e1, toks)

and try_parse_atom toks =
  let< () = Some (try_parse_delim parse_expr TParOpen TParClosed toks) in
  match toks with
  | TInt x :: toks -> Some (int x, toks)
  | TString x :: toks -> Some (str x, toks)
  | TIde x :: toks -> Some (ide x, toks)
  | _ -> None

and parse_expr = function
  (* let, match ecc *)
  | TLet :: toks ->
     (match toks with
      | TIde x :: toks ->
         let toks = expect TEquals toks in
         let (e, toks) = parse_expr toks in
         let toks = expect TSemicolon toks in
         let (b, toks) = parse_expr toks in
         (letin x e b, toks)
      | _ -> raise ParseError)

  | toks -> parse_ops toks

