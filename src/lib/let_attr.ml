open Base

type t = Public | Secret | Tainted | Untainted [@@deriving equal, sexp]

let to_lbl (attr : t) ~(default : Lbl.t): Lbl.t =
  match attr with
  | Public    -> (Lbl.Public, Lbl.intg_proj default)
  | Secret    -> (Lbl.Secret, Lbl.intg_proj default)
  | Tainted   -> (Lbl.conf_proj default, Lbl.Tainted)
  | Untainted -> (Lbl.conf_proj default, Lbl.Untainted)

let list_to_lbl (attrs: t list) ~(default: Lbl.t) =
  let attrs = List.map ~f:(to_lbl ~default) attrs in
  let op = Lbl.(match default with
                | l when equal l top -> merge
                | l when equal l bot -> join
                | _ -> raise (Invalid_argument "list_to_lbl called with a value that is neither top nor bottom"))
  in
  List.fold_left ~f:op ~init:default attrs

