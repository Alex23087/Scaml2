open Base

type t = Public | Secret | Tainted | Untainted [@@deriving equal, sexp]

let to_lbl (attr : t) ~(plugin : int) ~(default : Lbl.t): Lbl.t =
  match attr with
  | Public    -> (Lbl.Public, Lbl.intg_proj default)
  | Secret    -> (Lbl.Secret plugin, Lbl.intg_proj default)
  | Tainted   -> (Lbl.conf_proj default, Lbl.Tainted)
  | Untainted -> (Lbl.conf_proj default, Lbl.Untainted)

let list_to_lbl (attrs : t list) ~(plugin : int) ~(meet : bool) =
  let default = if meet then Lbl.top else Lbl.bot in
  let attrs = List.map ~f:(to_lbl ~plugin ~default) attrs in
  let op = if meet then Lbl.meet else Lbl.join in
  List.fold_left ~f:op ~init:default attrs

let matches_lbl (attr : t) (lbl : Lbl.t) =
  match attr, lbl with
  | Public, (Lbl.Public, _) | Secret, (Lbl.Secret _, _)
  | Tainted, (_, Lbl.Tainted) | Untainted, (_, Lbl.Untainted)
    -> true

  | _ -> false
