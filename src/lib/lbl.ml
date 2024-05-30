open Base

exception SecurityException

type conf = Secret of int | Public [@@deriving sexp, equal]
type intg = Tainted | Untainted [@@deriving sexp, equal]

type t = conf * intg [@@deriving sexp, equal]

let conf_leq a b = match (a, b) with
  | Secret _, Public -> false
  | Secret p, Secret q -> q = 0 || p = q
  | _ -> true

let intg_leq a b = match (a, b) with
  | Tainted, Untainted -> false
  | _ -> true

let conf_join a b = match (a, b) with
  | Public, Public -> Public
  | Public, Secret p | Secret p, Public -> Secret p
  | Secret p, Secret q -> Secret (if p = q then p else 0)

let conf_meet a b = match (a, b) with
  | Secret p, Secret q ->
     (match p, q with
      | 0, x | x, 0 -> Secret x
      | _ when p = q -> Secret p
      | _ -> Public)
  | _ -> Public

let intg_join a b = match (a, b) with
| Untainted, Untainted -> Untainted
| _ -> Tainted

let intg_meet a b = match (a, b) with
  | Tainted, Tainted -> Tainted
  | _ -> Untainted

let top = (Secret 0, Tainted)
let bot = (Public, Untainted)

let leq (c1, i1) (c2, i2) = conf_leq c1 c2 && intg_leq i1 i2
let (<=) = leq

let join (c1, i1) (c2, i2) = (conf_join c1 c2, intg_join i1 i2)
let meet (c1, i1) (c2, i2) = (conf_meet c1 c2, intg_meet i1 i2)

let joins = List.fold_left ~f:join ~init:bot

let conf_proj (c, _) = c
let intg_proj (_, i) = i
