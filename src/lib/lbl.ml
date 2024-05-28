exception SecurityException

type conf = Secret | Public [@@deriving sexp]
type intg = Tainted | Untainted [@@deriving sexp]

type t = conf * intg [@@deriving sexp]

let conf_leq a b = match (a, b) with
  | Secret, Public -> false
  | _ -> true

let intg_leq a b = match (a, b) with
  | Tainted, Untainted -> false
  | _ -> true

let conf_join a b = match (a, b) with
  | Public, Public -> Public
  | _ -> Secret

let conf_merge a b = match (a, b) with
  | Secret, Secret -> Secret
  | _ -> Public

let intg_join a b = match (a, b) with
| Untainted, Untainted -> Untainted
| _ -> Tainted

let intg_merge a b = match (a, b) with
  | Tainted, Tainted -> Tainted
  | _ -> Untainted

let top = (Secret, Tainted)
let bot = (Public, Untainted)

let leq (c1, i1) (c2, i2) = conf_leq c1 c2 && intg_leq i1 i2
let (<=) = leq

let join (c1, i1) (c2, i2) = (conf_join c1 c2, intg_join i1 i2)
let merge (c1, i1) (c2, i2) = (conf_merge c1 c2, intg_merge i1 i2)

let conf_proj (c, _) = c
let intg_proj (_, i) = i