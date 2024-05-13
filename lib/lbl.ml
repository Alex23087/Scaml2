type conf = Secret | Public
type intg = Tainted | Untainted

type t = conf * intg

let conf_leq a b = match (a, b) with
  | Secret, Public -> false
  | _ -> true

let intg_leq a b = match (a, b) with
  | Tainted, Untainted -> false
  | _ -> true

let conf_join a b = match (a, b) with
  | Public, Public -> Public
  | _ -> Secret

let intg_join a b = match (a, b) with
  | Untainted, Untainted -> Untainted
  | _ -> Tainted

let top = (Secret, Tainted)
let bot = (Public, Untainted)

let leq (c1, i1) (c2, i2) = conf_leq c1 c2 && intg_leq i1 i2
let (<=) = leq

let join (c1, i1) (c2, i2) = (conf_join c1 c2, intg_join i1 i2)
