(* type handler = Handler *)

(* type ('a, 'eff) expr =
  | IntLiteral : int -> (int, 'eff) expr
  | Sum : ((int, 'eff) expr * (int, 'eff) expr) -> (int, 'eff) expr
  | Handler : ('a, 'eff) expr -> (handler, 'eff) expr
  | WithHandle : ((handler, 'eff) expr * ('a, 'eff) expr) -> ('a, 'eff) expr *)

type ident = string [@@deriving show]


type abinop =
  | Plus
  | Minus
  | Times
  | Div

type aunop =
  | Neg

type bbinop =
  | And
  | Or

type bunop =
  | Not

type comparison =
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq

type expr =
  | Let of ident * expr * expr
  | IntLiteral of int
  | BoolLiteral of bool
  | Var of ident
  | ABinop of abinop * expr * expr
  | AUnop of aunop * expr
  | BBinop of bbinop * expr * expr
  | BUnop of bunop * expr
  | Comparison of comparison * expr * expr
  | IfThenElse of expr * expr * expr
  | Func of ident list * expr
  | Apply of expr * expr list
  | Handler of (ident * expr) list
  | WithHandle of expr * expr
  | CallOp of ident * expr list
  | Fix of expr
  | Fixes of expr list
  | Print of expr