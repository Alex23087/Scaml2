open Base

type t = Any | Int | String | Bool | Fun of t * t | Tuple of t list [@@deriving sexp]
