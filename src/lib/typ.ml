open Base

type t = Any | Int | String | Bool | Fun of t * t | Tuple of t list [@@deriving sexp]

let to_string tau =
  tau |> sexp_of_t |> Sexp.to_string_hum