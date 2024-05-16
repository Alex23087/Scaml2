open Base

type t = string [@@deriving sexp]

let to_string x = x
let of_string x = x

let (=) = String.(=)
let equal = (=)
