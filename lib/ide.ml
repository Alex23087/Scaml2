open Base

type t = string

let to_string x = x
let of_string x = x

let (=) = String.(=)
let equal = (=)
