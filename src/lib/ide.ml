open Base

type t = string [@@deriving sexp]

let to_string x = x
let of_string x = x

let (=) = String.(=)
let equal = (=)

let next_id = Util.make_counter 0
let fresh prefix = "@_" ^ prefix ^ Int.to_string (next_id ())

let fresh_list prefix n =
  List.init n ~f:(fun _ -> fresh prefix)
