open Base

type t = string [@@deriving sexp]

let to_string x = x
let of_string x = x

let (=) = String.(=)
let equal = (=)

let fresh =
  let last = ref (-1) in
  fun prefix ->
    last := !last + 1;
    "@_" ^ prefix ^ Int.to_string !last

let fresh_list prefix n =
  List.init n ~f:(fun _ -> fresh prefix)
