type t =
  | Addition | Subtraction | Multiplication | Division
  | Equals | Less | More | LessEq | MoreEq
  | And | Or
  | Seq
[@@deriving sexp]

(*
let to_string = function
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
*)
