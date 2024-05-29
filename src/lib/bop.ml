type t =
  | Addition
  | Subtraction
  | Multiplication
  | Division

  | Equals
  | Less
  | More
  | LessEq
  | MoreEq

  | And
  | Or
  | Seq
[@@deriving sexp]

(*
type (_,_,_) t =
  | Addition : (int, int, int) t
  | Subtraction : (int, int, int) t
  | Multiplication : (int, int, int) t
  | Division : (int, int, int) t

  | Equals : (int, int, bool) t
  | Less : (int, int, bool) t
  | More : (int, int, bool) t
  | LessEq : (int, int, bool) t
  | MoreEq : (int, int, bool) t

  | And : (bool, bool, bool) t
  | Or : (bool, bool, bool) t
  | Seq : (bool, bool, bool) t
[@@deriving sexp]
*)

(*
let to_string = function
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
*)
