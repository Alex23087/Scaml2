type t = Addition | Subtraction | Multiplication | Division

let to_string = function
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"

