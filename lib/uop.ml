type t = Not | Minus

let to_string = function
  | Not -> "!"
  | Minus -> "-"
