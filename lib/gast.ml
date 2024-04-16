type ('pippo, 'bonchi) expr =
  | IntLiteral : int -> (int, unit) expr
  | Sum : ((int, 'bonchi) expr * (int, 'bonchi) expr) -> (int, 'bonchi) expr