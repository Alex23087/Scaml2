exception InvalidBop of string
exception InvalidUop of string

val eval : string -> Exp.t -> Exp.t Val.t
