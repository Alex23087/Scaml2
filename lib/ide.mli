type t

val to_string : t -> string
val of_string : string -> t

val (=) : t -> t -> bool
val equal : t -> t -> bool
