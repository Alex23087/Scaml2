open Base

type t [@@deriving sexp]

val to_string : t -> string
val of_string : string -> t

val (=) : t -> t -> bool
val equal : t -> t -> bool

val fresh : string -> t
val fresh_list : string -> int -> t list
