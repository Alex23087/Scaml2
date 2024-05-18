open Base

type 'a t [@@deriving sexp]

val bind : 'a t -> Ide.t -> 'a -> 'a t
val lookup : 'a t -> Ide.t -> 'a option
