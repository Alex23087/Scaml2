open Base

type 'a t [@@deriving sexp]

val bind : 'a t -> Ide.t -> 'a -> 'a t
val lookup : 'a t -> Ide.t -> 'a option
val lookup_exn : 'a t -> Ide.t -> 'a
val empty_env : 'a t