open Base

type 'a t

val bind : 'a t -> Ide.t -> 'a -> 'a t
val lookup : 'a t -> Ide.t -> 'a option
