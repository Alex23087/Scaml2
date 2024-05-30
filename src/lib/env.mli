open Base

type 'a t [@@deriving sexp]

val of_alist : (Ide.t * 'a) list -> 'a t
val to_alist : 'a t -> (Ide.t * 'a) list
val bind : 'a t -> Ide.t -> 'a -> 'a t
val bind_all : 'a t -> (Ide.t * 'a) list -> 'a t
val lookup : 'a t -> Ide.t -> 'a option
val lookup_exn : 'a t -> Ide.t -> 'a
val empty : 'a t
val restrict : 'b Decl.t list -> 'a t -> 'a t
