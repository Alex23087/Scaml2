type 'a t [@@deriving sexp]

val foobar : int t
val bazbax : 'a t -> (Ide.t * Ide.t list * 'a) list * (Ide.t * 'a)
