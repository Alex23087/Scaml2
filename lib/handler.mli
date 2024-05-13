type 'a t

val foobar : int t
val bazbax : 'a t -> (Ide.t * Ide.t list * 'a) list * (Ide.t * 'a)
