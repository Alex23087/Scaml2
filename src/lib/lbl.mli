exception SecurityException

type conf = Secret | Public [@@deriving sexp, equal]
type intg = Tainted | Untainted [@@deriving sexp, equal]

type t = conf * intg [@@deriving sexp, equal]

val conf_leq : conf -> conf -> bool
val intg_leq : intg -> intg -> bool

val conf_join  : conf -> conf -> conf
val conf_merge : conf -> conf -> conf
val intg_join  : intg -> intg -> intg
val intg_merge : intg -> intg -> intg

val top : t
val bot : t

val leq : t -> t -> bool
val (<=) : t -> t -> bool

val join  : t -> t -> t
val merge : t -> t -> t

val joins : t list -> t

val conf_proj : t -> conf
val intg_proj : t -> intg
