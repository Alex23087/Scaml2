exception SecurityException

type conf = Secret of int | Public [@@deriving sexp, equal]
type intg = Tainted | Untainted [@@deriving sexp, equal]

type t = conf * intg [@@deriving sexp, equal]

val conf_leq : conf -> conf -> bool
val intg_leq : intg -> intg -> bool

val conf_join  : conf -> conf -> conf
val conf_meet : conf -> conf -> conf
val intg_join  : intg -> intg -> intg
val intg_meet : intg -> intg -> intg

val top : t
val bot : t

val leq : t -> t -> bool
val (<=) : t -> t -> bool

val join  : t -> t -> t
val meet : t -> t -> t

val joins : t list -> t

val conf_proj : t -> conf
val intg_proj : t -> intg
