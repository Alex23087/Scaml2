type conf = Secret | Public
type intg = Tainted | Untainted

type t = conf * intg

val conf_leq : conf -> conf -> bool
val intg_leq : intg -> intg -> bool

val conf_join : conf -> conf -> conf
val intg_join : intg -> intg -> intg

val top : t
val bot : t

val leq : t -> t -> bool
val (<=) : t -> t -> bool

val join : t -> t -> t
