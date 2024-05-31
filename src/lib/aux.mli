val mod_let_desugaring : Exp.t Decl.t Base.list -> Exp.t

val restrict_to_intfs : 'a Val.t Env.t -> Intf.t list -> 'a Val.t Env.t

val set_plugin : 'a Val.t Env.t -> int -> 'a Val.t Env.t
val set_trusted : 'a Val.t Env.t -> bool -> 'a Val.t Env.t
val set_path : 'a Val.t Env.t -> string -> 'a Val.t Env.t

val get_plugin_exn : 'a Val.t Env.t -> int
val get_trusted_exn : 'a Val.t Env.t -> bool
val get_path_exn : 'a Val.t Env.t -> string

