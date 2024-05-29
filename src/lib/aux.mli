val eq_attr_lbl: Let_attr.t -> Lbl.t -> bool

val mod_let_desugaring : Exp.t Decl.t Base.list -> Exp.t

val restrict_to_intfs : 'a Val.t Env.t -> Intf.t list -> 'a Val.t Env.t
