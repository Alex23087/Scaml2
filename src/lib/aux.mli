val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val attr_to_lbl : Let_attr.t -> default:Lbl.t -> Lbl.t
val attr_list_to_lbl : Let_attr.t list -> default:Lbl.t -> Lbl.t

val eq_attr_lbl: Let_attr.t -> Lbl.t -> bool

val mod_let_desugaring : Exp.t Decl.t Base.list -> Exp.t
val apply_intf : 'a Val.t -> Typ.t -> 'a Val.t
