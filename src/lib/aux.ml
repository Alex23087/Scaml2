let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let attr_to_lbl (attr: Let_attr.t) ~(default: Lbl.t): Lbl.t =
  match attr with
  | Public    -> (Lbl.Public, Lbl.intg_proj default)
  | Secret    -> (Lbl.Secret, Lbl.intg_proj default)
  | Tainted   -> (Lbl.conf_proj default, Lbl.Tainted)
  | Untainted -> (Lbl.conf_proj default, Lbl.Untainted)


let attr_list_to_lbl (attrs: Let_attr.t Base.list) ~(default: Lbl.t) =
  let attrs = List.map (attr_to_lbl ~default) attrs in
  let op = (match default with
    | l when l = Lbl.top -> Lbl.merge
    | l when l = Lbl.bot -> Lbl.join
    | _ -> raise (Invalid_argument "attr_list_to_lbl called with a value that is neither top nor bottom")) in
  List.fold_left op default attrs


let rec mod_let_desugaring (decls: Exp.t Decl.t Base.list): Exp.t =
  match decls with
    | [] -> Exp.Lam (Ide.of_string "x", Exp.Var (Ide.of_string "x")) (* TODO: Fresh ide probably not necessary, since we discard it and return the closure environment. Check *)
    | e::ds -> (
      match e with
        | Decl.Let (attrs, ide, exp) -> (
          Exp.Let (attrs, ide, exp, mod_let_desugaring ds)
        )
        (* TODO: | Decl.LetRec *)
        | _ -> raise (Invalid_argument "mod_let_desugaring can only accept a list of let declarations")
    )


let rec apply_intf (v, ell: 'a Val.t) (tau: Typ.t): 'a Val.t =
  let ell' = Lbl.join ell (Lbl.Public, Lbl.Tainted) in
  match (v, tau) with
    | (_, Typ.Any)
    | (Val.Int _, Typ.Int)
    | (Val.String _, Typ.String)
    | (Val.Bool _, Typ.Bool) -> (
      (v, ell')
    )
    | (Val.Fun _, Typ.Fun _) -> (Val.ValType (v, tau), ell')
    | (Val.Tuple vts, Typ.Tuple tts) -> (
      let mapped = List.combine vts tts |> List.map (uncurry apply_intf) in
      (Val.Tuple(mapped), ell')
    )
    (* | (Val.NoSecret v', _) -> (Val.NoSecret (apply_intf (v', ell) tau)) (\* TODO: NoSecret takes a bare value, this needs to be fixed *\) *)
    | _ -> raise (Invalid_argument "Cannot apply interface to value")
