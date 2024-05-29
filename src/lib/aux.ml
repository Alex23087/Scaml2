open Base

let eq_attr_lbl (attr: Let_attr.t) (lbl: Lbl.t): bool =
  let (conf, intg) = Let_attr.to_lbl attr ~default:lbl in
  let (conf', intg') = lbl in
  Lbl.(equal_conf conf conf' && equal_intg intg intg')


let rec mod_let_desugaring (decls: Exp.t Decl.t Base.list): Exp.t =
  match decls with
    | [] -> Exp.Lam (Ide.of_string "x", Exp.Var (Ide.of_string "x")) (* TODO: Fresh ide probably not necessary, since we discard it and return the closure environment. Check *)
    | e::ds -> (
      match e with
        | Decl.Let (attrs, ide, exp) -> (
          Exp.Let (attrs, ide, exp, mod_let_desugaring ds)
        )
        (* TODO: | Decl.LetRec *)
        | Export _ -> mod_let_desugaring ds
        | _ -> raise (Invalid_argument "mod_let_desugaring can only accept a list of let declarations")
    )

let restrict_to_intfs (env : 'a Env.t) (intfs : Intf.t list) : 'a Env.t =
  List.filter_map (Env.to_alist env) ~f:(fun (x, v) ->
      List.find intfs ~f:(fun (y, _) -> Ide.equal x y)
      |> Option.map ~f:(fun (x, typ) ->
             (x, Val.apply_intf v typ)))
  |> Env.of_alist
