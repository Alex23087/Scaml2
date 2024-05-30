open Base

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


let plugin_ide = Ide.of_string "@plugin"
let trusted_ide = Ide.of_string "@trusted"

let set_plugin env p = Env.bind env plugin_ide (Val.Int p, Lbl.bot)
let set_trusted env t = Env.bind env trusted_ide (Val.Bool t, Lbl.bot)

let get_plugin_exn env =
  let p = Env.lookup_exn env plugin_ide in
  match p with
    | Val.Int p, _ -> p
    | _ -> failwith (Ide.to_string plugin_ide ^ " bound to non-integer value")

let get_trusted_exn env =
  let p = Env.lookup_exn env trusted_ide in
  match p with
    | Val.Bool t, _ -> t
    | _ -> failwith (Ide.to_string trusted_ide ^ " bound to non-boolean value")

