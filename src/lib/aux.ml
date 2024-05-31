open Base

let rec mod_let_desugaring (decls: Exp.t Decl.t Base.list): Exp.t =
  match decls with
  | [] -> Exp.Lam (Ide.of_string "x", Exp.Var (Ide.of_string "x")) (* TODO: Fresh ide probably not necessary, since we discard it and return the closure environment. Check *)
  | e::ds ->
       (match e with
        | Decl.Let (attrs, ide, exp) ->
             Exp.Let (attrs, ide, exp, mod_let_desugaring ds)

        | Decl.LetRec (list) ->
             Exp.LetRec (list, mod_let_desugaring ds)

        | Export _ -> mod_let_desugaring ds)

let restrict_to_intfs (env : 'a Env.t) (intfs : Intf.t list) : 'a Env.t =
  List.filter_map (Env.to_alist env) ~f:(fun (x, v) ->
      List.find intfs ~f:(fun (y, _) -> Ide.equal x y)
      |> Option.map ~f:(fun (x, typ) ->
             (x, Val.apply_intf v typ)))
  |> Env.of_alist


let plugin_ide = Ide.of_string "@plugin"
let trusted_ide = Ide.of_string "@trusted"
let path_ide = Ide.of_string "@path"

let set_plugin env x = Env.bind env plugin_ide (Val.Int x, Lbl.bot)
let set_trusted env x = Env.bind env trusted_ide (Val.Bool x, Lbl.bot)
let set_path env x = Env.bind env path_ide (Val.String x, Lbl.bot)

let get_plugin_exn env =
  let p = Env.lookup_exn env plugin_ide in
  match p with
  | Val.Int x, _ -> x
  | _ -> failwith (Ide.to_string plugin_ide ^ " bound to non-integer value")

let get_trusted_exn env =
  let p = Env.lookup_exn env trusted_ide in
  match p with
  | Val.Bool x, _ -> x
  | _ -> failwith (Ide.to_string trusted_ide ^ " bound to non-boolean value")

let get_path_exn env =
  let p = Env.lookup_exn env path_ide in
  match p with
  | Val.String x, _ -> x
  | _ -> failwith (Ide.to_string path_ide ^ " bound to non-string value")

