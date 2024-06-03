open Base
open Stdio

exception InvalidBop of string
exception InvalidUop of string

let raise_invalid_bop bop v1 v2 =
  raise
    (InvalidBop
       ("Cannot apply "
       ^ Sexp.to_string_hum (Bop.sexp_of_t bop)
       ^ " to arguments " ^ Val.to_string v1 ^ " and " ^ Val.to_string v2))

let raise_invalid_uop uop v =
  raise
    (InvalidUop
       ("Cannot apply "
       ^ Sexp.to_string_hum (Uop.sexp_of_t uop)
       ^ " to argument " ^ Val.to_string v))

let next_plugin_id = Util.make_counter 1

let init_env path =
  let env = Aux.set_plugin Env.empty 0 in
  let env = Aux.set_trusted env false in
  let env = Aux.set_path env path in
  Env.bind_all env
    [(Ide.of_string "get_string", (Val.Fun (Env.empty, Ide.of_string "_", Exp.GetString), Lbl.bot));
     (Ide.of_string "get_int", (Val.Fun (Env.empty, Ide.of_string "_", Exp.GetInt), Lbl.bot));
     (Ide.of_string "get_bool", (Val.Fun (Env.empty, Ide.of_string "_", Exp.GetBool), Lbl.bot));
     (Ide.of_string "print", (Val.Fun (Env.empty, Ide.of_string "x", Exp.Print (Exp.Var (Ide.of_string "x"))), Lbl.bot));
     (Ide.of_string "length", (Val.Fun (Env.empty, Ide.of_string "x", Exp.TupleLength (Exp.Var (Ide.of_string "x"))), Lbl.bot))]

let rec eval_exp (env : Exp.t Val.t Env.t) (pc : Lbl.t) (exp : Exp.t) :
    Exp.t Val.t =
  match exp with
  | Var ide -> (
      let v, ell = Env.lookup_exn env ide in
      match v with
      | Val.Defer (delta', expr) ->
          let v, ell2 = eval_exp delta' pc expr in
          (v, Lbl.joins [ pc; ell; ell2 ])
      | _ -> (v, Lbl.join pc ell))

  | Lit (v, _) -> (v, pc)

  | Field (modexpr, fieldide) -> (
      let modval, l = eval_exp env pc modexpr in
      match modval with
      | Mod modenv | TMod modenv | Plugin modenv -> (
          match Env.lookup modenv fieldide with
          | Some (v, l') -> (v, Lbl.joins [ pc; l; l' ])
          | None ->
              failwith
                ("Field " ^ Ide.to_string fieldide ^ " not present in "
               ^ Val.to_string modval))
      | _ ->
          failwith
            ("Trying to access field of non-module value: "
           ^ Val.to_string modval))

  | Bop (bop, e1, e2) -> (
      let v1, l1 = eval_exp env pc e1 in
      let v2, l2 = eval_exp env pc e2 in
      let final_label = Lbl.joins [ pc; l1; l2 ] in
      match (bop, v1, v2) with
      | Seq, _, v2 -> (v2, final_label) (* Maybe this should be (v2,l2)?*)
      | _, Int i1, Int i2 -> (
          let ibop =
            match bop with
            | Addition -> Some ( + )
            | Subtraction -> Some ( - )
            | Multiplication -> Some ( * )
            | Division -> Some ( / )
            | _ -> None
          in
          match ibop with
          | Some bop -> (Val.Int (bop i1 i2), final_label)
          | None -> (
              let bbop =
                match bop with
                | Equals -> Some ( = )
                | Less -> Some ( < )
                | More -> Some ( > )
                | LessEq -> Some ( <= )
                | MoreEq -> Some ( >= )
                | _ -> None
              in
              match bbop with
              | Some bop -> (Val.Bool (bop i1 i2), final_label)
              | None -> raise_invalid_bop bop v1 v2))
      | _, Bool b1, Bool b2 ->
          let bbop =
            match bop with
            | And -> ( && )
            | Or -> ( || )
            | Equals -> Bool.( = )
            | _ -> raise_invalid_bop bop v1 v2
          in
          (Val.Bool (bbop b1 b2), final_label)
      | Addition, String s1, String s2 -> (Val.String (s1 ^ s2), final_label)
      | Equals, Tuple t1, Tuple t2 -> (Val.Bool
        (match t1, t2 with
          | [], [] -> true
          | _ -> false),
        final_label)
      | Equals, String s1, String s2 -> (Val.Bool (String.(=) s1 s2), final_label)
      | _ -> raise_invalid_bop bop v1 v2)

  | Uop (uop, e) -> (
      let v, l = eval_exp env pc e in
      match (uop, v) with
      | Not, Bool b -> (Bool (not b), Lbl.join pc l)
      | Minus, Int i -> (Int (-i), Lbl.join pc l)
      | _ -> raise_invalid_uop uop v)

  | App (funexp, argexp) ->
      let f, lf = eval_exp env pc funexp in
      let v = eval_exp env pc argexp in

      let apply env' x e v =
        let env'' = Env.bind env' x v in
        let u, lu = eval_exp env'' (Lbl.join pc lf) e in
        (u, Lbl.joins [ pc; lf; lu ])
      in

      (match f with
       | Fun (env', x, e) -> apply env' x e v

       | ValType (Fun (env', x, e), Typ.Fun (t1, t2)) ->
           let v' = Val.apply_intf v t1 in
           let u = apply env' x e v' in
           let u', lu' = Val.apply_intf u t2 in
           (u', Lbl.joins [pc; lf; lu'])

       | _ -> failwith ("Applying non function value: "
                        ^ Val.to_string f))

  | Lam (ide, body) -> (Val.Fun (env, ide, body), pc)

  | Fix expr ->
      let closure, l = eval_exp env pc expr in
      (match closure with
       | Fun (clenv, ide, body) ->
           let newenv =
             Env.bind clenv ide
               (Val.Defer (clenv, Exp.Fix (Exp.Lam (ide, body))), l)
           in
           let v, l' = eval_exp newenv (Lbl.join pc l) body in
           (v, Lbl.joins [ pc; l; l' ])
       | _ ->
           failwith
             ("Applying fixpoint to non function value: " ^ Val.to_string closure))

  | Fixs e ->
      (match eval_exp env pc e with
       | Tuple fs, l ->
           let n = List.length fs in
           let ys = Ide.fresh_list "y" n in
           let env'' = List.zip_exn ys fs |> Env.bind_all env in
           let es =
             List.map fs ~f:(function
                 | Fun (envi', x, ei'), li ->
                     let ys_exp = List.map ys ~f:(fun y -> Exp.Var y) in
                     let defer_tuple =
                       (Val.Tuple (
                            List.mapi fs ~f:(fun j (_, lj) ->
                                (Val.Defer
                                   (env'',
                                    Exp.TupleField
                                      (Exp.Fixs (Exp.Tuple ys_exp),
                                       Exp.Lit (Val.Int j, Lbl.bot))),
                                 lj))),
                        li)
                     in
                     eval_exp
                       (Env.bind envi' x defer_tuple)
                       (Lbl.join pc li)
                       ei'

                 | v, _ -> failwith ("Applying fixpoint to non function value: "
                                     ^ Val.to_string v))
           in
           let _, ls = List.unzip fs in
           let _, ls' = List.unzip es in
           (Val.Tuple es, Lbl.joins (pc :: l :: ls @ ls'))

       | v, _ -> failwith ("Applying fix* to non tuple value: "
                           ^ Val.to_string v))

  | Let (attrs, ide, expr, body) ->
      let v1, l1 = match expr with
        | Exp.TupleField (et, ei) ->
            eval_tuple_field env pc et ei
        | _ -> eval_exp env pc expr
      in

      let plugin = Aux.get_plugin_exn env in
      let l1' = Let_attr.cast_lbl l1 ~to_:attrs ~plugin in
      let env' = Env.bind env ide (v1, l1') in
      let v, l = eval_exp env' pc body in
      (v, Lbl.join pc l)

  | LetRec (decls, body) ->
      let xs = Ide.fresh "xs" in

      let wrap e' =
        let rec aux i = function
          | [] -> e'
          | (attrsi, xi) :: xs' ->
            Exp.Let (attrsi, xi,
                     Exp.TupleField (Exp.Var xs,
                                     Exp.Lit (Val.Int i, Lbl.bot)),
                     aux (i + 1) xs')
        in
        let b = aux 0 (List.map decls ~f:(fun (attrsi, xi, _) ->
                           (attrsi, xi)))
        in
        Exp.Lam (xs, b)
      in

      let fns = List.map decls ~f:(fun (_, _, e) -> wrap e) in

      (match eval_exp env pc (Exp.Fixs (Exp.Tuple fns)) with
       | Val.Tuple vs, _ ->

           let plugin = Aux.get_plugin_exn env in
           let binds =
             List.map2_exn decls vs ~f:(fun (attrs, x_i, _) (v_i, l_i) ->
                 (x_i, (v_i, Let_attr.cast_lbl l_i ~to_:attrs ~plugin)))
           in
           let env' = Env.bind_all env binds in
           let v, l' = eval_exp env' pc body in
           (v, Lbl.join pc l')

       | _ -> failwith "fix* didn't return a tuple")

  | If (guard, bthen, belse) ->
      let bguard, lc = eval_exp env pc guard in
      let resv, resl =
        eval_exp env (Lbl.join pc lc)
          (match bguard with
          | Val.Bool true -> bthen
          | Val.Bool false -> belse
          | _ -> failwith ("Nonboolean guard: " ^ Val.to_string bguard))
      in
      (resv, Lbl.joins [ pc; lc; resl ])

  | Module decls ->
      let modlet = Aux.mod_let_desugaring decls in
      let modclosure = eval_exp (Aux.set_trusted env false) pc modlet in
      (match modclosure with
      | Val.Fun (newenv, _, _), _l -> (Val.Mod (Env.restrict decls newenv), pc)
      | _ ->
          failwith "Impossible! mod_let_desugaring necessarily returns a lambda")

  | TrustedModule decls ->
      let modlet = Aux.mod_let_desugaring decls in
      let modclosure = eval_exp (Aux.set_trusted env true) pc modlet in
      (match modclosure with
      | Val.Fun (newenv, _, _), _l -> (Val.Mod (Env.restrict decls newenv), pc)
      | _ ->
          failwith "Impossible! mod_let_desugaring necessarily returns a lambda")

  | Print x ->
      let v, l = eval_exp env pc x in
      let l' = Lbl.join pc l in
      (match l' with
       | Public, _ ->
           Out_channel.print_string (Val.to_string v);
           Out_channel.flush stdout
       | _ -> raise Lbl.SecurityException);
      v, l'

  | Tuple exprs ->
      let vals = List.map exprs ~f:(fun e -> eval_exp env pc e) in
      let lbl = Lbl.joins (pc :: List.map vals ~f:(fun (_, l) -> l)) in
      (Val.Tuple vals, lbl)

  | TupleField (et, ei) ->
      (match eval_tuple_field env pc et ei with
       | Val.Defer (env', e), l ->
           let v, l' = eval_exp env' pc e in
           (v, Lbl.joins [pc; l; l'])
       | vl -> vl)

  | TupleLength e ->
      let (v, ell) = eval_exp env pc e in
      (match v with
       | Val.Tuple t -> (Val.Int (List.length t), Lbl.join pc ell)
       | _ -> failwith ("Trying to get length of non-tuple value " ^ (Val.to_string v)))

  | HasAttr (attr, e) ->
      let _, l = eval_exp env pc e in
      (Let_attr.matches_lbl attr l |> Val.Bool, Lbl.join pc l)

  | Plugin (fname, intfs) ->
      let plugin_path = Aux.get_path_exn env ^ "/" ^ fname in
      let e = Parser.parse_file plugin_path in
      let env = init_env (Core.Filename.dirname plugin_path) in
      let env = Aux.set_plugin env (next_plugin_id ()) in
      (match eval_exp env Lbl.bot e with
       | Val.Mod env, l ->
           (Val.Plugin (Aux.restrict_to_intfs env intfs),
            Lbl.join pc l)
       | _ -> failwith "unreachable (plugin evaluated to non-module)")

  | Die -> failwith "Died"

  | Declassify e ->
      let v, (lc, li) = eval_exp env pc e in
      let res = v, (Lbl.Public, li) in

      if Aux.get_trusted_exn env then
        match lc, Aux.get_plugin_exn env with
        | Public, _ -> res
        | Secret p, q when p = q || q = 0 -> res
        | _ -> failwith "Cannot declassify a value from another plugin or main module"

      else failwith "Cannot declassify in untrusted module"

  | Endorse e ->
      let v, (lc, _) = eval_exp env pc e in
      if Aux.get_trusted_exn env
      then  (v, (lc, Lbl.Untainted))
      else failwith "Cannot endorse in untrusted module"

  | DeclassifyPC e ->
      let pc_c, pc_i = pc in

      (if Aux.get_trusted_exn env then
         match pc_c, Aux.get_plugin_exn env with
         | Public, _ -> ()
         | Secret p, q when p = q || q = 0 -> ()
         | _ -> failwith "Cannot declassify secret PC from another plugin or main module"
       else failwith "Cannot declassify in untrusted module");

      let v, (lc, li) = eval_exp env (Lbl.Public, pc_i) e in
      (v, (lc, Lbl.intg_join pc_i li))

  | EndorsePC e ->
      if Aux.get_trusted_exn env then
        let pc_c, _ = pc in
        let v, (lc, li) = eval_exp env (pc_c, Lbl.Untainted) e in
        (v, (Lbl.conf_join pc_c lc, li))

      else
        failwith "Cannot endorse in untrusted module"

  | Assert e ->
    let v, l = eval_exp env pc e in
    (match v with
      | Val.Bool true -> (Val.Bool true, Lbl.join pc l)
      | Val.Bool false -> failwith ("Assertion " ^ (Exp.exp_to_string e) ^ " failed")
      | _ -> failwith "Nonboolean guard in assertion")

  | GetString -> (
    let input = In_channel.input_line Stdio.stdin |> Option.value_exn in
    let (lc, _) = pc in
    (Val.String input, (lc, Lbl.Tainted))
  )
  | GetInt -> (
    let input = In_channel.input_line Stdio.stdin |> Option.value_exn in
    let int = Int.of_string input in
    let (lc, _) = pc in
    (Val.Int int, (lc, Lbl.Tainted))
  )
  | GetBool -> (
    let input = In_channel.input_line Stdio.stdin |> Option.value_exn in
    let bool = Bool.of_string input in
    let (lc, _) = pc in
    (Val.Bool bool, (lc, Lbl.Tainted))
  )

  | Handle _ | Do _ ->
      failwith ("Not implemented: "
                ^ (Exp.sexp_of_t exp |> Sexp.to_string_hum))

and eval_tuple_field env pc et ei =
  let vt, lt = eval_exp env pc et in
  let vi, li = eval_exp env pc ei in
  (match vt, vi with
   | Val.Tuple vs, Val.Int i ->
       let v, l = List.nth_exn vs i in
       (v, Lbl.joins [pc; lt; li; l])

   | Val.Tuple _, v -> failwith ("Tried using non-integer "
                                 ^ Val.to_string v
                                 ^ " as tuple index")
   | v, _ -> failwith ("Tried indexing non-tuple "
                       ^ Val.to_string v))


let eval path = eval_exp (init_env path) Lbl.bot
