open Base

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
      | _ -> raise_invalid_bop bop v1 v2)

  | Uop (uop, e) -> (
      let v, l = eval_exp env pc e in
      match (uop, v) with
      | Not, Bool b -> (Bool (not b), Lbl.join pc l)
      | Minus, Int i -> (Int (-i), Lbl.join pc l)
      | _ -> raise_invalid_uop uop v)

  | App (funexp, argexp) -> (
      let closure, lf = eval_exp env pc funexp in
      let param = eval_exp env pc argexp in
      match closure with
      | Fun (clenv, ide, body) ->
          let newenv = Env.bind clenv ide param in
          let u, lu = eval_exp newenv (Lbl.join pc lf) body in
          (u, Lbl.joins [ pc; lf; lu ])
      | _ -> failwith ("Applying non function value: " ^ Val.to_string closure))

  | Lam (ide, body) -> (Val.Fun (env, ide, body), pc)

  | Fix expr -> (
      let closure, l = eval_exp env pc expr in
      match closure with
      | Fun (clenv, ide, body) ->
          let newenv =
            Env.bind clenv ide
              (Val.Defer (clenv, Exp.Fix (Exp.Lam (ide, body))), l)
          in
          let v, l' = eval_exp newenv (Lbl.join pc l) body in
          (v, Lbl.joins [ pc; l; l' ])
      | _ ->
          failwith
            ("Applying fixpoint to non function value: " ^ Val.to_string closure)
      )

  | Fixs expr -> (
      let tuplexpr, l = eval_exp env pc expr in
      match tuplexpr with
      | Tuple closures ->
          let tied_closures : Exp.t Val.t list =
            List.map closures ~f:(fun (closure, l') ->
                match closure with
                | Fun (clenv, ide, body) ->
                    let newenv =
                      Env.bind clenv ide (Val.Defer (clenv, Exp.Fixs expr), l')
                    in
                    let v, l'' = eval_exp newenv (Lbl.join pc l') body in
                    (v, Lbl.joins [ pc; l'; l'' ])
                | _ ->
                    failwith
                      ("Applying fixpoint to non function value: "
                     ^ Val.to_string closure))
          in
          let lbl =
            Lbl.joins (l :: List.map tied_closures ~f:(fun (_, l) -> l))
          in
          (Val.Tuple tied_closures, lbl)
      | _ ->
          failwith
            ("Applying fixpoint to non tuple value: " ^ Val.to_string tuplexpr))

  | Let (attrs, ide, expr, body) ->
      let plugin = Aux.get_plugin_exn env in
      let v1, l1 = eval_exp env pc expr in
      let l1' = Let_attr.cast_lbl l1 ~to_:attrs ~plugin in
      let env' = Env.bind env ide (v1, l1') in
      let v, l = eval_exp env' pc body in
      (v, Lbl.join pc l)

  | LetRec (decls, body) ->
      let fns = List.map decls ~f:(fun (_, x, e) -> Exp.Lam (x, e)) in
      (match eval_exp env pc (Exp.Fixs (Exp.Tuple fns)) with
       | Val.Tuple vs, _ -> (* TODO usare il label? *)
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

  | Module decls -> (
      let modlet = Aux.mod_let_desugaring decls in
      let modclosure = eval_exp env pc modlet in
      match modclosure with
      | Val.Fun (newenv, _, _), _l -> (Val.Mod (Env.restrict decls newenv), pc)
      | _ ->
          failwith "Impossible! mod_let_desugaring necessarily returns a lambda"
      )

  | Print x ->
      let res = eval_exp env pc x in
      (* res
         |> Val.sexp_of_t (Exp.sexp_of_t)
         |> Sexp.to_string_hum
         |> Stdio.print_endline; *)
      (match res with
      | res, (Public, _) -> res |> Val.to_string |> Stdio.print_string
      | _ -> raise Lbl.SecurityException);
      res

  | Tuple exprs ->
      let vals = List.map exprs ~f:(fun e -> eval_exp env pc e) in
      let lbl = Lbl.joins (pc :: List.map vals ~f:(fun (_, l) -> l)) in
      (Val.Tuple vals, lbl)

  | TupleField (tuple, index) -> (
      let tup, lt = eval_exp env pc tuple in
      let idx, li = eval_exp env pc index in
      match tup with
      | Tuple vals -> (
          match idx with
          | Val.Int i ->(
                let v, lbl = List.nth_exn vals i in
                match v with
                | Val.Defer (delta', expr) ->
                    let v, l2 = eval_exp delta' pc expr in
                    (v, Lbl.joins [ pc; lt; li; lbl; l2 ])
                | _ -> (v, Lbl.joins [ pc; lt; li; lbl ])
              )
          | _ ->
              failwith
                ("Trying to access field of tuple with non-integer index: "
               ^ Val.to_string idx))
      | _ ->
          failwith
            ("Trying to access field of non-tuple value: " ^ Val.to_string tup))

  | HasAttr (attr, e) ->
      let _, l = eval_exp env pc e in
      (Let_attr.matches_lbl attr l |> Val.Bool, Lbl.join pc l)

  | Plugin (fname, intfs) ->
      let e = Parser.parse_file fname in
      (match eval_exp Env.empty Lbl.bot e with
       | Val.Mod env, l ->
           (Val.Plugin (Aux.restrict_to_intfs env intfs),
            Lbl.join pc l)
       | _ -> failwith "unreachable (plugin evaluated to non-module)")

  | Die -> failwith "Died"

  | _ ->
      failwith ("Not implemented: " ^ (Exp.sexp_of_t exp |> Sexp.to_string_hum))

let main_env = Aux.set_plugin Env.empty 0

let eval = eval_exp main_env Lbl.bot
