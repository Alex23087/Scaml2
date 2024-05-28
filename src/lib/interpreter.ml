open Base

let rec eval_exp (env: Exp.t Val.t Env.t) (pc: Lbl.t) (exp: Exp.t): Exp.t Val.t =
	match exp with
		| Var ide -> (
			let (v, ell) = Env.lookup_exn env ide in
			match v with
				| Val.Defer (delta', expr) -> (
					let (v, ell2) = eval_exp delta' pc expr in
					(v, Lbl.join (Lbl.join pc ell) ell2)
				)
				| _ -> (v, Lbl.join pc ell)
		)
		| Lit (v, _) -> (v, pc)
		| Lam (ide, body) -> (
			((Val.Fun (env, ide, body)), pc)
		)
		| Let (attrs, ide, expr, body) -> (
			let (v1, l1) = eval_exp env pc expr in
			(if Lbl.(<=) l1 (Aux.attr_list_to_lbl attrs ~default:Lbl.top)
				then ()
				else raise Lbl.SecurityException);
			let new_env = Env.bind env ide (v1, Lbl.join l1 (Aux.attr_list_to_lbl attrs ~default:Lbl.bot)) in
			let (v, l) = eval_exp new_env pc body in
			(v, Lbl.join pc l)
		)
		| Module decls -> (
			let modlet = Aux.mod_let_desugaring decls in
			let modclosure = eval_exp env pc modlet in
			match modclosure with
				| (Val.Fun (newenv, _, _), _l) -> (
					(Val.Mod newenv, pc)
				)
				| _ -> failwith "Impossible! mod_let_desugaring necessarily returns a lambda"
		)
		| Print x -> (
			let res = eval_exp env pc x in
			(* res
			|> Val.sexp_of_t (Exp.sexp_of_t)
			|> Sexp.to_string_hum
			|> Stdio.print_endline; *)
			res
			|> fst
			|> Val.to_string
			|> Stdio.print_string;
			res
		)
		| _ -> failwith ("Not implemented: " ^ (Exp.sexp_of_t exp |> Sexp.to_string_hum))

let eval = eval_exp Env.empty_env Lbl.bot