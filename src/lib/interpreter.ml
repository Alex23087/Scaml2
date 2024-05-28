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
		| _ -> failwith "Not implemented"