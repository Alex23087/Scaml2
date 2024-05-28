open Base

exception InvalidBop of string
exception InvalidUop of string


let raise_invalid_bop bop v1 v2 = raise (InvalidBop ("Cannot apply " ^ (Sexp.to_string_hum (Bop.sexp_of_t bop)) ^ " to arguments " ^ (Val.to_string v1) ^ " and " ^ (Val.to_string v2)))
let raise_invalid_uop uop v = raise (InvalidUop ("Cannot apply " ^ (Sexp.to_string_hum (Uop.sexp_of_t uop)) ^ " to argument " ^ (Val.to_string v)))


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
		| Field (modexpr, fieldide) -> (
			let (modval, l) = eval_exp env pc modexpr in
			match modval with
				| Mod modenv
				| TMod modenv
				| Plugin modenv -> (
					match Env.lookup modenv fieldide with
						| Some (v, l') -> (
							(v, Lbl.join (Lbl.join pc l) l')
						)
						| None -> failwith ("Field " ^ (Ide.to_string fieldide) ^ " not present in " ^ (Val.to_string modval))
				)
				| _ -> failwith ("Trying to access field of non-module value: " ^ (Val.to_string modval))
		)
		| Bop (bop, e1, e2) -> (
			let (v1, l1) = eval_exp env pc e1 in
			let (v2, l2) = eval_exp env pc e2 in
			let final_label = Lbl.join (Lbl.join pc l1) l2 in
			match (bop, v1, v2) with
				| (Seq, _, v2) -> (v2, final_label) (* Maybe this should be (v2,l2)?*)
				| (_, Int i1, Int i2) -> (
					let ibop = (match bop with
						| Addition -> Some (+)
						| Subtraction -> Some (-)
						| Multiplication -> Some ( * )
						| Division -> Some (/)
						| _ -> None) in
					match ibop with
						| Some bop -> (Val.Int (bop i1 i2), final_label)
						| None -> (
							let bbop = (match bop with
								| Equals -> Some (=)
								| Less -> Some (<)
								| More -> Some (>)
								| LessEq -> Some (<=)
								| MoreEq -> Some (>=)
								| _ -> None) in
							(match bbop with
								| Some bop -> (Val.Bool (bop i1 i2), final_label)
								| None -> raise_invalid_bop bop v1 v2)
						)
				)
				| (_, Bool b1, Bool b2) -> (
					let bbop = (match bop with
						| And -> (&&)
						| Or -> (||)
						| Equals -> Bool.(=)
						| _ -> raise_invalid_bop bop v1 v2) in
					((Val.Bool (bbop b1 b2)), final_label)
				)
				| (Addition, String s1, String s2) -> (
					((Val.String (s1 ^ s2)), final_label)
				)
				| _ -> raise_invalid_bop bop v1 v2
		)
		| Uop (uop, e) -> (
			let (v, l) = eval_exp env pc e in
			match (uop, v) with
				| (Not, Bool b) -> (Bool (not b), Lbl.join pc l)
				| (Minus, Int i) -> (Int (-i), Lbl.join pc l)
				| _ -> raise_invalid_uop uop v
		)
		| App (funexp, argexp) -> (
			let (closure, lf) = eval_exp env pc funexp in
			let param = eval_exp env pc argexp in
			match closure with
				| Fun (clenv, ide, body) -> (
					let newenv = Env.bind clenv ide param in
					let (u, lu) = eval_exp newenv (Lbl.join pc lf) body in
					(u, Lbl.join (Lbl.join pc lf) lu)
				)
				| _ -> failwith ("Applying non function value: " ^ (Val.to_string closure))
		)
		| Lam (ide, body) -> (
			((Val.Fun (env, ide, body)), pc)
		)
		| Fix expr -> (
			let (closure, l) = eval_exp env pc expr in
			match closure with
				| Fun (clenv, ide, body) -> (
					let newenv = Env.bind clenv ide ((Val.Defer (clenv, Exp.Fix(Exp.Lam(ide, body)))), l) in
					let (v, l') = eval_exp newenv (Lbl.join pc l) body in
					(v, Lbl.join (Lbl.join pc l) l')
				)
				| _ -> failwith ("Applying fixpoint to non function value: " ^ (Val.to_string closure))
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
		| If (guard, bthen, belse) -> (
			let (bguard, lc) = eval_exp env pc guard in
			let (resv, resl) = eval_exp env pc (match bguard with
				| Val.Bool true -> bthen
				| Val.Bool false -> belse
				| _ -> failwith ("Nonboolean guard: " ^ (Val.to_string bguard))
			) in
			(resv, Lbl.join (Lbl.join pc lc) resl)
		)
		| Module decls -> (
			let modlet = Aux.mod_let_desugaring decls in
			let modclosure = eval_exp env pc modlet in
			match modclosure with
				| (Val.Fun (newenv, _, _), _l) -> (
					(Val.Mod (Env.restrict decls newenv), pc)
				)
				| _ -> failwith "Impossible! mod_let_desugaring necessarily returns a lambda"
		)
		| Print x -> (
			let res = eval_exp env pc x in
			(* res
			|> Val.sexp_of_t (Exp.sexp_of_t)
			|> Sexp.to_string_hum
			|> Stdio.print_endline; *)
			(
				match res with
					| (_, (Secret, _)) ->
						raise Lbl.SecurityException
					| (res, _) -> res
						|> Val.to_string
						|> Stdio.print_string
			); res
		)
		| _ -> failwith ("Not implemented: " ^ (Exp.sexp_of_t exp |> Sexp.to_string_hum))

let eval = eval_exp Env.empty_env Lbl.bot