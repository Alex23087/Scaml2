open Base

type 'a naked_val = Int of int
                  | String of string
                  | Bool of bool
                  | Fun of 'a t Env.t * Ide.t * 'a
                  | Mod of unit (* TODO *)

and 'a t = 'a naked_val * Lbl.t

let int x = (Int x, Lbl.bot)
let string x = (String x, Lbl.bot)
let bool x = (Bool x, Lbl.bot)
let fun_ env x e = (Fun (env, x, e), Lbl.bot)

(* let to_string = function *)
(*   | Int x -> Int.to_string x *)
(*   | String x -> x *)
(*   | Bool x -> Bool.to_string x *)
(*   | Fun _ -> "<function>" *)
(*   | Mod _ -> "<module>" *)
