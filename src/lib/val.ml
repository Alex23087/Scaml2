open Base

type 'a naked_val = Int of int
                  | String of string
                  | Bool of bool
                  | Fun of 'a t Env.t * Ide.t * 'a
                  | Tuple of 'a t list
                  | Defer of 'a
                  | Mod of 'a t Env.t
                  | ValType of 'a naked_val * Type.t
                  | NoSecret of 'a naked_val
[@@deriving sexp]

and 'a t = 'a naked_val * Lbl.t
[@@deriving sexp]

let int x = (Int x, Lbl.bot)
let string x = (String x, Lbl.bot)
let bool x = (Bool x, Lbl.bot)


let get_val ((v, _): 'a t): 'a naked_val = v
let get_lbl ((_, l): 'a t): Lbl.t = l

(* let to_string = function *)
(*   | Int x -> Int.to_string x *)
(*   | String x -> x *)
(*   | Bool x -> Bool.to_string x *)
(*   | Fun _ -> "<function>" *)
(*   | Mod _ -> "<module>" *)
