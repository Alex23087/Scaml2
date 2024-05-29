open Base

type 'a naked_val = Int of int
                  | String of string
                  | Bool of bool
                  | Fun of 'a t Env.t * Ide.t * 'a
                  | Tuple of 'a t list
                  | Defer of 'a t Env.t * 'a
                  | Mod of 'a t Env.t
                  | TMod of 'a t Env.t
                  | Plugin of 'a t Env.t
                  | ValType of 'a naked_val * Typ.t
                  | NoSecret of 'a naked_val
[@@deriving sexp]

and 'a t = 'a naked_val * Lbl.t
[@@deriving sexp]

let int x = (Int x, Lbl.bot)
let string x = (String x, Lbl.bot)
let bool x = (Bool x, Lbl.bot)

let get_val ((v, _): 'a t): 'a naked_val = v
let get_lbl ((_, l): 'a t): Lbl.t = l

let rec to_string = function
  | Int x -> Int.to_string x
  | String x -> x
  | Bool x -> Bool.to_string x
  | Fun _ -> "<function>"
  | Mod _ -> "<module>"
  | Tuple vs -> "[" ^ (String.concat ~sep:", " (List.map ~f:to_string (List.map ~f:fst vs))) ^ "]"
  | _ -> failwith "to_string not implemented"

let rec apply_intf (v, ell: 'a t) (tau: Typ.t): 'a t =
  let ell' = Lbl.join ell (Lbl.Public, Lbl.Tainted) in
  match (v, tau) with
    | (_, Typ.Any)
    | (Int _, Typ.Int)
    | (String _, Typ.String)
    | (Bool _, Typ.Bool) -> (v, ell')

    | (Fun _, Typ.Fun _) -> (ValType (v, tau), ell')

    | (Tuple vs, Typ.Tuple ts) ->
       let vs' = List.map2_exn vs ts ~f:apply_intf in
       (Tuple vs', ell')

    | (NoSecret v', _) ->
       let v'', _ = apply_intf (v', ell) tau in
       (NoSecret v'', ell')

    | _ -> raise (Invalid_argument "Cannot apply interface to value")
