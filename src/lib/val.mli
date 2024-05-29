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

val int : int -> 'a t
val string : string -> 'a t
val bool : bool -> 'a t
val get_val : 'a t -> 'a naked_val
val get_lbl : 'a t -> Lbl.t
val to_string : 'a naked_val -> string
val apply_intf : 'a t -> Typ.t -> 'a t
