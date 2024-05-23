open Base

(* op x* = e, ..., return x = e *)
type 'a t = { ops : (Ide.t * Ide.t list * 'a) list;
              ret : Ide.t * 'a }
[@@deriving sexp]
