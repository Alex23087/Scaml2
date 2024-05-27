open Base

type 'a t = Let of Let_attr.t list * Ide.t * 'a
          | LetRec of (Let_attr.t list * Ide.t * 'a) list
          | Export of Ide.t
[@@deriving sexp]
