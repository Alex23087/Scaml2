open Base

type 'a t = Let of Let_attr.t list * Ide.t * 'a
          | Export of Ide.t
[@@deriving sexp]
