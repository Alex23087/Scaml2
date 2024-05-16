(* open Base *)

type vis = Exported | Private [@@deriving sexp]

type 'a t = vis * Ide.t * 'a [@@deriving sexp]
