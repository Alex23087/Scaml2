(* open Base *)

type vis = Exported | Private

type 'a t = vis * Ide.t * 'a
