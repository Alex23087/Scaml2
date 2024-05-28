open Base

exception IdentifierNotBound of string

type 'a t = (Ide.t * 'a) list [@@deriving sexp]

let bind env x v = (x, v) :: env
let lookup env x = List.Assoc.find env x ~equal:Ide.(=)
let lookup_exn env x = Option.value_exn ~error:(Error.of_exn (IdentifierNotBound ("Identifier " ^ (Ide.to_string x) ^" not bound"))) (lookup env x)
