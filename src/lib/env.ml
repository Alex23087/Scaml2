open Base

exception IdentifierNotBound of string

type 'a t = (Ide.t * 'a) list [@@deriving sexp]

let bind env x v = (x, v) :: env
let lookup env x = List.Assoc.find env x ~equal:Ide.(=)
let lookup_exn env x = Option.value_exn ~error:(Error.of_exn (IdentifierNotBound ("Identifier " ^ (Ide.to_string x) ^" not bound"))) (lookup env x)
let empty_env: 'a t = []


let restrict (export_list: 'b Decl.t list) (env: 'a t): 'a t =
  let exports = List.filter_map export_list ~f:(fun d -> match d with Decl.Export i -> Some i | _ -> None) in
  List.filter env ~f:(fun (i, _) -> List.mem exports i ~equal:Ide.(=))