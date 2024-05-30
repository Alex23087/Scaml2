open Base

exception IdentifierNotBound of string

type 'a t = (Ide.t * 'a) list [@@deriving sexp]

let of_alist x = x
let to_alist x = x

let bind env x v = (x, v) :: env
let bind_all env bindings = List.fold bindings ~init:env ~f:(fun env (x, v) -> bind env x v)
let lookup env x = List.Assoc.find env x ~equal:Ide.(=)

let lookup_exn env x =
  Option.value_or_thunk (lookup env x)
    ~default:(fun () ->
      let msg = "Identifier " ^ (Ide.to_string x) ^ " not bound" in
      raise (IdentifierNotBound msg))

let empty_env: 'a t = []

let restrict (export_list: 'b Decl.t list) (env: 'a t): 'a t =
  let exports = List.filter_map export_list
                  ~f:(function
                      | Decl.Export i -> Some i
                      | _ -> None)
  in
  List.filter env ~f:(fun (i, _) -> List.mem exports i ~equal:Ide.(=))
