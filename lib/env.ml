open Base

type 'a t = (Ide.t * 'a) list

let bind env x v = (x, v) :: env
let lookup env x = List.Assoc.find env x ~equal:Ide.(=)
