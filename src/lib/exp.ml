open Base

type t = Var of Ide.t
       | Lit of t Val.t
       | Field of t * Ide.t

       | Bop of Bop.t * t * t
       | Uop of Uop.t * t

       | Tuple of t list
       | TupleField of t * t

       | App of t * t
       | Lam of Ide.t * t

       | Fix of t
       | Fixs of t

       | Let of Let_attr.t list * Ide.t * t * t
       | LetRec of (Let_attr.t list * Ide.t * t) list * t

       | If of t * t * t

       | Handle of t Handler.t * t
       | Do of Ide.t * t list

       | Module of t Decl.t list
       | TruMod of t Decl.t list
       | Plugin of string * Intf.t list

       | HasAttr of Let_attr.t * t
       | Endorse of t
       | Declassify of t

       | Print of t

       | Die
[@@deriving sexp]

(*
let wrap s = "(" ^ s ^ ")"

let to_string e =
  let indent i = String.init (i * 4) ~f:(Fn.const ' ') in
  let rec aux i e =
    (match e with
     | Var x -> Ide.to_string x
     | Lit v -> Val.to_string v
     | Field (e, x) -> aux i e ^ "." ^ Ide.to_string x
     | Bop (op, e1, e2) ->
        aux i e1 ^ " " ^ Bop.to_string op ^ " " ^ aux (i + 1) e2
        |> wrap
     | Uop (op, e) -> Uop.to_string op ^ aux (i + 1) e
     | App (e1, e2) -> aux i e1 ^ " " ^ aux (i + 1) e2 |> wrap
     | Lam (x, e) -> "\\" ^ Ide.to_string x ^ ". " aux (i + 1) e |> wrap
     | Fix es ->
        let s = List.map es ~f:(aux (i + 1)) |> String.concat ~sep:", " in
        "fix [" ^ s ^ "]" |> wrap
     | Let (x, e, b) ->
        "let " ^ Ide.to_string x ^ " = " ^ aux (i + 1) e ^ "in\n"
        ^ indent i ^ (aux i b)
     | If (c, t, e) ->
        "if " ^ aux (i + 1) c ^ " then\n"
          ^ indent (i + 1) ^ aux (i + 1) t ^ "\n"
          ^ indent i ^ "else\n"
          ^ indent (i + 1) ^ aux (i + 1) e
     | _ -> failwith "unimplemented pretty printer"
    )

  in
  aux 0 e
*)

(*
class type ['repr] expr  = object
  method int : int -> 'repr
  method str : string -> 'repr
  method ide : string -> 'repr

  method letin : string -> 'repr -> 'repr -> 'repr

  method neg : 'repr -> 'repr

  method app : 'repr -> 'repr -> 'repr
  method add : 'repr -> 'repr -> 'repr
  method sub : 'repr -> 'repr -> 'repr
  method div : 'repr -> 'repr -> 'repr
  method mul : 'repr -> 'repr -> 'repr
  method cons : 'repr -> 'repr -> 'repr
end

let int x = fun ro -> ro#int x
let str x = fun ro -> ro#str x
let ide x = fun ro -> ro#ide x

let letin x e b = fun ro -> ro#letin x (e ro) (b ro)

let neg e = fun ro -> ro#neg (e ro)

let app  e1 e2 = fun ro -> ro#app  (e1 ro) (e2 ro)
let add  e1 e2 = fun ro -> ro#add  (e1 ro) (e2 ro)
let sub  e1 e2 = fun ro -> ro#sub  (e1 ro) (e2 ro)
let div  e1 e2 = fun ro -> ro#div  (e1 ro) (e2 ro)
let mul  e1 e2 = fun ro -> ro#mul  (e1 ro) (e2 ro)
let cons e1 e2 = fun ro -> ro#cons (e1 ro) (e2 ro)
*)
