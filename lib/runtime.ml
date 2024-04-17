exception IdentifierNotFoundException
  
type ident = Ast.ident [@@deriving show]
type 'a environment = ident -> 'a

let emptyEnv () = fun _ -> raise IdentifierNotFoundException
let bind (name: ident) (value: 'a) (env: 'a environment) = fun x -> if x = name then value else env x
let rec bindlist (names: ident list) (values: 'a list) (env: 'a environment) =
  match (names, values) with
  | (x::xs, y::ys) -> (
    bindlist xs ys (bind x y env)
  )
  | ([], []) -> env
  | _ -> failwith "Wrong number of parameters passed"

type evaluationType =
  | Int of int
  | Bool of bool
  | Closure of ident list * (Ast.expr[@opaque]) * (evaluationType environment[@opaque])
  | Handler of evaluationType
[@@deriving show]

let unwrapInt v = match v with Int a -> a | _ -> failwith "Wrong type!"

let rec eval (expression: Ast.expr) (env: evaluationType environment) : evaluationType =
  match expression with
    | IntLiteral i -> Int i
    | BoolLiteral b -> Bool b
    | Let (id, exp, body) -> (
      let env = bind id (eval exp env) env in
        eval body env
    )
    | ABinop (op, lhs, rhs) -> ( Int (
      (match op with
        | Plus  -> ( + )
        | Minus -> ( - )
        | Times -> ( * )
        | Div   -> ( / ))
          (unwrapInt(eval lhs env)) (unwrapInt(eval rhs env)
      )
    ))
    | AUnop (op, exp) -> ( Int (
        (match op with
          | Neg -> (-) 0)
            (unwrapInt(eval exp env))
      )
    )
    | Func (params, body) -> (
      Closure (params, body, env)
    )
    | Fix (exp) -> (
      match (eval exp env) with 
        | Closure (is, body, env) -> (
          let rec x = Closure(List.tl is, body, fun y -> if y= (List.hd is) then x else env y) in x
        )
        | _ -> failwith "Applying fixpoint to non-function"
    )
    | Apply (func, params) -> (
      match (eval func env) with
        | Closure (plist, body, clenv) -> (
          eval body (bindlist plist (List.map (fun p -> eval p env) params) clenv)
        )
        | _ -> failwith "Trying to apply a non-function value"
    )
    | _ -> failwith "Not implemented"
    (*
    | BBinop of bbinop * expr * expr
    | BUnop of expr
    | Comparison of comparison * expr * expr
    | IfThenElse of expr * expr * expr
    | Handler of expr list
    | WithHandle of expr * expr
    | CallOp of expr * expr list *)

