type ident = Ast.ident [@@deriving show]

exception IdentifierNotFoundException of ident

type 'a environment = ident -> 'a

let rec drop n lst =
  if n <= 0 then lst
  else match lst with
       | [] -> []
       | _ :: tl -> drop (n - 1) tl

let rec take n lst =
  if n <= 0 then []
  else match lst with
       | [] -> []
       | hd :: tl -> hd :: take (n - 1) tl

let emptyEnv () = fun x -> raise (IdentifierNotFoundException x)
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
  | Handler of (ident * evaluationType) list
  | Lista of evaluationType list
[@@deriving show]

let unwrapInt v = match v with Int a -> a | _ -> failwith "Wrong type!"
let unwrapBool v = match v with Bool a -> a | _ -> failwith "Wrong type!"

let bindHandler (h: evaluationType) (env: evaluationType environment) = fun (x: ident) -> match h with
  | Handler (ops) -> (
    if List.exists (fun y -> (fst y) = x) ops
      then h
      else env x
  )
  | _ -> failwith "Trying to handle with a non handler";;

let rec eval (expression: Ast.expr) (env: evaluationType environment) (effEnv: evaluationType environment): evaluationType =
  match expression with
    | IntLiteral i -> Int i
    | BoolLiteral b -> Bool b
    | Let (id, exp, body) -> (
      let env = bind id (eval exp env effEnv) env in
        eval body env effEnv
    )
    | ABinop (op, lhs, rhs) -> ( Int (
      (match op with
        | Plus  -> ( + )
        | Minus -> ( - )
        | Times -> ( * )
        | Div   -> ( / ))
          (unwrapInt(eval lhs env effEnv)) (unwrapInt(eval rhs env effEnv)
      )
    ))
    | AUnop (op, exp) -> ( Int (
        (match op with
          | Neg -> (-) 0)
            (unwrapInt(eval exp env effEnv))
      )
    )
    | Func (params, body) -> (
      Closure (params, body, env)
    )
    | Fix (exp) -> (
      match (eval exp env effEnv) with
        | Closure (is, body, env) -> (
          let x = ref (let rec x = Closure(List.tl is, body, fun y -> if y= (List.hd is) then x else env y) in x) in
          while (match !x with | Closure (li, body, env) when List.length li = 0 -> x := eval body env effEnv; true | _ -> false) do
            ()
          done;
          !x
        )
        | _ -> failwith "Applying fixpoint to non-function"
    )
    | Fixes (exps) -> (
      let rec closures = lazy (List.map (fun exp -> match (eval exp env effEnv) with
          | Closure (is, body, env) -> Closure (
            drop (List.length exps) is,
            body,
            fun y -> let io = List.find_index (fun i -> i = y) (take (List.length exps) is) in 
              match io with
                | Some i -> (List.nth (Lazy.force closures) i)
                | None -> env y
            )
          | _ -> failwith "Applying fixpoint to non-function"
      ) exps) in Lista (Lazy.force closures)
    )
    | Apply (func, params) -> (
      match (eval func env effEnv) with
        | Closure (plist, body, clenv) -> (
          eval body (bindlist plist (List.map (fun p -> eval p env effEnv) params) clenv) effEnv
        )
        | _ -> failwith "Trying to apply a non-function value"
    )
    | Comparison (op, lhs, rhs) -> ( Bool (
      (match op with
        | Lt -> (<)
        | Gt -> (>)
        | Leq -> (<=)
        | Geq -> (>=)
        | Eq -> (=)
        | Neq -> (<>))
        (unwrapInt(eval lhs env effEnv)) (unwrapInt(eval rhs env effEnv))
      )
    )
    | Var ident -> (
      env ident
    )
    | IfThenElse (guard, thenbranch, elsebranch) -> (
      match eval guard env effEnv with
        | Bool b -> (
          eval (if b then thenbranch else elsebranch) env effEnv
        )
        | _ -> failwith "Guard cannot be non-bool"
    )
    | BBinop (op, lhs, rhs) -> ( Bool (
      (match op with
        | And -> (&&)
        | Or -> (||))
        (unwrapBool(eval lhs env effEnv)) (unwrapBool(eval rhs env effEnv))
      )
    )
    | BUnop (op, exp) -> ( Bool (
      (match op with
        | Not -> (not))
        (unwrapBool(eval exp env effEnv))
      )
    )
    | Handler (exprs) -> (
      Handler (List.map (fun (id, e) -> (id, eval e env effEnv)) exprs)
    )
    | WithHandle (handler, body) -> (
      let handler = eval handler env effEnv in
      match handler with
          | Handler (_) -> (
            eval body env (bindHandler handler effEnv)
          )
          | _ -> failwith "Handling with a non-handler value"
    )
    | CallOp (op, params) -> (
      let[@warning "-partial-match"] Handler (ops) = effEnv op in
      let op = List.find (fun y -> (fst y) = op) ops in
      match snd op with
        | Closure (plist, body, clenv) -> (
          let newEnv = bindlist plist (List.map (fun p -> eval p env effEnv) params) clenv in
          eval body newEnv effEnv
        )
        | _ -> failwith "Operation is wrongly typed"
    )
    | Print (e) -> (
      let e = eval e env effEnv in 
      (print_endline (show_evaluationType e));
      e
    )
    (* | _ -> failwith "Not implemented" *)
