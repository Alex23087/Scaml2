open Scaml.Ast
open Scaml.Runtime

let fact = Apply(Fix(Func(["f"; "x"], IfThenElse(
  Comparison(Eq, Var "x", IntLiteral 0),
  IntLiteral 1,
  ABinop(Times, Var "x", Apply(Var "f", [ABinop(Minus, Var "x", IntLiteral 1)]))
))), [IntLiteral 5]);;
print_endline (show_evaluationType (eval fact (emptyEnv ()) (emptyEnv ())));;
flush_all;;

let costa =
  Fix(Func(["_"], IntLiteral 42));;
(* let costa =
  Apply(Apply(Fix(Func(["x"], Var "x")), []),[]);; *)

print_endline (show_evaluationType (eval costa (emptyEnv ()) (emptyEnv ())))

let eff =
  Let("printHandler", 
  Handler (["print", Func(["x"], Print(Var("x")))]),
  WithHandle(
    Var("printHandler"),
    CallOp ("print", [IntLiteral 99])
  )
  );;
eval eff (emptyEnv()) (emptyEnv())

let dyneff =
  Let("printHandler", 
  Handler (["print", Func(["x"], Print(Var("x")))]),
  WithHandle(
    Var("printHandler"),
    WithHandle( 
      Handler (["print", Func(["x"], Print(Print(Var("x"))))]),
      CallOp ("print", [IntLiteral 99])
    )
  )
  );;
eval dyneff (emptyEnv()) (emptyEnv())