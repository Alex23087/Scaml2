open Scaml.Ast
open Scaml.Runtime

let fact = Apply(Func(["f"; "x"], IfThenElse()), [IntLiteral 1; IntLiteral 1]);;
print_endline (show_evaluationType (eval fact (emptyEnv ())))