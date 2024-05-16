open Base
open Stdio
open Scaml2

let print_exp exp =
   let formatter = Stdlib.Format.formatter_of_out_channel (Out_channel.stdout) in
   Sexp.pp_hum formatter (Exp.sexp_of_t exp);
   Stdlib.Format.pp_print_flush formatter ()

let () =
  let fname = (Sys.get_argv ()).(1) in
  match In_channel.with_file fname ~f:Lexer.tokenize with
  | Ok toks -> Parser.parse toks |> print_exp
  | Error err -> Out_channel.print_endline (Lexer.error_to_string err)
