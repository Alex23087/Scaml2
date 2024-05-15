open Base
open Stdio
open Scaml2

let () =
  let fname = (Sys.get_argv ()).(1) in
  match In_channel.with_file fname ~f:Lexer.tokenize with
  | Ok toks -> Parser.parse toks
  | Error err -> Out_channel.print_endline (Lexer.error_to_string err)
