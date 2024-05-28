open Base
open Stdio
open Scaml2

let print_exp exp =
	let formatter = Stdlib.Format.formatter_of_out_channel (Out_channel.stdout) in
	Sexp.pp_hum formatter (Exp.sexp_of_t exp);
	Stdlib.Format.pp_print_flush formatter ();
	Out_channel.print_endline "";
	exp

let () =
	let args = Sys.get_argv() in
  let fname = args.(1) in
	let should_print_expr = if (Array.length args) > 2 then (String.(=) (args.(2)) "--print-expr") else false in
  match In_channel.with_file fname ~f:Lexer.tokenize with
  | Ok toks ->
	  (* Out_channel.print_endline (Lexer.toks_to_string toks); *)
	  let parsed = Parser.parse toks in
		parsed
		|> (if should_print_expr then print_exp else (fun x -> x))
	  |> Interpreter.eval
	  (* |> (fun (v, l) -> "(" ^ Val.to_string v ^ ", " ^ Sexp.to_string_hum (Lbl.sexp_of_t l) ^ ")")
		|> print_endline *)
		|> fun _ -> ()
  | Error err -> Out_channel.print_endline ("lexer error: " ^ Lexer.error_to_string err)
