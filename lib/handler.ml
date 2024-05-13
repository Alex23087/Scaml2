open Base

(* op x* = e, ..., return x = e *)
type 'a t = { ops : (Ide.t * Ide.t list * 'a) list;
              ret : Ide.t * 'a }

let foobar = { ops = []; ret = (Ide.of_string "x"), 1; }
let bazbax x = x.ops, x.ret
