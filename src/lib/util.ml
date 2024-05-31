open Base

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let make_counter init =
  let x = ref (init - 1) in
  fun () -> x := !x + 1; !x

