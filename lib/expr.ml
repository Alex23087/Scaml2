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
