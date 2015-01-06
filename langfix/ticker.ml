
let printStr s = Printf.printf "\"%s\"\n" s

type ticker = { tick : int -> string; tock : int -> string }

let rec ticker0 =
  { tick = (fun n -> if n <= 0 then "" else "tick " ^ ticker0.tock(n))
  ; tock = (fun n -> if n <= 0 then "" else "tock " ^ ticker0.tick(n-1)) }

let tick this n =
  if n <= 0 then "" else "tick " ^ this.tock(n)

let tock this n =
  if n <= 0 then "" else "tock " ^ this.tick(n-1)

let tock' this n =
  this.tick(n-1)

let rec ticker1 =
  { tick = (fun n -> tick ticker1 n)
  ; tock = (fun n -> tock ticker1 n) }

let rec ticker2 =
  { tick = (fun n -> tick ticker2 n)
  ; tock = (fun n -> tock' ticker2 n) }

let rec ticker3 =
  { tick = (fun n -> "! " ^ tick ticker3 n)
  ; tock = (fun n -> "! " ^ tock ticker3 n) }

let wrap g f x y = g (f x y)
let exclaim = wrap (fun s -> "! " ^ s)

let rec ticker3' =
  { tick = (fun n -> (exclaim tick) ticker3' n)
  ; tock = (fun n -> (exclaim tock) ticker3' n) }

let _ = printStr (ticker0.tick(2))
let _ = printStr (ticker1.tick(2))
let _ = printStr (ticker2.tick(2))
let _ = printStr (ticker3.tick(2))
let _ = printStr (ticker3'.tick(2))

let louderTick f g =
  let rec o =
    { tick = (fun n -> (exclaim f) o n)
    ; tock = (fun n -> g o n) }
  in o 

let tock'' = exclaim tock
let tick'' = exclaim tick

let _ = printStr ((louderTick tick tock).tick(2))
let _ = printStr ((louderTick tick tock').tick(2))
let _ = printStr ((louderTick tick'' tock).tick(2))
let _ = printStr ((louderTick tick'' tock'').tick(2))

