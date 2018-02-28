

(* A simple function that multiplies integers *)
let boo a b = a * b

(* Some string *)
let mystr = "Number " ^ string_of_int 10 ^ " is a number."

(* The following does not work completely. Should be able to
   take a string as input *)
let strtest x = "["

let _ = Js.export "boo" boo
let _ = Js.export "hello" 10
let _ = Js.export "mystr" mystr
let _ = Js.export "strtest" strtest
