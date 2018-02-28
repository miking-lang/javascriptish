

let boo a b = a * b

let mystr = "Number " ^ string_of_int 10 ^ " is a number"

let _ = Js.export "boo" boo
let _ = Js.export "hello" 10
let _ = Js.export "mystr" mystr
