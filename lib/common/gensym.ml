let counter = ref 0
let init () = counter := 0

let fresh s =
  let id = !counter in
  counter := id + 1;
  s ^ "." ^ string_of_int id
