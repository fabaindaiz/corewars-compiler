open Printf

let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter);;

let enumerate (l : 'a list) : (int * 'a) list =
  List.mapi (fun i x -> (i, x)) l