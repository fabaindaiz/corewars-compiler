open Printf

(* Lexical Environment *)
type 'a env = (string * 'a) list

let empty_env : 'a env = []

let extend_env (names : string list) (vals : 'a list) (env : 'a env) : 'a env =
  let param_vals = List.combine names vals in
  List.fold_left (fun env p -> p :: env) env param_vals

let lookup_env (x : string) (env : 'a env) : 'a =
    match List.assoc_opt x env with
    | Some v -> v
    | None -> failwith (sprintf "Unbound identifier: %s" x)