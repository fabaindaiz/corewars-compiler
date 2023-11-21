open Common
open Syntax
open Type
open Ast_typed
module Env = Map.Make (String)

let err msg = Error.(error (Ty (IncompatibleType msg)))

let rec typecheck e = ignore (typeof Env.empty e) 

and typeof env e =
  match e.desc with
  | Arg _ -> Type.Arg
  | Var x -> (
      match Env.find_opt x env with
      | Some ty -> ty
      | None -> 
          err (Format.asprintf "unbound variable '%s'" x))
  | Label _ -> Type.Instr
  | Prim1 (op, e1) -> (
      let ty1 = typeof env e1 in
      let a, b = ty_of_op1 op in
      if ty1 = a then b
      else
        err (Format.asprintf "expected argument of type '%a', but got '%a'"
             Type.pp a Type.pp ty1))
  | Prim2 (op, e1, e2) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      let a, b, c = ty_of_op2 op in
      if ty1 = a && ty2 = b then c
      else
        err (Format.asprintf
            "expected arguments of types '%a' and '%a', but got '%a' and '%a'"
            Type.pp a Type.pp b Type.pp ty1 Type.pp ty2)
  | Prim3 (op, e1, e2, e3) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      let ty3 = typeof env e3 in
      let a, b, c, d = ty_of_op3 op in
      if ty1 = a && ty2 = b && ty3 = c then d
      else
        err (Format.asprintf
            "expected arguments of types '%a', '%a' and '%a', but got '%a', '%a' and '%a'"
            Type.pp a Type.pp b Type.pp c Type.pp ty1 Type.pp ty2 Type.pp ty3)
  | Let (binding, e) -> (
      match typeof env binding.e with
      | ty when ty = binding.ty -> typeof (Env.add binding.name ty env) e
      | ty ->
          err (Format.asprintf "expected '%a', but got '%a'"
              Type.pp binding.ty Type.pp ty))
  | Seq l ->
      let aux e =
        let ty = typeof env e in
        if ty = Type.Instr then ()
        else
          err (Format.asprintf "expected instruction, but got '%a'" Type.pp ty)
      in
      List.iter aux l;
      Type.Instr
