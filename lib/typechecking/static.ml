open Common
open Syntax
open Type
open Typed_ast
module Env = Map.Make (String)

let err msg = Error.(error (Ty (IncompatibleType msg)))

let rec typecheck e = ignore (typeof Env.empty e)

and ty_of_value env = function
  | Arg _ -> Type.Arg
  | Var x -> Env.find x env

and typeof env e =
  match e with
  | Value v -> ty_of_value env v
  | Label _ -> Type.Instr
  | Lam (x, ty, e) ->
      let env' = Env.add x ty env in
      let ty' = typeof env' e in
      Arrow (ty, ty')
  | App (e1, e2) -> (
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      match ty1 with
      | Arrow (a, b) when a = ty2 -> b
      | Arrow (ty11, _) ->
          err
            (Format.asprintf "expected argument of type '%a', but got '%a'"
               Type.pp ty11 Type.pp ty2)
      | _ ->
          err
            (Format.asprintf "expected function type, but got '%a'" Type.pp ty1)
      )
  | Prim1 (op, e1) -> (
      let ty1 = typeof env e1 in
      let a, b = ty_of_op1 op in
      if ty1 = a then b
      else
        err
          (Format.asprintf "expected argument of type '%a', but got '%a'"
             Type.pp a Type.pp ty1))
  | Prim2 (op, e1, e2) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      let a, b, c = ty_of_op2 op in
      if ty1 = a && ty2 = b then c
      else
        err
          (Format.asprintf
             "expected arguments of types '%a' and '%a', but got '%a' and '%a'"
             Type.pp a Type.pp b Type.pp ty1 Type.pp ty2)
  | Prim3 (op, e1, e2, e3) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      let ty3 = typeof env e3 in
      let a, b, c, d = ty_of_op3 op in
      if ty1 = a && ty2 = b && ty3 = c then d
      else
        err
          (Format.asprintf
             "expected arguments of types '%a', '%a' and '%a', but got '%a', \
              '%a' and '%a'"
             Type.pp a Type.pp b Type.pp c Type.pp ty1 Type.pp ty2 Type.pp
             ty3)
  | Let (binding, e) -> (
      match typeof env binding.e with
      | ty when ty = binding.ty -> typeof (Env.add binding.name ty env) e
      | ty ->
          err
            (Format.asprintf "expected '%a', but got '%a'" Type.pp binding.ty
               Type.pp ty))
  | Tuple _ -> Type.Instr
  | Ascribe (e, ty') -> (
      match typeof env e with
      | ty when ty = ty' -> ty
      | ty ->
          err
            (Format.asprintf
               "expected expression of type '%a', but got expression of type \
                '%a'"
               Type.pp ty' Type.pp ty))
