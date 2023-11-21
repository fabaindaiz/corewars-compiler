type 'a t = { desc : 'a desc; meta : 'a }

and 'a desc =
  | Arg of Arg.t
  | Var of string
  | Label of string
  | Prim1 of Prim.op1 * 'a t
  | Prim2 of Prim.op2 * 'a t * 'a t
  | Prim3 of Prim.op3 * 'a t * 'a t * 'a t
  | Let of 'a binding * 'a t
  | Seq of 'a t list

and 'a binding = { name : string; term : 'a t }

[@@deriving show { with_path = false }]

let rec pp fmt e = pp_desc fmt e.desc

and pp_desc fmt =
  let open Format in
  function
  | Arg arg -> fprintf fmt "%s" (Arg.to_string arg)
  | Var x -> fprintf fmt "%s" x
  | Label l -> fprintf fmt "%s" l
  | Prim1 (op1, e1) -> fprintf fmt "@[<hov 2>%a@ %a@]" Prim.pp_op1 op1 pp e1
  | Prim2 (op2, e1, e2) ->
      fprintf fmt "@[<hov 2>%a@ %a@ %a@]" Prim.pp_op2 op2 pp e1 pp e2
  | Prim3 (op3, e1, e2, e3) ->
      fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]" Prim.pp_op3 op3 pp e1 pp e2 pp e3
  | Let (binding, e) ->
      fprintf fmt "@[<hov 0>let %s =@ %a@ in@.%a@]" binding.name pp binding.term
        pp e
  | Seq exprs ->
      fprintf fmt "@[<hov 2>(%a)@]" (Common.Pp.pp_list ",@ " pp) exprs

and pp_binding fmt binding =
  Format.fprintf fmt "@[<hov 2>%s =@ %a@]" binding.name pp binding.term

let rec map (f : 'a -> 'b) (t : 'a t) : 'b t =
  let bind desc = { desc; meta = f t.meta } in
  match t.desc with
  | Arg arg -> bind (Arg arg)
  | Var x -> bind (Var x)
  | Label l -> bind (Label l)
  | Prim1 (op1, e1) -> bind (Prim1 (op1, map f e1))
  | Prim2 (op2, e1, e2) -> bind (Prim2 (op2, map f e1, map f e2))
  | Prim3 (op3, e1, e2, e3) -> bind (Prim3 (op3, map f e1, map f e2, map f e3))
  | Let (binding, e) ->
      bind (Let ({ binding with term = map f binding.term }, map f e))
  | Seq exprs -> bind (Seq (List.map (map f) exprs))
