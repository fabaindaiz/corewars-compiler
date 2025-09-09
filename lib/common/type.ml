open Printf
open Env

exception TypeError of string

type const =
  | Unit
  | Bool of bool
  | Int of int

let string_of_const =
  function
  | Unit -> "unit"
  | Bool b -> Bool.to_string b
  | Int n -> Int.to_string n

type tbase =
  | TUnit
  | TBool
  | TInt

type ttype =
  | TBase of tbase
  | TArrow of ttype *  ttype

let string_of_tbase =
  function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TInt -> "Int"

let rec string_of_ttype =
  function
  | TBase t -> string_of_tbase t
  | TArrow (t1, t2) -> sprintf "(%s -> %s)" (string_of_ttype t1) (string_of_ttype t2)

let const_typing =
  function
  | Unit -> TUnit
  | Bool _ -> TBool
  | Int _ -> TInt

type tenv = ttype env

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else raise (TypeError "Type mismatch")