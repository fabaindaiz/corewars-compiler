type t = {
  typechecking : typechecking;
  verbose : bool;
  input : input;
  output : output;
}

and typechecking =
  | Static
  | Dynamic
  | Nocheck

and input =
  | Stdin
  | InputFile of string
  | InputString of string

and output =
  | Stderr
  | OutputFile of string
