exception Error of string

module Config : sig
  type t = private {
    name : string;
    assertions : bool;
    tagged : bool;
  }

  val init : string -> bool -> bool -> unit
  val get : unit -> t
end = struct
  type t = {
    name : string;
    assertions : bool;
    tagged : bool;
  }

  let config = ref None

  let init name assertions tagged =
    if assertions && not tagged then
      failwith "tried to enable assertions without tags";
    config := Some { name; assertions; tagged }

  let get () =
    match !config with
    | Some conf -> conf
    | None -> failwith "tried to use config before it has been initialized"
end