(** Hill: the settings a warrior is written for **)

type t = { key : string; coresize : int; length : int; pspace : bool }

(* As the hills publish them: 94nop, 94 and 94x from koth.org's table (2026-10-04); 94b from SAL's
   beginners' settings (pmars/config/94b.opt); tiny and nano as Koenigstuhl scores its archives of
   them (SAL was unreachable that day). Only 94nop says it disallows p-space. *)
let all = [
  { key = "94b"; coresize = 8000; length = 100; pspace = true };
  { key = "94nop"; coresize = 8000; length = 100; pspace = false };
  { key = "94"; coresize = 8000; length = 100; pspace = true };
  { key = "94x"; coresize = 55440; length = 200; pspace = true };
  { key = "tiny"; coresize = 800; length = 20; pspace = true };
  { key = "nano"; coresize = 80; length = 5; pspace = true };
]

let default = List.hd all

let find (key : string) : t option = List.find_opt (fun h -> h.key = key) all

let keys : string = String.concat ", " (List.map (fun h -> h.key) all)
