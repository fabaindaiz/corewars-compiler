open Cored

(* The command line: Driver.run decides what to print, write and exit with; this only does it. *)
let () =
  (* a directory or an unreadable file is no file: the caller's error says which *)
  let read f = try Some (In_channel.with_open_bin f In_channel.input_all) with Sys_error _ -> None in
  let o = Driver.run ~read (List.tl (Array.to_list Sys.argv)) in
  List.iter (fun (path, text) -> Out_channel.with_open_bin path (fun oc -> output_string oc text)) o.files ;
  print_string o.out ;
  prerr_string o.err ;
  exit o.code
