open Cored

(* The command line: Driver.run decides what to print, write and exit with; this only does it. *)
let () =
  let read f = if Sys.file_exists f then Some (In_channel.with_open_bin f In_channel.input_all) else None in
  let o = Driver.run ~read (List.tl (Array.to_list Sys.argv)) in
  List.iter (fun (path, text) -> Out_channel.with_open_bin path (fun oc -> output_string oc text)) o.files ;
  print_string o.out ;
  prerr_string o.err ;
  exit o.code
