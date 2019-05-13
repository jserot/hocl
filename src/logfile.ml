let fname = "hocl.output"
let channel = ref None

let stop () = match !channel with
  Some oc -> close_out oc
| None -> ()
  
let start () = 
  stop ();
  let oc = open_out fname in
  channel := Some oc

let write fname =
  (* Genmake.add_target fname; *)
  (* if not !Misc.generate_makefiles then begin *)
    Printf.printf "Wrote file %s\n" fname;
    match !channel with
      Some oc -> Printf.fprintf oc "%s\n" fname
    | None -> ()
    (* end *)
