(**********************************************************************)
(*                                                                    *)
(*              This file is part of the HOCL package                 *)
(*                                                                    *)
(*  Copyright (c) 2019-present, Jocelyn SEROT (jocelyn.serot@uca.fr). *)
(*                     All rights reserved.                           *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

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
