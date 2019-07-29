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

open Printf
   
let source_file = ref ""
                
let anonymous fname = source_file := fname

let usage = "usage: pi2hcl [options...] file"

type options = {
  mutable dot_output: bool;
  }

let options = {
  dot_output = false;
  }

let options_spec = [
    "-dot", Arg.Unit (fun () -> options.dot_output <- true), "dump .dot representation";
    "-names", Arg.String (fun f -> Hcl.cfg.Hcl.annot_file <- f), "set name annotation file";
    (* "-incl_dir", Arg.String (fun d -> Hcl.cfg.Hcl.incl_dir <- d), "set include dir for code pragmas (default: ./include)"; *)
  ]

let main () =
try
  Sys.catch_break true;
  Arg.parse options_spec anonymous usage;
  let p = Ir.from_file !source_file in
  if options.dot_output then Dot.dump (Misc.replace_suffix "dot" !source_file) p;
  Hcl.dump Hcl.cfg.Hcl.annot_file (Misc.replace_suffix "hcl" !source_file) p
with
| Sys.Break -> flush stderr; exit 5
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg;
    flush stderr; exit 6
| Ir.CyclicGraph n ->
   eprintf "Not implemented: translation of cyclic graphs (cycle here starts at node %s).\n" n;
   exit 4
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 7

let _ = Printexc.print main ()
