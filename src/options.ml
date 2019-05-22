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

let print_version () =
  Printf.printf "This is the HOCL compiler, version %s\n" Version.version

type output_format = NoOutput | Dot | Preesm | Systemc | Xdf (* | Dif | Vhdl *)

type cfg = {
    mutable prelude: string;
    mutable output_fmt: output_format;
    mutable output_prefix: string;
    mutable prefix: string;
    mutable dump_tenv: bool;
    mutable dump_senv: bool;
    mutable dump_typed: bool;
    mutable dump_static: bool;
    mutable dump_boxes: bool;
    mutable target_dir: string
  }

let cfg = {
  prelude = "";
  output_fmt = NoOutput;
  output_prefix = "";
  prefix = "";
  dump_tenv = false;
  dump_senv = false;
  dump_typed = false;
  dump_static = false;
  dump_boxes = false;
  target_dir = ".";
  }

let set_prelude name = cfg.prelude <- name
let set_output_prefix name = cfg.output_prefix <- name
(* let add_include_path path = Lexer.include_path <- !Lexer.include_path @ [path] *)
let set_prefix p = cfg.prefix <- p
let set_target_dir p = cfg.target_dir <- p
let do_dump_tenv () = cfg.dump_tenv <- true
let do_dump_senv () = cfg.dump_senv <- true
let do_dump_typed () = cfg.dump_typed <- true
let do_dump_static () = cfg.dump_static <- true
let do_dump_boxes () = cfg.dump_boxes <- true
let do_insert_bcasts () = Static.cfg.Static.insert_bcasts <- true
(* let do_insert_fifos () = Static.cfg.Static.insert_fifos <- true *)
let do_dot () = cfg.output_fmt <- Dot
let do_preesm () = cfg.output_fmt <- Preesm
let do_systemc () = cfg.output_fmt <- Systemc
let do_xdf () = cfg.output_fmt <- Xdf
(* let do_dif () = output_fmt := Dif
 * let do_vhdl () = output_fmt := Vhdl *)
let do_dot_unlabeled_edges () = Dot.cfg.Dot.labeled_edges <- false
let do_dot_show_indexes () = Dot.cfg.Dot.show_indexes <- true
let do_dot_wire_annots () = Dot.cfg.Dot.show_wire_annots <- true
let do_dot_simple_boxes () = Dot.cfg.Dot.slotted_boxes <- false
let set_dot_rank_dir s = Dot.cfg.Dot.rank_dir <- s
let do_phantom_types () = () (* Pr_type.print_type_repr := true  *)
(* PREESM related options *)
let set_preesm_name n = Preesm.cfg.Preesm.top_name <- n
(* SYSTEMC related options *)
let set_sc_stop_time n = Systemc.cfg.Systemc.sc_stop_time <- n
(* let set_sc_stop_idle_time n = Systemc.cfg.Systemc.sc_stop_idle_time <- n *)
let set_sc_clock_period n = Systemc.cfg.Systemc.sc_clock_period_ns <- n
let set_sc_fifo_capacity n = Systemc.cfg.Systemc.sc_fifo_capacity <- n
let set_sc_trace () = Systemc.cfg.Systemc.sc_trace <- true
(* let set_sc_trace_fifos () = Systemc.cfg.Systemc.sc_trace_fifos <- true
 * let set_sc_dump_fifos () = Systemc.cfg.Systemc.sc_dump_fifos <- true
 * let set_sc_dump_fifo_stats () = Systemc.cfg.Systemc.sc_dump_fifo_stats <- true
 * let set_sc_fifo_stats_file f = Systemc.cfg.Systemc.sc_fifo_stats_file <- f *)
(* XDF related options *)
let set_xdf_package p = Xdf.cfg.Xdf.target_package <- p

let options_spec = [
"-prelude", Arg.String (set_prelude), "set location of the standard prelude file";
"-prefix", Arg.String (set_output_prefix), "set prefix output file names (default is main source file basename)";
(* "-I", Arg.String (add_include_path), "add path to the list of dirs to search for include"; *)
"-target_dir", Arg.String (set_target_dir), "set target directory for generated files (default is current directory)";
"-dump_tenv", Arg.Unit (do_dump_tenv), "dump builtin typing environment (for debug only)";
"-dump_senv", Arg.Unit (do_dump_senv), "dump builtin static environment (for debug only)";
"-dump_typed", Arg.Unit (do_dump_typed), "dump typed program (for debug only)";
"-dump_static", Arg.Unit (do_dump_static), "dump static representation (for debug only)";
"-phantom_types", Arg.Unit (do_phantom_types), "print sized types using underlying representation (not for the casual user)";
"-dump_boxes", Arg.Unit (do_dump_boxes), "dump static representation of boxes";
"-insert_bcasts", Arg.Unit (do_insert_bcasts), "insert broadcast boxes";
(* "-insert_fifos", Arg.Unit (do_insert_fifos), "insert fifos between actors"; *)
"-dot", Arg.Unit (do_dot), "generate .dot representation of the program";
"-preesm", Arg.Unit (do_preesm), "activate the Preesm backend";
"-systemc", Arg.Unit (do_systemc), "activate the SystemC backend";
"-xdf", Arg.Unit (do_xdf), "generate .xdf representation of the network";
(* "-dif", Arg.Unit (do_dif), "generate .dif representation of the program";
 * "-vhdl", Arg.Unit (do_vhdl), "activate the VHDL backend"; *)
"-version", Arg.Unit (print_version), "print version of the compiler";
"--v", Arg.Unit (print_version), "print version of the compiler";
"-dot_rank_dir", Arg.String (set_dot_rank_dir), "set rank direction for DOT output graph (default: LR)";
"-dot_unlabeled_edges", Arg.Unit (do_dot_unlabeled_edges), "do not annotate graph edges";
"-dot_wire_annots", Arg.Unit (do_dot_wire_annots), "print wire annotations (phase/fifo_size) when available (implies [-dot_show_indexes])";
"-dot_show_indexes", Arg.Unit (do_dot_show_indexes), "print box and wire indexes";
"-dot_simple_boxes", Arg.Unit (do_dot_simple_boxes), "print boxes without i/o slots";
"-preesm_top_name", Arg.String (set_preesm_name), "set top level name for Preesm graph (default: base name of input file";
"-sc_stop_time", Arg.Int (set_sc_stop_time), "stop after n ns";
(* "-sc_stop_when_idle", Arg.Int (set_sc_stop_idle_time), "stop when outputs have been inactive for n ns"; *)
"-sc_clock_period", Arg.Int (set_sc_clock_period), "set clock period (ns) (default: 10)";
"-sc_default_fifo_capacity", Arg.Int (set_sc_fifo_capacity), "set default fifo capacity (systemc only) (default: 256)";
"-sc_trace", Arg.Unit (set_sc_trace), "set trace mode";
(* "-sc_dump_fifos", Arg.Unit (set_sc_dump_fifos), "dump fifo contents";
 * "-sc_trace_fifos", Arg.Unit (set_sc_trace_fifos), "trace fifo usage in .vcd file";
 * "-sc_dump_fifo_stats", Arg.Unit (set_sc_dump_fifo_stats), "dump fifo usage statistics after run";
 * "-sc_fifo_stats_file", Arg.String (set_sc_fifo_stats_file), "set file for dumping fifo statistics (default: fifo_stats.dat)"; *)
"-xdf_package", Arg.String (set_xdf_package), "set package name for the generated XDF code";
];
