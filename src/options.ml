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
  Printf.printf "This is the HoCL compiler, version %s\n" Version.version

type output_format = NoOutput | Dot | Systemc | Preesm | Xdf | Dif (* | Vhdl *)

type cfg = {
    mutable stdlib: string;
    mutable output_fmt: output_format;
    mutable output_prefix: string;
    mutable prefix: string;
    mutable dump_tenv: bool;
    mutable dump_senv: bool;
    mutable dump_typed: bool;
    mutable dump_ir: bool;
    mutable dump_boxes: bool;
    mutable target_dir: string
  }

let cfg = {
  stdlib = "";
  output_fmt = NoOutput;
  output_prefix = "";
  prefix = "";
  dump_tenv = false;
  dump_senv = false;
  dump_typed = false;
  dump_ir = false;
  dump_boxes = false;
  target_dir = ".";
  }

let set_stdlib path = cfg.stdlib <- path
let no_stdlib () = cfg.stdlib <- "none"
let set_output_prefix name = cfg.output_prefix <- name
(* let add_include_path path = Lexer.include_path <- !Lexer.include_path @ [path] *)
let set_prefix p = cfg.prefix <- p
let set_target_dir p = cfg.target_dir <- p
let do_dump_tenv () = cfg.dump_tenv <- true
let do_dump_senv () = cfg.dump_senv <- true
let do_dump_typed () = cfg.dump_typed <- true
let do_dump_ir () = cfg.dump_ir <- true
let do_dump_boxes () = cfg.dump_boxes <- true
let do_insert_bcasts () = Interm.cfg.Interm.insert_bcasts <- true
(* let do_insert_fifos () = Static.cfg.Static.insert_fifos <- true *)
(* DOT related options *)
let do_dot () = cfg.output_fmt <- Dot
let do_dot_unlabeled_edges () = Dot.cfg.Dot.labeled_edges <- false
let do_dot_show_indexes () = Dot.cfg.Dot.show_indexes <- true
let do_dot_wire_annots () = Dot.cfg.Dot.show_wire_annots <- true
let do_dot_no_io_rates () = Dot.cfg.Dot.show_io_rates <- false
let do_dot_slotted_boxes () = Dot.cfg.Dot.slotted_boxes <- true
let set_dot_rank_dir s = Dot.cfg.Dot.rank_dir <- s
(* SYSTEMC related options *)
let do_systemc () = begin cfg.output_fmt <- Systemc; Interm.cfg.Interm.insert_bcasts <- true end
let set_sc_stop_time n = Systemc.cfg.Systemc.sc_stop_time <- n
(* let set_sc_stop_idle_time n = Systemc.cfg.Systemc.sc_stop_idle_time <- n *)
let set_sc_clock_period n = Systemc.cfg.Systemc.sc_clock_period_ns <- n
let set_sc_fifo_capacity n = Systemc.cfg.Systemc.sc_data_fifo_capacity <- n
let set_sc_trace () = Systemc.cfg.Systemc.sc_trace <- true
let set_sc_trace_fifos () = Systemc.cfg.Systemc.sc_trace_fifos <- true
let set_sc_dump_fifos () = Systemc.cfg.Systemc.sc_dump_fifos <- true
let set_sc_dump_fifo_stats () = Systemc.cfg.Systemc.sc_dump_fifo_stats <- true
let set_sc_fifo_stats_file f = Systemc.cfg.Systemc.sc_fifo_stats_file <- f
(* PREESM related options *)
let do_preesm () = begin cfg.output_fmt <- Preesm; Interm.cfg.Interm.insert_bcasts <- true end
let set_preesm_name n = Preesm.cfg.Preesm.top_name <- n
(* XDF related options *)
let do_xdf () = cfg.output_fmt <- Xdf
let set_xdf_package p = Xdf.cfg.Xdf.target_package <- p
(* DIF related options *)
let do_dif () = cfg.output_fmt <- Dif
(* VHDL related options *)
(* let do_vhdl () = cfg.output_fmt <- Vhdl *)

let options_spec = [
 "-stdlib", Arg.String (set_stdlib), "set location of the standard library file (default: " ^ Version.stdlib ^ ")";
 "-no_stdlib", Arg.Unit (no_stdlib), "do not use the standard library";
"-prefix", Arg.String (set_output_prefix), "set prefix output file names (default is main source file basename)";
(* "-I", Arg.String (add_include_path), "add path to the list of dirs to search for include"; *)
"-target_dir", Arg.String (set_target_dir), "set target directory for generated files (default is current directory)";
"-dump_tenv", Arg.Unit (do_dump_tenv), "dump builtin typing environment (for debug only)";
"-dump_typed", Arg.Unit (do_dump_typed), "dump typed program (for debug only)";
"-dump_senv", Arg.Unit (do_dump_senv), "dump builtin static environment (for debug only)";
"-dump_ir", Arg.Unit (do_dump_ir), "dump intermediate representation (for debug only)";
"-dump_boxes", Arg.Unit (do_dump_boxes), "dump static representation of boxes";
"-insert_bcasts", Arg.Unit (do_insert_bcasts), "insert broadcast boxes";
"-version", Arg.Unit (print_version), "print version of the compiler";
"--v", Arg.Unit (print_version), "print version of the compiler";
(* "-insert_fifos", Arg.Unit (do_insert_fifos), "insert fifos between actors"; *)
"-dot", Arg.Unit (do_dot), "generate .dot representation of the program";
"-dot_rank_dir", Arg.String (set_dot_rank_dir), "set rank direction for DOT output graph (default: LR)";
"-dot_unlabeled_edges", Arg.Unit (do_dot_unlabeled_edges), "do not annotate graph edges";
(* "-dot_wire_annots", Arg.Unit (do_dot_wire_annots), "print wire annotations (phase/fifo_size) when available (implies [-dot_show_indexes])"; *)
"-dot_no_io_rates", Arg.Unit (do_dot_no_io_rates), "do not annotate ports with resp. rates";
"-dot_show_indexes", Arg.Unit (do_dot_show_indexes), "print box and wire indexes";
"-dot_slotted_boxes", Arg.Unit (do_dot_slotted_boxes), "print boxes with i/o slots";
"-systemc", Arg.Unit (do_systemc), "activate the SystemC backend";
"-sc_stop_time", Arg.Int (set_sc_stop_time), "stop after n ns";
(* "-sc_stop_when_idle", Arg.Int (set_sc_stop_idle_time), "stop when outputs have been inactive for n ns"; *)
"-sc_clock_period", Arg.Int (set_sc_clock_period), "set clock period (ns) (default: 10)";
"-sc_default_fifo_capacity", Arg.Int (set_sc_fifo_capacity), "set default fifo capacity (systemc only) (default: 256)";
"-sc_trace", Arg.Unit (set_sc_trace), "set trace mode";
"-sc_dump_fifos", Arg.Unit (set_sc_dump_fifos), "dump fifo contents";
"-sc_trace_fifos", Arg.Unit (set_sc_trace_fifos), "trace fifo usage in .vcd file";
"-sc_dump_fifo_stats", Arg.Unit (set_sc_dump_fifo_stats), "dump fifo usage statistics after run";
"-sc_fifo_stats_file", Arg.String (set_sc_fifo_stats_file), "set file for dumping fifo statistics (default: fifo_stats.dat)";
"-preesm", Arg.Unit (do_preesm), "activate the Preesm backend";
(* "-preesm_top_name", Arg.String (set_preesm_name), "set top level name for Preesm graph (default: base name of input file"; *)
"-xdf", Arg.Unit (do_xdf), "generate .xdf representation of the network";
"-xdf_package", Arg.String (set_xdf_package), "set package name for the generated XDF code";
"-dif", Arg.Unit (do_dif), "generate .dif representation of the program";
(* "-vhdl", Arg.Unit (do_vhdl), "activate the VHDL backend"; *)
];
