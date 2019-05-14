type output_format = NoOutput | Dot (* | Xdf | Dif | Systemc | Vhdl *)

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
let do_dot () = cfg.output_fmt <- Dot
(* let do_xdf () = output_fmt := Xdf
 * let do_dif () = output_fmt := Dif
 * let do_systemc () = output_fmt := Systemc
 * let do_vhdl () = output_fmt := Vhdl *)
let do_dot_unlabeled_edges () = Dot.cfg.Dot.labeled_edges <- false
let do_dot_show_indexes () = Dot.cfg.Dot.show_indexes <- true
let do_dot_wire_annots () = Dot.cfg.Dot.show_wire_annots <- true
let do_dot_simple_boxes () = Dot.cfg.Dot.slotted_boxes <- false
let do_phantom_types () = () (* Pr_type.print_type_repr := true  *)
(* XDF related options *)
(* let set_xdf_package p = Xdf.cfg.Xdf.target_package <- p *)

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
"-dot", Arg.Unit (do_dot), "generate .dot representation of the program";
(* "-xdf", Arg.Unit (do_xdf), "generate .xdf representation of the network and .cal descriptions of the actors";
 * "-dif", Arg.Unit (do_dif), "generate .dif representation of the program";
 * "-systemc", Arg.Unit (do_systemc), "activate the SystemC backend";
 * "-vhdl", Arg.Unit (do_vhdl), "activate the VHDL backend"; *)
(* "-version", Arg.Unit (print_version), "print version of the compiler";
 * "--v", Arg.Unit (print_version), "print version of the compiler"; *)
"-dot_unlabeled_edges", Arg.Unit (do_dot_unlabeled_edges), "do not annotate graph edges";
"-dot_wire_annots", Arg.Unit (do_dot_wire_annots), "print wire annotations (phase/fifo_size) when available (implies [-dot_show_indexes])";
"-dot_show_indexes", Arg.Unit (do_dot_show_indexes), "print box and wire indexes";
"-dot_simple_boxes", Arg.Unit (do_dot_simple_boxes), "print boxes without i/o slots";
(* "-xdf_package", Arg.String (set_xdf_package), "set package name for the generated XDF code"; *)
];
