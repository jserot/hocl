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

(* DOT backend *)

open Printf
   
type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable local_param_box_shape: string;
    mutable input_param_box_shape: string;
    mutable srcsnk_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_annots: bool;
    mutable rank_dir: string
  }

let cfg = {
  labeled_edges = true;
  show_indexes = false;
  local_param_box_shape = "invhouse";
  input_param_box_shape = "invtriangle";
  srcsnk_box_shape = "cds";
  slotted_boxes = true;
  show_wire_annots = false;
  rank_dir = "LR"
}

let output_box ch b =
  let open Ir in
  let is_input io = io.io_direction = "IN" in
  let is_output io = io.io_direction = "OUT" in
  let ioslots ios = Misc.string_of_list (function io -> "<" ^ io.io_name ^ ">" ^ io.io_name) "|" ios in
  match b.n_kind with
  | "actor" ->
      if cfg.slotted_boxes then
        fprintf ch "%s [shape=record,style=rounded,label=\"<id>%s|{{%s}|{%s}}\"];\n"
          b.n_name
          b.n_name
          (ioslots (List.filter is_input b.n_ios))
          (ioslots (List.filter is_output b.n_ios))
      else
        fprintf ch "%s [shape=box,style=rounded,label=\"%s\"];\n"  b.n_name b.n_name
  (* | GraphB ->
   *       fprintf ch "n%d [shape=box,label=\"%s\"];\n" i  bid *)
  | "src" | "snk" -> 
     fprintf ch "%s [shape=%s,label=\"%s\"];\n"
       b.n_name
       cfg.srcsnk_box_shape
       b.n_name
  | "param" -> 
     fprintf ch "%s [shape=%s,label=\"%s\n(%s)\"];\n"
       b.n_name
       cfg.local_param_box_shape
       b.n_name
       b.n_desc
  | "cfg_in_iface" -> 
     fprintf ch "%s [shape=%s,label=\"%s\"];\n"
       b.n_name
       cfg.input_param_box_shape
       b.n_name
  | _ -> ()

let output_wire ch w =
  let open Ir in
  let style = match w.e_kind with "dependency" -> "dashed" | _ -> "plain" in
  let slot s p = match p with "" -> s | _ -> s ^ ":" ^ p in
  match cfg.labeled_edges with
  | true ->
     fprintf ch "%s -> %s [label=\"%s\"; style=%s];\n" (slot w.e_source w.e_sourceport) (slot w.e_target w.e_targetport) w.e_type style
  | false ->
     fprintf ch "%s -> %s [style=%s];\n" (slot w.e_source w.e_sourceport) (slot w.e_target w.e_targetport) style

let output ch boxes wires = 
  fprintf ch "digraph g {\n";
  fprintf ch "rankdir=%s;\n" cfg.rank_dir;
  List.iter (output_box ch) boxes;
  List.iter (output_wire ch) wires;
  fprintf ch "}\n"

let dump fname p =
  let open Ir in
  let oc = open_out fname  in
  output oc p.p_nodes p.p_edges;
  close_out oc;
  printf "Generated file %s\n" fname
