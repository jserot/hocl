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
open Interm
open Semval

type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable actor_box_style: string;
    mutable graph_box_style: string;
    mutable rec_box_style: string;
    mutable input_param_box_shape: string;
    mutable local_param_box_shape: string;
    mutable srcsnk_box_shape: string;
    mutable bcast_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_types: bool;
    mutable show_io_rates: bool;
    mutable rank_dir: string
  }

let cfg = {
  labeled_edges = true;
  show_indexes = false;
  actor_box_style = "rounded";
  graph_box_style = "solid";
  rec_box_style = "dashed";
  local_param_box_shape = "invhouse";
  input_param_box_shape = "cds";
  srcsnk_box_shape = "cds";
  bcast_box_shape = "octagon";
  slotted_boxes = false;
  show_wire_types = true;
  show_io_rates = true;
  rank_dir = "LR"
}

let output_box ch (i,b) =
  let slot_id pfx k = pfx ^ string_of_int k in
  let string_of_bio_annots anns = match cfg.show_io_rates, Backend.get_rate_expr anns with
    | true, Some rate -> "[" ^ Syntax.string_of_expr rate ^ "]"
    | _, _ -> "" in 
  let string_of_bio_slot pfx k (id,_,_,anns) = "<" ^ slot_id pfx k ^ ">" ^ id ^ string_of_bio_annots anns in
  let bid =
    if cfg.show_indexes
    then string_of_int i ^ ":" ^ b.b_model
    else b.b_model in
  let output_regular_box style =
       if cfg.slotted_boxes then
        fprintf ch "n%d [shape=record,style=%s,label=\"<id>%s|{{%s}|{%s}}\"];\n"
          i
          style
          bid
          (Misc.string_of_iarray (string_of_bio_slot "e") "|" b.b_ins)
          (Misc.string_of_iarray (string_of_bio_slot "s") "|" b.b_outs)
      else
        fprintf ch "n%d [shape=box,style=%s,label=\"%s\"];\n" i style bid in
  let shape_of_box tag = match tag with
    | BcastB -> cfg.bcast_box_shape
    | SourceB | SinkB -> cfg.srcsnk_box_shape
    | _ -> "box" in
  let param_lbl b =
    match Syntax.string_of_expr b.b_val.bv_sub with
    | "" -> b.b_model 
    | s -> s in
    (* match b.b_val.bv_val with
     * | SVUnit -> bid
     * | v -> bid ^ "=" ^ Semval.string_of_semval v in *)
  match b.b_tag with
  | ActorB -> output_regular_box cfg.actor_box_style 
  | GraphB -> output_regular_box cfg.graph_box_style
  | SourceB | SinkB | BcastB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\"];\n"
       i
       (shape_of_box b.b_tag)
       bid
  | RecB -> output_regular_box cfg.rec_box_style 
  | InParamB -> 
     fprintf ch "n%d [shape=%s,style=\"dashed\",label=\"%s\"];\n"
       i
       cfg.input_param_box_shape
       (param_lbl b)
  | LocalParamB -> 
     fprintf ch "n%d [shape=%s,style=\"dashed\",label=\"%s\"];\n"
       i
       cfg.local_param_box_shape
       (param_lbl b)

let output_wire ch (wid,(((s,ss,ty),(d,ds,_)))) =
  let style = if Types.is_param_type ty then "dashed" else "plain" in
  match cfg.labeled_edges, cfg.show_wire_types, cfg.show_indexes with
  | false, _, _
    | true, false, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [style=%s];\n" s ss d ds style
  | true, true, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\"%s\"; style=%s];\n" s ss d ds (Pr_type.string_of_type ty) style
  | true, false, true ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\"w%d\"; style=%s];\n" s ss d ds wid style
  | true, true, true ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\"w%d:%s\"; style=%s];\n" s ss d ds wid (Pr_type.string_of_type ty) style

let output_graph oc boxes wires = 
  fprintf oc "digraph g {\n";
  fprintf oc "rankdir=%s;\n" cfg.rank_dir;
  List.iter (output_box oc) boxes;
  List.iter (output_wire oc) wires;
  fprintf oc "}\n"

let dump_graph path pfx id g =
  let fname = path ^ pfx ^ "_" ^ id ^ ".dot" in 
  let oc = open_out fname  in
  output_graph oc g.sg_boxes g.sg_wires;
  Printf.printf "Wrote file %s\n" fname;
  close_out oc

let dump_node path pfx (id,n) =
  match n.sn_impl with
  | NI_Graph g -> dump_graph path pfx id g
  | NI_Actor _ -> ()

let dump path pfx ir =
  List.iter (dump_node path pfx) ir.ir_graphs;
  List.iter (dump_node path pfx) ir.ir_nodes
