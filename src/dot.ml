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
open Static

type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable actor_box_style: string;
    mutable graph_box_style: string;
    mutable input_param_box_shape: string;
    mutable local_param_box_shape: string;
    mutable srcsnk_box_shape: string;
    mutable bcast_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_annots: bool;
    mutable show_io_rates: bool;
    mutable rank_dir: string
  }

let cfg = {
  labeled_edges = true;
  show_indexes = false;
  actor_box_style = "rounded";
  graph_box_style = "solid";
  local_param_box_shape = "invhouse";
  input_param_box_shape = "cds";
  srcsnk_box_shape = "cds";
  bcast_box_shape = "octagon";
  slotted_boxes = false;
  show_wire_annots = false;
  show_io_rates = true;
  rank_dir = "LR"
}

(* and string_of_val v =
 *   let clip s = if String.length s > !max_array_disp_size then "..." else s in
 *   match v with  (\* Special version for DOT since node labels do not support any chars :( *\)
 *     Expr.Val_int (i,_) -> string_of_int i
 *   | Expr.Val_bool b -> string_of_bool b
 *   | Expr.Val_float b -> string_of_float b
 *   | Expr.Val_array1 (n,vs) -> clip (Array1.to_string string_of_val vs)
 *   | Expr.Val_array2 (n,vs) -> clip (Array2.to_string string_of_val vs)
 *   | Expr.Val_array3 (n,vs) -> clip (Array3.to_string string_of_val vs)
 *   | Expr.Val_prim _
 *   | Expr.Val_fun (_,_,_)
 *   | Expr.Val_extern _ -> "(fun)"
 *   | _ -> "?" *)

let output_box ch (i,b) =
  let slot_id pfx k = pfx ^ string_of_int k in
  let string_of_bio_annots anns = match cfg.show_io_rates, anns with
    | true, [Syntax.IA_Rate e] -> "[" ^ Syntax.string_of_rate_expr e ^ "]"
    | _, _ -> "" in (* TO FIX ? *)
  let string_of_bio_slot pfx k (id,(_,ty,anns)) = "<" ^ slot_id pfx k ^ ">" ^ id ^ string_of_bio_annots anns in
  let bid =
    if cfg.show_indexes
    then string_of_int i ^ ":" ^ b.b_name
    else b.b_name in
  let bval = 
    let s1 = Syntax.string_of_core_expr b.b_val.bv_lit in
    let s2 = Ssval.string_of_ssval b.b_val.bv_val in
    if b.b_val.bv_lit.ce_desc = Syntax.EVar "" && b.b_val.bv_val=Ssval.SVUnit && s1 <> s2
    then s1 ^ "=" ^ s2
    else s1 in
  let output_regular_box style =
       if cfg.slotted_boxes then
        fprintf ch "n%d [shape=record,style=%s,label=\"<id>%s|{{%s}|{%s}}\"];\n"
          i
          style
          bid
          (Misc.string_of_ilist (string_of_bio_slot "e") "|" b.b_ins)
          (Misc.string_of_ilist (string_of_bio_slot "s") "|" b.b_outs)
          (* (ioslots "e" (List.length b.b_ins))
           * (ioslots "s" (List.length b.b_outs)) *)
      else
        fprintf ch "n%d [shape=box,style=%s,label=\"%s\"];\n" i style bid in
  let shape_of_box tag = match tag with
    | EBcastB | IBcastB -> cfg.bcast_box_shape
    | SourceB | SinkB -> cfg.srcsnk_box_shape
    | _ -> "box" in
  match b.b_tag with
  | ActorB -> output_regular_box cfg.actor_box_style 
  | GraphB -> output_regular_box cfg.graph_box_style 
  | EBcastB
  | IBcastB
  (* | DelayB -> *)
  | SourceB | SinkB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\"];\n"
       i
       (shape_of_box b.b_tag)
       bid
  | LocalParamB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\"];\n"
       i
       cfg.local_param_box_shape
       bval
  | InParamB -> 
     let lbl =
       begin match b.b_val.bv_val with
       | SVUnit -> bid
       | v -> bid ^ "=" ^ Ssval.string_of_ssval v
       end in
     fprintf ch "n%d [shape=%s,style=\"dashed\",label=\"%s\"];\n"
       i
       cfg.input_param_box_shape
       lbl
  | DummyB ->  (* Should not occur *)
      fprintf ch "n%d [shape=box,style=dotted,label=\"%s\"];\n" i "dummy"

let string_of_wtype ty = ":" ^ Pr_type.string_of_type ty

let output_wire ch (wid,(((s,ss),(d,ds)),ty,kind))=
  let wire_annot wid =
    (* try Interm.string_of_wire_annot (List.assoc wid wire_annots)
    with Not_found -> *) "" in
  let style = match kind with Ssval.ParamW -> "dashed" | _ -> "plain" in
  match cfg.labeled_edges, cfg.show_indexes, cfg.show_wire_annots with
  | true, _, true ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" w%d:%s\"; style=%s];\n" s ss d ds wid (wire_annot wid) style
  | true, true, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds ("w" ^ string_of_int wid ^ string_of_wtype ty) style
  | true, false, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds (string_of_wtype ty) style
  | false, _, _ ->
     fprintf ch "n%d:s%d -> n%d:e%d [style=%s];\n" s ss d ds style

let output_graph oc boxes wires = 
  fprintf oc "digraph g {\n";
  fprintf oc "rankdir=%s;\n" cfg.rank_dir;
  List.iter (output_box oc) boxes;
  List.iter (output_wire oc) wires;
  fprintf oc "}\n"

let dump_graph path pfx (id,gd) =
  let fname = path ^ pfx ^ "_" ^ id ^ ".dot" in 
  let oc = open_out fname  in
  output_graph oc gd.tg_impl.sg_boxes gd.tg_impl.sg_wires;
  Logfile.write fname;
  close_out oc

let dump path pfx sp =
  List.iter (dump_graph path pfx) sp.sp_graphs
