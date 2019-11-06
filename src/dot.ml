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
open Ssval
open Static

type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable local_param_box_shape: string;
    mutable input_param_box_shape: string;
    mutable srcsnk_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_annots: bool;
    mutable show_io_rates: bool;
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
  let string_of_bio_rate r = match cfg.show_io_rates, r with
    | false, _ -> ""
    | true, None -> ""
    | true, Some e -> "[" ^ Syntax.string_of_rate_expr e ^ "]" in
  let string_of_bio_slot pfx k (id,(_,ty,rate,ann)) = "<" ^ slot_id pfx k ^ ">" ^ id ^ string_of_bio_rate rate in
  let bid =
    if cfg.show_indexes
    then string_of_int i ^ ":" ^ b.b_name
    else b.b_name in
  let bval = 
    let s1 = Syntax.string_of_core_expr b.b_val.bv_lit in
    let s2 = string_of_ssval b.b_val.bv_val in
    if s1 = s2 then s1 else s1 ^ "=" ^ s2 in
  match b.b_tag with
  | ActorB
  | BcastB
  | DelayB ->
      if cfg.slotted_boxes then
        fprintf ch "n%d [shape=record,style=rounded,label=\"<id>%s|{{%s}|{%s}}\"];\n"
          i
          bid
          (Misc.string_of_ilist (string_of_bio_slot "e") "|" b.b_ins)
          (Misc.string_of_ilist (string_of_bio_slot "s") "|" b.b_outs)
          (* (ioslots "e" (List.length b.b_ins))
           * (ioslots "s" (List.length b.b_outs)) *)
      else
        fprintf ch "n%d [shape=box,style=rounded,label=\"%s\"];\n" i  bid
  | GraphB ->
        fprintf ch "n%d [shape=box,label=\"%s\"];\n" i  bid
  | SourceB | SinkB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\"];\n"
       i
       cfg.srcsnk_box_shape
       bid
  | LocalParamB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\n(%s)\"];\n"
       i
       cfg.local_param_box_shape
       bid
       bval
  | InParamB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\"];\n"
       i
       cfg.input_param_box_shape
       bid
  | DummyB ->  (* Should not occur *)
      fprintf ch "n%d [shape=box,style=dotted,label=\"%s\"];\n" i "dummy"

let string_of_wtype ty = ":" ^ Pr_type.string_of_type ty

let output_wire ch (wid,(((s,ss),(d,ds)),ty,kind))=
  let wire_annot wid =
    (* try Interm.string_of_wire_annot (List.assoc wid wire_annots)
    with Not_found -> *) "" in
  let style = match kind with ParamW -> "dashed" | _ -> "plain" in
  match cfg.labeled_edges, cfg.show_indexes, cfg.show_wire_annots with
  | true, _, true ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" w%d:%s\"; style=%s];\n" s ss d ds wid (wire_annot wid) style
  | true, true, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds ("w" ^ string_of_int wid ^ string_of_wtype ty) style
  | true, false, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds (string_of_wtype ty) style
  | false, _, _ ->
     fprintf ch "n%d:s%d -> n%d:e%d [style=%s];\n" s ss d ds style

let output ch boxes wires = 
  fprintf ch "digraph g {\n";
  fprintf ch "rankdir=%s;\n" cfg.rank_dir;
  List.iter (output_box ch) boxes;
  List.iter (output_wire ch) wires;
  fprintf ch "}\n"

let dump fname sp =
    let oc = open_out fname  in
    output oc sp.boxes sp.wires;
    Logfile.write fname;
    close_out oc
