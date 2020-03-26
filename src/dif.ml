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

(* DIF backend *)

open Printf
open Interm
open Semval
open Backend

type dif_config = {
  node_prefix: string;
  edge_prefix: string;
  (* src_ifile: string;
   * snk_ofile: string; *)
  }

let cfg = {
  node_prefix = "n";
  edge_prefix = "e";
  (* src_ifile = "ifile";
   * snk_ofile = "ofile"; *)
}

let node_id i = cfg.node_prefix ^ string_of_int i
let edge_id i = cfg.edge_prefix ^ string_of_int i

let string_of_val v = match v with  
    SVInt n -> string_of_int n
  | SVBool b -> string_of_bool b
  | _ -> Misc.not_implemented ("DIF translation of value " ^ Semval.string_of_semval v) 

let string_of_node (i,b) = node_id i 

let string_of_edge (wid,(((s,ss),(d,ds)),ty(*,kind*))) =
  edge_id wid ^ "(" ^ node_id s ^ "," ^ node_id d ^ ")"
(* Q : how are I/O slot indexes handled in DIF ?? *)
(*   sprintf "e%d(n%d:s%d -> n%d:e%d)" s ss d ds *)

(* let dump_parameter oc (name, gc) = 
 *   fprintf oc "    %s = %s;\n" name (string_of_val gc.gc_val) *)

let dump_io_decl oc g (i,b) = 
  match b.b_tag with 
    SourceB ->
      let outp = "o" in
      fprintf oc "  actortype %s {\n" b.b_name;
      fprintf oc "    output %s;\n" outp;
      (* fprintf oc "    param %s;\n" cfg.src_ifile; *)
(*       fprintf oc "    production %s:%d;\n" outp 1; *)
      fprintf oc "    }\n\n"
  | SinkB ->
      let inp = "i" in
      fprintf oc "  actortype %s {\n" b.b_name;
      fprintf oc "    input %s;\n" inp;
      (* fprintf oc "    param %s;\n" cfg.snk_ofile; *)
(*       fprintf oc "    consumption %s:%d;\n" inp 1; *)
      fprintf oc "    }\n\n"
  |  _ ->
      ()

let dump_actor_decl oc g (name, n) =
  match n.sn_impl with
  | NI_Graph _ -> Misc.not_implemented "DIF: actor refined as a subgraph"
  | NI_Actor _ ->
     if List.exists (fun (_,b) -> b.b_name = name) g.sg_boxes  then begin
         (* Do not declare actors which are not instantiated in the graph *)
         fprintf oc "  actortype %s {\n" name;
         fprintf oc "    input %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " n.sn_intf.t_real_ins);
         fprintf oc "    output %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " n.sn_intf.t_real_outs);
         if n.sn_intf.t_params <> [] then
           fprintf oc "    param %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " n.sn_intf.t_params);
         fprintf oc "    }\n\n"
       end
     
let dump_actor_inst oc g (i,b) = 
  let string_of_param (n,(wid,ty,_)) = n ^ "=" ^ string_of_val (get_param_value "DIF" g wid) in
  let string_of_inp (id, (wid,ty,anns)) = edge_id wid ^ "->" ^ id  in
  let string_of_outp (id, (wids,ty,anns)) = 
    let string_of_woutp wid = id ^ "->" ^ edge_id wid in
    Misc.string_of_list string_of_woutp ", " wids in
  let param_ins, data_ins = List.partition (is_param_input g.sg_wires) b.b_ins in 
  fprintf oc "  actor %s {\n" (node_id i);
  fprintf oc "    type: %s;\n" b.b_name;
  if param_ins <> [] then
    fprintf oc "    param %s;\n" (Misc.string_of_list string_of_param ", " param_ins);
  fprintf oc "    interface %s;\n" (Misc.string_of_two_lists string_of_inp string_of_outp ", " data_ins b.b_outs);
  fprintf oc "    }\n\n"

let dump_graph ~toplevel path prefix ir id intf g = 
  let fname = path ^ id ^ ".dif" in
  let oc = open_out fname in
  fprintf oc "dif %s {\n\n" id;
  fprintf oc "  topology {\n";
  fprintf oc "    nodes = %s;\n" (Misc.string_of_list string_of_node ", " g.sg_boxes);
  fprintf oc "    edges = %s;\n" (Misc.string_of_list string_of_edge ", " g.sg_wires);
  fprintf oc "    }\n\n";
  (* let global_params = collect_parameters g.sg_boxes in  (\* TO FIX ! *\)
   * if global_params <> [] then begin
   *   fprintf oc "  parameter {\n";
   *   List.iter (dump_parameter oc) global_params;
   *   fprintf oc "    }\n\n"
   *   end; *)
  List.iter (dump_io_decl oc g) g.sg_boxes;
  List.iter (dump_actor_decl oc g) ir.ir_nodes;
  List.iter (dump_actor_inst oc g) g.sg_boxes;
  fprintf oc "}\n";
  Logfile.write fname;
  close_out oc

let dump_top_graph path prefix ir (id,g) =
  dump_graph ~toplevel:true path prefix ir id g.tg_intf g.tg_impl

let dump path prefix ir =
  List.iter (dump_top_graph path prefix ir) ir.ir_graphs
