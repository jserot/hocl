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

(* Static evaluation *)

open Syntax
open Semval
open Interm
open Eval

(* Aux *)
   
let rec static_value e = match e.e_desc with
  | EInt n -> SVInt n
  | EBool b -> SVBool b
  | EQuote e -> SVQuote e
  | _ -> failwith "Static.static_value" (* TO FIX !*)
       
let eval_param_expr ty e = match e with
  | Some e when Types.is_param_type ty -> Some (static_value e)
  | _ -> None
       
(* E, B |- NodeImpl => G *)

let eval_node_impl (env, boxes) impl =
  match impl with
  | NM_Actor d -> NI_Actor d
  | NM_Fun g -> 
       let _, boxes', wires = Eval_fun.eval_val_decls (env,boxes) g in
       NI_Graph { sg_boxes=boxes'; sg_wires=wires }
  | NM_Struct g ->
       let boxes', wires = Eval_struct.eval_struct_graph_desc (env,boxes) g in
       NI_Graph { sg_boxes=boxes'; sg_wires=wires }

(* E |- NodeDecl => E', N *)

let eval_node_decl (env,nodes) {nd_desc=(id,n)} =
  let env', boxes' = eval_node_intf n.n_intf in
  let impl = eval_node_impl (env'@env, boxes') n.n_impl in
  let intf = {
    sn_id = n.n_intf.n_id;
    sn_isgraph = n.n_intf.n_isgraph;
    sn_ins = List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, e, anns) n.n_intf.n_ins;
    sn_outs = List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, e, anns) n.n_intf.n_outs
    } in
  let node = { sn_intf=intf; sn_impl=impl } in
  let n = {
      sn_id = id;
      sn_kind =  (match impl with | NI_Actor _ -> ActorN | NI_Graph _ -> GraphN);
      sn_ins =
        List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, e, anns)
          n.n_intf.n_ins;
      sn_supplied_ins = []; (* TO FIX ? Should we consider that expressions attached to _graph_ inputs provided values ? *)
      sn_outs =
        List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, anns)
          n.n_intf.n_outs
      } in
  (id, SVNode n) :: env,
  (id, node) :: nodes

(* E |- NodeDecls => E', N *)
  
let eval_node_decls env node_decls = 
  List.fold_left eval_node_decl (env,[]) node_decls

(* E |-> Program => E', N *)
    
let build_static env p =
  let env_g, boxes_g, wires_g = Eval_fun.eval_val_decls (env,[]) p.values in
  let env_n, nodes = eval_node_decls (env++env_g) p.nodes in
  let graphs, nodes' = List.partition (fun (_,n) -> n.sn_intf.sn_isgraph) nodes in
  { ir_nodes = nodes'; ir_graphs = graphs }

(* Printing *)

let dump_static_value tenv (id,v) =
  let ty =
    try List.assoc id tenv
    with Not_found -> Misc.fatal_error "Static.dump_static_value" in
  Printf.printf "val %s : %s = %s\n" id (Pr_type.string_of_type_scheme ty) (string_of_semval v)

let dump_static_env title tenv senv =
  Printf.printf "%s ---------------\n" title;
  List.iter (dump_static_value tenv) senv;
  Printf.printf "----------------------------------\n"
    
