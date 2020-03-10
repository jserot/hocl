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

(* Several utility fns common to all backends *)

open Types
open Typing
open Static

let lookup_box boxes bid = 
      try List.assoc bid boxes
      with Not_found -> Misc.fatal_error "Backend.lookup_box"

let lookup_wire wires wid = 
      try List.assoc wid wires
      with Not_found -> Misc.fatal_error "Backend.lookup_wire"

let is_special_actor name =
  match name with
  | "delay" | "switch" | "merge" | "pmerge" -> true
  | _ -> false
    
let is_data_wire (wid,(_,_,kind)) = kind=Ssval.DataW

let is_param_input wires (iid, (wid,ty,annots)) = 
  match lookup_wire wires wid with
  | _, _, Ssval.ParamW -> true
  | _, _, _ -> false

let get_impl_fns target name attrs =
  match List.assoc_opt "incl_file" attrs,
        List.assoc_opt "loop_fn" attrs,
        List.assoc_opt "init_fn" attrs
  with
  | Some f, Some f', Some f'' -> f, f', f'', List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | Some f, Some f', None -> f, f', "", List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | _, _, _ -> Error.incomplete_actor_impl target name

let get_node_desc sp id =
    try List.assoc id sp.sp_nodes
    with Not_found -> Misc.fatal_error "Backend.get_node_desc"

let get_actor_desc sp target id = 
  match get_node_desc sp id with
  | { sn_impl=NI_Actor impls } -> 
     begin
       try List.assoc target impls
       with Not_found -> Error.missing_actor_impl target id
     end
  | _ ->
     Misc.fatal_error "Backend.get_actor_desc"

let rec string_of_core_expr ?(localize_id=Fun.id) e =
  match e.Syntax.ce_desc with
  | Syntax.EInt n -> string_of_int n
  | Syntax.EBool b -> string_of_bool b
  | Syntax.EVar v -> localize_id v
  | Syntax.EBinop (op, e1, e2) -> string_of_core_expr' ~localize_id e1 ^ op ^ string_of_core_expr' ~localize_id e2

and string_of_core_expr' ?(localize_id=Fun.id) e =
  if Syntax.is_simple_core_expr e
  then string_of_core_expr ~localize_id e
  else "(" ^ string_of_core_expr ~localize_id e ^ ")"

let is_actual_actor_io (id,(ty,_)) = not (is_unit_type ty)

let string_of_io_rate rate = match rate with
  | None -> "1"
  | Some e -> Syntax.string_of_rate_expr e

let get_rate_annot anns = 
  List.find_opt (function Syntax.IA_Rate _ -> true | _ -> false) anns

let get_rate_expr anns =
  match get_rate_annot anns with
  | Some (IA_Rate e) -> Some e
  | _ -> None

type delay_spec = {
    ds_typ: typ;
    ds_iv: string;
    ds_i: string;
    ds_irate: Syntax.core_expr option;
    ds_o: string;
    ds_orate: Syntax.core_expr option
  }
      
let get_delay_spec name intf =
     let iv_name, ty =
       match List.find_opt (fun (id, _) -> id = "ival") intf.t_params with
       | None -> Error.missing_ival_param name
       | Some (id,ty) -> id, ty in
     let get_io_spec ios =
       match ios with
       | [id,(ty,annots)] -> id, get_rate_expr annots
       | _ -> Error.illegal_interface "delay" name " (should have exactly one input and one output)" in
     let i_name, i_rate = get_io_spec intf.t_ins in
     let o_name, o_rate = get_io_spec intf.t_outs in
     let rate_expr_eq e1 e2 = match e1, e2 with
       | None, None -> true
       | Some e1, Some e2 -> Syntax.core_expr_equal e1 e2
       | _, _ -> false in
     if not (rate_expr_eq i_rate o_rate) then Error.illegal_interface "delay" name " (input and output rates do not match)";
     { ds_typ=ty; ds_iv=iv_name; ds_i=i_name; ds_irate=i_rate; ds_o=o_name; ds_orate=o_rate }

let collect_sub_graphs sp = 
    List.fold_left
      (fun acc (id,n) ->
        match n.sn_impl with
        | NI_Graph g -> (id,(n.sn_intf,g))::acc
        | _ -> acc)
      []
      sp.sp_nodes
                                                                       
