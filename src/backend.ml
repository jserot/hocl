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

open Syntax
open Interm

let get_impl_fns target name attrs =
  match List.assoc_opt "incl_file" attrs,
        List.assoc_opt "loop_fn" attrs,
        List.assoc_opt "init_fn" attrs
  with
  | Some f, Some f', Some f'' -> f, f', f'', List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | Some f, Some f', None -> f, f', "", List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | _, _, _ -> Error.incomplete_actor_impl target name

let get_node_desc ir id =
    try List.assoc id ir.ir_nodes
    with Not_found -> Misc.fatal_error "Backend.get_node_desc"

let get_actor_desc ir target id = 
  match get_node_desc ir id with
  | { sn_impl=NI_Actor impls } -> 
     begin
       try List.assoc target impls
       with Not_found -> Error.missing_actor_impl target id
     end
  | _ ->
     Misc.fatal_error "Backend.get_actor_desc"

let string_of_io_rate rate = match rate with
  | None -> "1"
  | Some e -> Syntax.string_of_expr e

let get_rate_expr anns = 
  match List.assoc_opt "rate" anns with
  | Some (AN_Expr e) -> Some e
  | _ -> None

let is_constant_expr e =
  match e.e_desc with
  | EInt _ -> true
  | EBool _ -> true
  | _ -> false

type delay_spec = {
    ds_typ: Types.typ;
    ds_iv: string;
    ds_i: string;
    ds_irate: Syntax.expr option;
    ds_o: string;
    ds_orate: Syntax.expr option 
  }

let get_delay_spec name intf =
     let iv_name, ty =
       match List.find_opt (fun (id,ty,e,anns) -> id = "ival") intf.sn_ins with
       | None -> Error.missing_ival_param name
       | Some (id,ty,_,_) -> id, ty in
     let get_io_spec ios =
       match List.find_opt (fun (id,ty,_,_) -> not (Types.is_param_type ty)) ios with
       | Some (id,ty,e,annots) -> id, get_rate_expr annots
       | None -> Error.illegal_interface "delay" name " (cannot find data input)" in
     let i_name, i_rate = get_io_spec intf.sn_ins in
     let o_name, o_rate = get_io_spec intf.sn_outs in
     let rate_expr_eq e1 e2 = match e1, e2 with
       | None, None -> true
       | Some e1, Some e2 -> Syntax.basic_expr_equal e1 e2
       | _, _ -> false in
     if not (rate_expr_eq i_rate o_rate) then Error.illegal_interface "delay" name " (input and output rates do not match)";
     { ds_typ=ty; ds_iv=iv_name; ds_i=i_name; ds_irate=i_rate; ds_o=o_name; ds_orate=o_rate }

let rec string_of_basic_expr ?(localize_id=Fun.id) e =
  match e.e_desc with
  | EInt n -> string_of_int n
  | EBool b -> string_of_bool b
  | EVar v -> localize_id v
  | EBinop (op, e1, e2) -> string_of_basic_expr' ~localize_id e1 ^ op ^ string_of_basic_expr' ~localize_id e2
  | EQuote e -> string_of_basic_expr ~localize_id e
  | _ -> Misc.fatal_error "Backend.string_of_basic_expr"

and string_of_basic_expr' ?(localize_id=Fun.id) e =
  if is_simple_basic_expr e
  then string_of_basic_expr ~localize_id e
  else "(" ^ string_of_basic_expr ~localize_id e ^ ")"

and is_simple_basic_expr e = match e.e_desc with
  | EVar _ -> true
  | EInt _ -> true
  | EBool _ -> true
  | _ -> false

let is_special_actor name =
  match name with
  | "delay" | "bcast" | "switch" | "merge" -> true
  | _ -> false

let is_data_wire (wid,((_,_,ty),_)) = Types.is_data_type ty
let is_param_input (id,wid,ty,annots) = Types.is_param_type ty

let get_src_box boxes ((s,ss,ty),_) = Eval.lookup_box s boxes
let get_dst_box boxes (_,(d,ds,ty)) = Eval.lookup_box d boxes

let get_param_value backend g wid =
  match get_src_box g.sg_boxes (Eval.lookup_wire wid g.sg_wires) with
  | { b_tag=LocalParamB; b_val={bv_val=v} } 
  | { b_tag=InParamB; b_val={bv_val=v} } -> v
  | _ -> Misc.fatal_error (backend ^ " backend cannot retrieve parameter value")
