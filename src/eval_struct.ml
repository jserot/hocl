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

(* Evaluation of structural graph descriptions *)

open Syntax
open Semval
open Eval

(* Aux *)

type sbox_desc = 
  (* This is a variant of [Syntax.box_desc] in which IOs have been named 
     according to the refered node declaration *)
  { sb_node: string; (* Name of the instanciated model *)
    sb_ins: (string * Syntax.expr * Types.typ * Syntax.io_annot list) list;
    sb_outs: (string * Syntax.expr * Types.typ * Syntax.io_annot list) list }
   
let name_box_ios env b =
  let name f ns vs =
    try List.map2 f ns vs
    with Invalid_argument _ -> Misc.fatal_error "Eval_struct.name_box_ios: node mismatch" in
  let n = get_node_desc env b.bx_node in
  { sb_node = b.bx_node;
    sb_ins = List.map2 (fun (n,ty,e,anns) io -> n,io,ty,anns) n.sn_ins b.bx_ins;
    sb_outs = List.map2 (fun (n,ty,anns) io -> n,io,ty,anns) n.sn_outs b.bx_outs; }

(* |- WireDecl => E, W *)

let eval_wire_decl { wr_desc=(id,t) } =
  let wid = new_wid () in
  (* let ty = Types.type_wire t.te_typ in *)
  let ty = t.te_typ in
  let w = new_wire ty in
  (id,SVWire wid),
  (wid,w)

(* E, B, W |-> BoxInp => B', W', M *)

let eval_box_input loc env dst sel (boxes,wires,bindings) (slot,exp,ty,anns) =
  match exp.e_desc with
  | EVar id ->   (* Ex: node n: foo(i)(...) *)
     begin
       match lookup env loc id with
       | SVLoc (src,sel',_) ->
          let k = new_wid () in
          let w = ((src,sel',ty),(dst,sel,ty)) in
          let b' = add_box_output boxes src sel' k in
          Misc.replace_assoc boxes src b',
          (k,w)::wires,
          bindings@[slot,k,ty,anns]
       | SVWire k ->
          let w =
            begin
              match lookup_wire k wires with
              | (src,(-1,-1,ty)) -> (src, (dst,sel,ty))
              | _ -> Misc.fatal_error "Eval_struct.eval_box_input"
            end  in
          boxes,
          Misc.replace_assoc wires k w,
          bindings@[slot,k,ty,anns]
       | _ ->
          Misc.fatal_error "Eval_struct.eval_box_input"
     end
  | _ -> (* Ex: node n: f(k+1)(...) *)
     let l, boxes', wires' = Eval.eval_param_expr env boxes exp in
     let k = new_wid () in
     let w = (l,(dst,sel,ty)) in
     boxes+++boxes',
     wires++wires'++[k,w],
     bindings@[slot,k,ty,anns]

(* E, W |-> BoxInps => B, M *)

let eval_box_inputs loc env boxes wires dst inps =
  Misc.fold_lefti (eval_box_input loc env dst) (boxes,wires,[]) inps

(* E, W |-> BoxOutp => B, M *)

let eval_box_output loc env src sel (boxes,wires,bindings) (slot,expr,ty,anns) =
  match expr.e_desc with
  | EVar id -> 
     begin
       match lookup env loc id with
       | SVLoc (dst,sel',_) ->
          let k = new_wid () in
          let w = ((src,sel,ty),(dst,sel',ty)) in
          let b' = set_box_input boxes dst sel' k in
          Misc.replace_assoc boxes dst b',
          (k,w)::wires,
          (id,[k],ty,anns)::bindings
       | SVWire k ->
          let w =
            begin
              match lookup_wire k wires with
              | ((-1,-1,ty),dst) -> ((src,sel,ty),dst)
              | _ -> Misc.fatal_error "Eval_struct.eval_box_output"
            end  in
          boxes,
          Misc.replace_assoc wires k w,
          bindings@[slot,[k],ty,anns] (* Keep ordering ! *)
       | _ ->
          Misc.fatal_error "Eval_struct.eval_box_output"
     end
  | _ ->
     Misc.fatal_error "Eval_struct.eval_box_output" (* should not happen *)

(* E, W |-> BoxOutps => B, M *)

let eval_box_outputs loc env boxes wires src outps =
  Misc.fold_lefti (eval_box_output loc env src) (boxes,wires,[]) outps
  
(* E, W |-> BoxDecl => B, W' *)

let eval_box_decl env (boxes,wires) {bx_desc=(id,b'); bx_loc=loc} =
  let b = name_box_ios env b' in
  match lookup env loc b.sb_node with
  | SVNode n ->
     let l = new_bid () in
     let boxes_i, wires_i, bins = eval_box_inputs loc env boxes wires l b.sb_ins in
     let boxes_o, wires_o, bouts = eval_box_outputs loc env boxes_i wires_i l b.sb_outs in
     let tag = tag_of_kind n.sn_kind in
     let b = new_box l n.sn_id tag bins bouts no_bval in
     (l,b)::boxes_o,
     wires_o
  | _ ->
     Misc.fatal_error "Eval_struct.eval_box_decl"

(* E, W |-> BoxDecls => B, W' *)

let eval_box_decls env boxes wires box_decls =
  List.fold_left (eval_box_decl env) (boxes,wires) box_decls
  
(* E, B |- StructGraphDefn => B', W *)

let eval_struct_graph_desc (env,boxes) g =
  let env_w, wires_w = g.gs_wires |> List.map eval_wire_decl |> List.split in
  let boxes', wires = g.gs_boxes |> eval_box_decls (env_w @ env) boxes wires_w in
  boxes', wires
