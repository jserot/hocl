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

(* Intermediate representation *)

open Syntax
open Printf
open Semval
open Eval

type cfg = {
  mutable insert_bcasts: bool;
  bcast_name: string;
  (* mutable insert_fifos: bool; *)
  (* fifo_name: string; *)
  }

let cfg = {
  insert_bcasts = false;
  bcast_name = "bcast";
  (* insert_fifos = false; *)
  (* fifo_name = "fifo"; *)
  }

type ir = {
  ir_nodes: (string * sn_desc) list;
  ir_graphs: (string * sn_desc) list;
  }
               
and sn_desc = {
  sn_intf: sn_intf;
  sn_impl: sn_impl; 
  }

and sn_intf = {
    sn_id: string;
    sn_isgraph: bool;
    sn_ins: (string * Types.typ * Syntax.expr option * io_annot list) list;
    sn_outs: (string * Types.typ * Syntax.expr option * io_annot list) list;
  }

and sn_impl =
  | NI_Actor of Syntax.actor_desc   (* Opaque node, with external implementation(s) *)
  | NI_Graph of sg_desc            

and sg_desc = { 
  sg_boxes: box_env; 
  sg_wires: wire_env;
  }

(* Transformations *)

(* Insert (implicit) BCASTs 
 * 
 *        +--------+         +--------+          +--------+         +---------+          +--------+
 *        |        |         |        |          |        |         |         |    w1'   |        |
 *        |        |    w1   |        |          |        |    w'   |        0|--------->|        |
 *        |   A1 k1|-------->|k2 A2   |    ===>  |   A1 k1|-------->|0 Bcast  |          |k2 A2   |
 *        |        |\        |        |          |        |         |        1|----+     |        |
 *        |        | |       |        |          |        |         |         |    |     |        |
 *        +--------+ |       +--------+          +--------+         +---------+    |     +--------+
 *                   |                                                             |
 *                   |       +--------+                                            |     +--------+
 *                   |       |        |                                        w2' |     |        |
 *                   |  w2   |        |                                            |     |        |
 *                   +------>|k3 A3   |                                            +---->|k3 A3   |
 *                           |        |                                                  |        |
 *                           |        |                                                  |        |
 *                           +--------+                                                  +--------+
*)

(* let new_bcast_box ty wid wids =
 *   let bid = new_bid () in
 *   let bos = wids |> Array.of_list |> Array.mapi (fun i wid -> "o_" ^ string_of_int (i+1),[wid],ty,[]) in
 *   bid, { b_id=bid; b_tag=BcastB; b_model=cfg.bcast_name; 
 *          b_ins=[|"i",wid,ty,[]|]; b_outs=bos; b_val=SVUnit } *)

(* let is_bcast_box boxes bid = (lookup_box boxes bid).b_tag = BcastB *)

(* let rec insert_bcast bid oidx (boxes,wires,box) bout = 
 *   match bout with
 *   | (id, ([],_,_)) -> boxes, wires, box       (\* should not happen ? *\)
 *   | (id, ([wid],_,_)) -> boxes, wires, box    (\* no need to insert here *\)
 *   | (id, (wids,ty,anns)) ->                   (\* the relevant case : a box output connected to several wires *\)
 *       let wid' = new_wid () in
 *       let m, mb = new_bcast_box ty wid' wids in
 *       let box' = { box with b_outs = Misc.replace_assoc box.b_outs id (fun ( -> [wid'],ty,anns) } in
 *       let wires' = Misc.foldl_index (update_wires m) wires wids in
 *       let boxes' = Misc.replace_assoc bid (function b -> box') boxes in
 *       (\* (m,mb) :: boxes', (wid',(((bid,oidx),(m,0)),ty,get_wire_kind wires (List.hd wids))) :: wires', box' *\)
 *       (m,mb) :: boxes', (wid',(((bid,oidx),(m,0)),ty)) :: wires', box' *)

(* and get_wire_kind wires wid =
 *   try
 *     match List.assoc wid wires with
 *     | _,_,k -> k
 *   with
 *   | Not_found -> 
 *      Misc.fatal_error "Static.get_wire_kind" *)


let bcast_output bid b sel (boxes,wires) bout =
  let update_output bouts sel wid' =
    let r = Array.copy bouts in
    let id,_,ty,anns = r.(sel) in
    r.(sel) <- (id,[wid'],ty,anns);
    r in
  let update_wires s' j wires wid = 
    (* Misc.update_assoc wires (fun ((s,ss),(d,ds),ty) -> (s',j),(d,ds),ty) wid in *)
    Misc.update_assoc wires (fun ((s,ss,ty),(d,ds,ty')) -> (s',j,ty),(d,ds,ty')) wid in
  match bout with
  | (id,[],_,_) -> boxes, wires       (* should not happen ? *)
  | (id,[wid],_,_) -> boxes, wires   (* Single output connexion *)
  | (id,wids,ty,anns) ->              (* The relevant case : a box output connected to several wires *)
     let wid' = new_wid () in
     let bid' = new_bid () in
     let b' = {
         b_id = bid';
         b_tag = BcastB;
         b_model = cfg.bcast_name; 
         b_ins = [|"i",wid',ty,[]|];
         b_outs = wids |> List.mapi (fun i wid -> "o_" ^ string_of_int (i+1),[wid],ty,[]) |> Array.of_list;
         b_val = no_bval } in
     let b'' = { b with b_outs = update_output b.b_outs sel wid' } in
     let wires' = Misc.fold_lefti (update_wires bid') wires wids in
     let boxes' = Misc.replace_assoc boxes bid b'' in
     let wires'' = (wid',((bid,sel,ty),(bid',0,ty))) :: wires' in
     let boxes'' = (bid',b') :: boxes' in 
     boxes'', wires''

let insert_bcast (boxes,wires) (bid,b) = 
   Misc.array_foldli (bcast_output bid b) (boxes,wires) b.b_outs

let insert_bcast_after after_tags (boxes,wires) (bid,b) = 
  if List.mem b.b_tag after_tags then 
    let boxes', wires' = insert_bcast (boxes,wires) (bid,b) in
    boxes', wires'
  else
    boxes, wires

let insert_bcasts after_tags ir = 
  let insert g =
    let boxes', wires' = List.fold_left (insert_bcast_after after_tags) (g.sg_boxes,g.sg_wires) g.sg_boxes in
    { sg_boxes = boxes'; sg_wires = wires' } in
  let insert (id,n) = match n.sn_impl with
    | NI_Graph g -> id, {n with sn_impl = NI_Graph (insert g)}
    | _ -> id,n in
  { ir_nodes = List.map insert ir.ir_nodes; 
    ir_graphs = List.map insert ir.ir_graphs }

(* Printing *)

let string_of_node_impl m = match m with
  | NI_Actor _ -> "actor"
  | NI_Graph _ -> "subgraph"

let dump_box ~typed (i,b) = Printf.printf "  B%d: %s\n" i (string_of_box ~typed b)

let dump_wire ~typed (i,w) = Printf.printf "  W%d: %s\n" i (string_of_wire ~typed w)

let dump_graph ~typed g =
  printf "  - Boxes --------------------------\n";
  List.iter (dump_box ~typed) g.sg_boxes;
  printf "  - Wires --------------------------\n";
  List.iter (dump_wire ~typed) g.sg_wires;
  printf "  ----------------------------------\n"

let dump_node_impl ~typed m = match m with
  | NI_Actor _ -> ()
  | NI_Graph g -> dump_graph ~typed g

let string_of_node_io (id,ty,e,anns) =
  let string_of_opt_exp = function None -> "" | Some e -> "=" ^ string_of_expr e in
  id ^ ":" ^ Pr_type.string_of_type ty ^ string_of_opt_exp e

let dump_node ~typed (id,n) = 
  let i = n.sn_intf in 
  Printf.printf "%s: ins=[%s] outs=[%s] = %s\n"
    id
    (Misc.string_of_list string_of_node_io ","  n.sn_intf.sn_ins)
    (Misc.string_of_list string_of_node_io ","  n.sn_intf.sn_outs)
    (string_of_node_impl n.sn_impl);
  dump_node_impl ~typed n.sn_impl

let dump_ir ?(typed=false) ir =
  printf "Intermediate representation ------\n";
  printf "- Nodes ----------------\n";
  List.iter (dump_node ~typed) ir.ir_nodes;
  printf "- Toplevel graphs ----------------\n";
  List.iter (dump_node ~typed) ir.ir_graphs
