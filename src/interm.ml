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
(* Result of static elaboration and input of backends *)

open Syntax
open Types
open Typing
open Printf
open Pr_type
open Semval

type ir = {
  ir_values: (string * sem_val) list;
  ir_nodes: (string * sn_desc) list;  (* Node models  *)
  ir_graphs: (string * tg_desc) list; (* Toplevel graphs *)
  }
               
and sn_desc = {
  sn_intf: Typing.typed_intf;
  sn_impl: sn_impl; 
  }

and sn_impl =
  | NI_Actor of Syntax.actor_desc   (* Opaque node, with external implementation(s) *)
  | NI_Graph of sg_desc             (* Sub-graph *)

and tg_desc = {
  tg_intf: Typing.typed_intf;
  tg_impl: sg_desc; 
  }

and sg_desc = { 
  sg_boxes: (bid * ir_box) list;
  sg_wires: (wid * sv_wire) list;
  }

and ir_box = {
  b_id: int;
  b_tag: box_tag;
  b_name: string;                  (* For regular (resp. param) boxes, name of the instanciated actor (resp. param) *)
  b_typ: typ;                      (* "Functional" type, i.e. either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
  b_ins: (string * (wid * typ * Syntax.io_annot list)) list;
  b_outs: (string * (wid list * typ * Syntax.io_annot list)) list;
  mutable b_val: b_val;            (* For parameter boxes *)
  }

and box_tag = 
    ActorB
  | IBcastB (* Implicit bcast (inserted automatically) *)
  | EBcastB (* Explicit bcast *)
  | SourceB
  | SinkB
  | GraphB
  | LocalParamB
  | InParamB
  (* | DelayB *)
  | DummyB  (* Temporary boxes used for handling recursive defns *)

and wid = int
and bid = int

and b_val = { 
  bv_lit: core_expr;     (* Original expression *)
  bv_sub: core_expr;     (* Original expression after substitution of dependencies (ex: "k+1" -> "i1+1") *)
  bv_val: sem_val         (* Statically computed value - SVUnit if N/A *)
  }

(* Accessors *)
          
let find_box boxes bid = 
    try List.assoc bid boxes
    with Not_found -> Misc.fatal_error "Interm.find_box: cannot find box from id"

let find_wire wires wid = 
    try List.assoc wid wires
    with Not_found -> Misc.fatal_error "Interm.find_wire: cannot find wire from id"

let get_src_box boxes (((s,ss),(d,ds)), ty(*, _*)) = find_box boxes s
let get_dst_box boxes (((s,ss),(d,ds)), ty(*, _*)) = find_box boxes d

(* Printing *)

let dump_value (id,v) = Printf.printf "%s: %s\n" id (string_of_semval v)
                      
let string_of_typed_bin (id,(wid,ty,_)) = id ^ ":" ^ string_of_type ty ^ "(<-W" ^ string_of_int wid ^ ")"
let string_of_typed_bout (id,(wids,ty,_)) = 
    id ^ ":" ^ string_of_type ty ^ "(->["
  ^ (Misc.string_of_list (function wid -> "W" ^ string_of_int wid) "," wids) ^ "])"

let string_of_typed_io (id,(ty,anns)) = id ^ ":" ^ string_of_type ty ^ Syntax.string_of_io_annots anns
let string_of_opt_value = function None -> "?" | Some v -> string_of_semval v
let string_of_typed_param (id,ty) = id ^ ":" ^ string_of_type ty 

let string_of_impl m = match m with
  | NI_Actor _ -> "actor"
  | NI_Graph _ -> "subgraph"

let dump_node (id,n) = 
  Pr_type.reset_type_var_names ();
  let i = n.sn_intf in 
  Printf.printf "%s: %s : params=[%s] ins=[%s] outs=[%s] = %s\n"
    id
    (string_of_type_scheme i.t_sig)
    (Misc.string_of_list string_of_typed_param ","  i.t_params)
    (Misc.string_of_list string_of_typed_io ","  i.t_ins)
    (Misc.string_of_list string_of_typed_io ","  i.t_outs)
    (string_of_impl n.sn_impl)

let box_prefix b = match b.b_tag with
    IBcastB -> "Y"
  | EBcastB -> "X"
  (* | DelayB -> "D" *)
  | SourceB -> "I"
  | SinkB -> "O"
  | ActorB -> "B"
  | GraphB -> "G"
  | LocalParamB -> "L"
  | InParamB -> "P"
  | DummyB -> "_"

let string_of_box_value v =
  "\"" ^ Syntax.string_of_core_expr v.bv_lit ^ "\"=" ^ string_of_semval v.bv_val

let dump_box (i,b) =
  Pr_type.reset_type_var_names ();
  Printf.printf "%s%d: %s ins=[%s] outs=[%s] %s\n"
        (box_prefix b)
        i
        b.b_name
        (* (string_of_box_value b) *)
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)
        (match b.b_tag with InParamB | LocalParamB -> "val=" ^ string_of_box_value b.b_val | _ -> "")

let dump_wire (i,(((s,ss),(d,ds)),ty(*,kind*))) =
  Printf.printf "W%d: %s: (B%d,%d) -> (B%d,%d)\n" i (string_of_type ty) s ss d ds

let dump_graph (id,g) =
  printf "* %s\n" id;
  printf "- Boxes --------------------------\n";
  List.iter dump_box g.sg_boxes;
  printf "- Wires --------------------------\n";
  List.iter dump_wire g.sg_wires;
  printf "----------------------------------\n"

let dump_graph_node (id,n) =
  match n.sn_impl with
  | NI_Graph g -> dump_graph (id,g)
  | NI_Actor _ -> ()

let dump_top_graph (id,g) = dump_graph (id, g.tg_impl)
                         
let dump_ir ir =
  printf "Intermediate representation ------\n";
  printf "- Values -------------------------\n";
  List.iter dump_value ir.ir_values;
  printf "- Nodes ---------------------------\n";
  List.iter dump_node ir.ir_nodes;
  printf "- Graphs --------------------------\n";
  List.iter dump_top_graph ir.ir_graphs;
  List.iter dump_graph_node ir.ir_nodes
