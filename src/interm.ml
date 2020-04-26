(* Intermediate representation *)

open Syntax
open Printf
open Semval

type ir = {
  ir_nodes: (string * sn_desc) list;
  ir_graphs: (string * sn_desc) list;
  }
               
and sn_desc = {
  sn_intf: Syntax.node_intf;
  sn_impl: sn_impl; 
  }

and sn_impl =
  | NI_Actor of Syntax.actor_desc   (* Opaque node, with external implementation(s) *)
  | NI_Graph of sg_desc            

and sg_desc = { 
  sg_boxes: box_env; 
  sg_wires: wire_env;
  }

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

let dump_node ~typed (id,n) = 
  let i = n.sn_intf in 
  Printf.printf "%s: params=[%s] ins=[%s] outs=[%s] = %s\n"
    id
    (Misc.string_of_list Syntax.string_of_node_param ","  n.sn_intf.n_params)
    (Misc.string_of_list Syntax.string_of_node_io ","  n.sn_intf.n_ins)
    (Misc.string_of_list Syntax.string_of_node_io ","  n.sn_intf.n_outs)
    (string_of_node_impl n.sn_impl);
  dump_node_impl ~typed n.sn_impl

let dump_ir ?(typed=false) ir =
  printf "Intermediate representation ------\n";
  printf "- Nodes ----------------\n";
  List.iter (dump_node ~typed) ir.ir_nodes;
  printf "- Toplevel graphs ----------------\n";
  List.iter (dump_node ~typed) ir.ir_graphs
