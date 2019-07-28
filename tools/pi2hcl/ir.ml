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

type key = string
type value = string

type node_desc = {
    n_id: string;
    n_name: string;
    n_kind: string;
    n_period: string;
    n_desc: string;
    n_ports: port_desc list;
    n_init_fn: string;
    n_loop_fn: string;
    n_ios: io_desc list;
    mutable n_mark: mark (* for topological sorting *)
  }
    
and port_desc = {
    pt_name: string;
    pt_kind: string;
    pt_expr: string;
    pt_annotation: string;
  }

and io_desc = {
    io_name: string;
    io_direction: string;
    io_type: string;
    io_isConfig: string;
  }
               
and mark = Unmarked | TemporaryMark | PermanentMark
                                    
type edge_desc = {
    e_id: string;
    e_kind: string;
    e_type: string;
    e_source: string;
    e_sourceport: string;
    e_target: string;
    e_targetport: string
  }

type program_desc = {
    p_name: string;
    p_nodes: node_desc list;
    p_edges: edge_desc list;
  }
                 

let rem_url ((_,name),v) = (name,v)

let mk_edge_id i = "e" ^ string_of_int i
let mk_node_id i = "n" ^ string_of_int i
                 
let mk_edge i (attrs, _) = mk_edge_id (i+1), List.map rem_url attrs

let node_info = function `El _ -> true | `Data _ -> false
                                                       
let mk_node i (attrs, subnodes) = mk_node_id (i+1), List.map rem_url attrs, List.filter node_info subnodes
                       
let rec get_data l = match l with
  | [] -> raise Not_found
  | `Data d :: _ -> d 
  | _ :: rest -> get_data rest


let rec mk_node_desc (id,attrs,nodes) =
  let name = List.assoc "id" attrs in
  let kind = List.assoc "kind" attrs in
  match kind with
    "actor" -> 
    { n_id = id;    
      n_name = name;
      n_kind = kind;
      n_period = (try List.assoc "period" attrs with Not_found -> "");
      n_desc = nodes |> Ezxmlm.member_with_attr "data" |> snd |> get_data;
      n_ports = nodes |> Ezxmlm.members_with_attr "port" |> List.map fst |> List.map (List.map rem_url)
                    |> List.map mk_port_desc;
      n_init_fn =
        (try nodes |> Ezxmlm.member_with_attr "init"  |> fst |> List.map rem_url |> List.assoc "name"
        with Ezxmlm.Tag_not_found _ -> ""); 
      n_loop_fn = 
        (try nodes |> Ezxmlm.member_with_attr "loop"  |> fst |> List.map rem_url |> List.assoc "name"
        with Ezxmlm.Tag_not_found _ -> ""); 
      n_ios =
        (try nodes |> Ezxmlm.member_with_attr "loop" |> snd |> Ezxmlm.members_with_attr "param"
             |> List.map fst |> List.map (List.map rem_url) |> List.map mk_io_desc 
        with Ezxmlm.Tag_not_found _ -> []); 
      n_mark = Unmarked; }
   | "broadcast" -> 
    { n_id = id;    
      n_name = name;
      n_kind = kind;
      n_period = "";
      n_desc = "";
      n_ports = nodes |> Ezxmlm.members_with_attr "port" |> List.map fst |> List.map (List.map rem_url)
                    |> List.map mk_port_desc;
      n_init_fn = "";
      n_loop_fn =  "";
      n_ios = [];
      n_mark = Unmarked; }
    | "src" | "snk" -> 
    { n_id = id;    
      n_name = name;
      n_kind = kind;
      n_period = "";
      n_desc = "";
      n_ports = nodes |> Ezxmlm.members_with_attr "port" |> List.map fst |> List.map (List.map rem_url)
                    |> List.map mk_port_desc;
      n_init_fn = "";
      n_loop_fn =  "";
      n_ios = [];
      n_mark = Unmarked; }
  | "param" ->
    { n_id = id;    
      n_name = name;
      n_kind = kind;
      n_period = "";
      n_desc = List.assoc "expr" attrs; 
      n_ports = [];
      n_init_fn = "";
      n_loop_fn = "";
      n_ios = [];
      n_mark = Unmarked; }
  | _ ->
    { n_id = id;    
      n_name = name;
      n_kind = kind;
      n_period = "";
      n_desc = "";
      n_ports = [];
      n_init_fn = "";
      n_loop_fn = "";
      n_ios = [];
      n_mark = Unmarked; }

and mk_port_desc l =
  let lookup k = try List.assoc k l with Not_found -> "" in
  { pt_name = lookup "name";
    pt_kind = lookup "kind";
    pt_expr = lookup "expr";
    pt_annotation = lookup "annotation" }

and mk_io_desc l =
  let lookup k = try List.assoc k l with Not_found -> "" in
  { io_name = lookup "name";
    io_direction = lookup "direction";
    io_type = lookup "type";
    io_isConfig = lookup "isConfig" }
  
let mk_edge_desc (id,l) =
  let lookup k = try List.assoc k l with Not_found -> "" in
  let kind = lookup "kind" in
  { e_id = id;
    e_kind = kind;
    e_type = (match kind with "dependency" -> "nat" | _ -> lookup "type");
    e_source = lookup "source";
    e_sourceport = lookup "sourceport";
    e_target = lookup "target";
    e_targetport = lookup "targetport" }


exception No_matching_edge of string * string * string
exception Several_matching_edges of string * string * string
exception No_matching_node of string * string
exception Several_matching_nodes of string * string

let find_inp_edge edges (target,targetport) = 
  match List.filter (fun e -> e.e_target = target && e.e_targetport = targetport) edges with
  | [] -> raise (No_matching_edge ("Ir.find_inp_edge", target, targetport))
  | [e] -> e
  | _ -> raise (Several_matching_edges ("Ir.find_inp_edge", target, targetport))
                            
let find_outp_edge edges (source,sourceport) = 
  match List.filter (fun e -> e.e_source = source && e.e_sourceport = sourceport) edges with
  | [] -> raise (No_matching_edge ("Ir.find_outp_edge", source, sourceport))
  | [e] -> e
  | _ -> raise (Several_matching_edges ("Ir.find_outp_edge", source, sourceport))
                       
let find_target_node nodes e =
  match List.filter (fun n -> n.n_name = e.e_target) nodes with
  | [] -> raise (No_matching_node ("Ir.find_target_node", e.e_target))
  | [n] -> n
  | _ -> raise (Several_matching_nodes ("Ir.find_target_node", e.e_target))
      
let succs edges nodes n =
  let mk_edge_spec n p = n.n_name, p.pt_name in
  let is_data_outp p = p.pt_kind = "output"  in
  List.filter is_data_outp n.n_ports
  |> List.map (mk_edge_spec n)
  |> List.map (find_outp_edge edges)
  |> List.map (find_target_node nodes)

exception CyclicGraph of string

let topological_sort edges nodes =    (* DFS-based algorithm *)
  let acc = ref [] in
  let rec visit n =
    match n.n_mark with
    | PermanentMark -> ()
    | TemporaryMark -> raise (CyclicGraph n.n_name)
    | Unmarked ->
       begin
         n.n_mark <- TemporaryMark;
         List.iter visit (succs edges nodes n);
         n.n_mark <- PermanentMark;
         acc := n :: !acc 
       end in
  let rec iter () = 
    match List.find_opt (fun n -> n.n_mark = Unmarked) nodes with
    | None -> !acc
    | Some n -> visit n; iter () in
  iter ()
               
let from_file f =
  let ic = open_in f in
  let _, xml = Ezxmlm.from_channel ic in
  let g = Ezxmlm.member  "graphml" xml |> Ezxmlm.member "graph" in
  let name = g |> Ezxmlm.member_with_attr "data" |> snd |> get_data in
  let nodes = g |> Ezxmlm.members_with_attr "node" |> List.mapi mk_node |> List.map mk_node_desc  in
  let edges = g |> Ezxmlm.members_with_attr "edge" |> List.mapi mk_edge |> List.map mk_edge_desc in
  { p_name = name;
    p_nodes = topological_sort edges nodes;
    p_edges = edges }
                            
