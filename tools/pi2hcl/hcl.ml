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

(* HCL backend *)

open Printf
   
type hcl_config = {
  mutable annot_file: string;
  mutable default_param_type: string;
  }

let cfg = {
  annot_file = "";
  default_param_type = "nat";
}

let collect_types acc e =
  let open Ir in
  if not (List.mem e.e_type acc) then e.e_type::acc else acc

let output_type oc t =
  fprintf oc "type %s;\n" t
  
let mk_hcl_fname f =
  if Filename.check_suffix f "pi"
  then Misc.replace_suffix "hcl" (Filename.basename f)
  else f
  
let output_pragma oc n =
  let open Ir in
  match n.n_kind, n.n_loop_fn, n.n_init_fn with
  | "actor", "", "" -> (* Subgraph case *)
     fprintf oc "#pragma code(\"%s\", \"%s\");\n" n.n_name (mk_hcl_fname n.n_desc) (* .pi -> .hcl *)
  | "actor", "", _ ->
    fprintf oc "#pragma code(\"%s\", \"%s\", \"%s\");\n" n.n_name n.n_desc n.n_loop_fn
  | "actor", _, _ ->
    fprintf oc "#pragma code(\"%s\", \"%s\", \"%s\", \"%s\");\n" n.n_name n.n_desc n.n_loop_fn n.n_init_fn
  | _, _, _ -> (* TO FIX ? *)
     ()

exception Aie of string * Ir.node_desc * Ir.edge_desc list

let get_port_type edges node port =
  let open Ir in
  try
    let io = List.find (fun io -> io.io_name = port.pt_name) node.n_ios in (* For "regular" (C-bound) actors ... *)
    io.io_type
  with Not_found -> (* For hierarchical actors (described by a sub-graph), the type must be inferred from the context *)
    let e =
      begin match port.pt_kind with
      | "input" | "cfg_input" -> find_inp_edge edges (node.n_name, port.pt_name) 
      | "output" | "cfg_output" -> find_outp_edge edges (node.n_name, port.pt_name) 
      | _ -> failwith "Hcl.get_port_type" (* should not happen *)
      end in
    e.e_type
    
let output_actor oc edges n =
  let open Ir in
  match n.n_kind with
  | "actor" | "src" | "snk" ->
    let params = List.filter (fun p -> p.pt_kind = "cfg_input") n.n_ports in
    let inps = List.filter (fun p -> p.pt_kind = "input") n.n_ports in
    let outps = List.filter (fun p -> p.pt_kind = "output") n.n_ports in
    let string_of_port p =
      let ty = get_port_type edges n p in
      p.pt_name ^ ": " ^ ty ^ (if p.pt_expr = "" then "" else " \"" ^ p.pt_expr ^ "\"") in 
    begin match n.n_kind, params, inps, outps with
      "actor", [], _, _ ->
       fprintf oc "actor %s\n  in (%s)\n  out (%s);\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " inps)
         (Misc.string_of_list string_of_port ", " outps)
    | "actor", _, _, _ ->
       fprintf oc "actor %s\n  param (%s)\n  in (%s)\n  out (%s);\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " params)
         (Misc.string_of_list string_of_port ", " inps)
         (Misc.string_of_list string_of_port ", " outps)
    | "src", [], _, [p] ->
       fprintf oc "source %s : %s;\n"
         n.n_name
         (get_port_type edges n p)
    | "src", _, _, [p] ->
       fprintf oc "source %s (%s) : %s;\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " params)
         (get_port_type edges n p)
    | "snk", [], [p], _ ->
       fprintf oc "sink %s : %s;\n"
         n.n_name
         (get_port_type edges n p)
    | "snk", _, [p], _ ->
       fprintf oc "sink %s (%s) : %s;\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " params)
         (get_port_type edges n p)
    | _, _, _, _ ->
       failwith "Hcl.dump_actor"
    end
  | _ ->
     ()

let output_parameter oc n =
  let open Ir in
  match n.n_kind with
  | "param" ->
     fprintf oc "parameter %s: %s = %s;\n"
         n.n_name
         cfg.default_param_type
         n.n_desc
  | "cfg_in_iface" ->
     fprintf oc "parameter %s: %s;\n"
         n.n_name
         cfg.default_param_type
  | _ ->
     ()

let output_defn oc names edges n =
  let open Ir in
  let tuplify ios = match ios with
      [] -> "()"
    | [x] -> x
    | _ -> "(" ^ Misc.string_of_list Misc.id "," ios ^ ")" in
  match n.n_kind with
  | "actor" | "src" | "snk" ->
    (* Note. This might have to be adjusted for "src" and "snk" nodes if cfg input ports are not explicitely 
       listed (like in the SIFT examples). To be clarified. *)
    let params = List.filter (fun p -> p.pt_kind = "cfg_input") n.n_ports in
    let inps = List.filter (fun p -> p.pt_kind = "input") n.n_ports in
    let outps = List.filter (fun p -> p.pt_kind = "output") n.n_ports in
    let mk_edge_spec port = n.n_name, port.pt_name in
    let mk_dep_name e =
      try List.assoc (e.e_source, e.e_sourceport) names
      with Not_found -> if e.e_sourceport = "" then e.e_source else e.e_source ^ "_" ^ e.e_sourceport in
    let inp_edges = List.map (find_inp_edge edges) (List.map mk_edge_spec inps) in
    let outp_edges = List.map (find_outp_edge edges) (List.map mk_edge_spec outps) in
    let param_edges = List.map (find_inp_edge edges) (List.map mk_edge_spec params) in
    let actual_inps = List.map mk_dep_name inp_edges |> tuplify in
    let actual_outps = List.map mk_dep_name outp_edges |> tuplify in
    let actual_params = List.map mk_dep_name param_edges |> tuplify in
    begin match params with
      [] ->
       fprintf oc "let %s = %s %s;\n" actual_outps n.n_name actual_inps  
    | _ ->
       fprintf oc "let %s = %s %s %s;\n" actual_outps n.n_name actual_params actual_inps
    end
  | _ ->
     ()
  
let output ch names nodes edges = 
  let types = List.fold_left collect_types [] edges in 
  List.iter (output_type ch) types;
  fprintf ch "\n";
  List.iter (output_parameter ch) nodes;
  fprintf ch "\n";
  List.iter (output_pragma ch) nodes;
  fprintf ch "\n";
  List.iter (output_actor ch edges) nodes;
  fprintf ch "\n";
  List.iter (output_defn ch names edges) nodes

exception Invalid_annotation of string * int
                              
let read_annot_file f =
  let ic = open_in f in
  printf "Reading annotation file %s\n" f; flush stdout;
  let res = ref [] in
  let lineno = ref 0 in
  try 
    while true do      (* Quick and dirty way *)
      let line = input_line ic in
      begin try
        ignore (Scanf.sscanf line "%s %s %s" (fun p s n -> res := ((p,s),n) :: !res));
      with 
        _ -> raise (Invalid_annotation (f,!lineno))
      end;
      incr lineno
    done;
    !res
  with End_of_file ->
    !res
  
let dump annot_file fname p =
  let open Ir in
  let sname = Misc.replace_suffix "pi" fname in
  let names = if annot_file = "" then [] else read_annot_file annot_file in
  let oc = open_out fname  in
  if annot_file = "" then 
    fprintf oc "-- Generated by pi2hcl from file %s\n\n" sname
  else
    fprintf oc "-- Generated by pi2hcl from files %s and %s\n\n" sname annot_file;
  output oc names p.p_nodes p.p_edges;
  close_out oc;
  printf "Generated file %s\n" fname
