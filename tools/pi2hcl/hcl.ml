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
  
let output_pragma oc n =
  let open Ir in
  match n.n_kind, n.n_init_fn with
  | "actor", "" ->
    fprintf oc "#pragma code(\"%s\", \"%s\", \"%s\");\n" n.n_name n.n_desc n.n_loop_fn
  | "actor", _ ->
    fprintf oc "#pragma code(\"%s\", \"%s\", \"%s\", \"%s\");\n" n.n_name n.n_desc n.n_loop_fn n.n_init_fn
  | _, _ ->
     ()

let output_actor oc n =
  let open Ir in
  match n.n_kind with
  | "actor" ->
    let params = List.filter (fun p -> p.pt_kind = "cfg_input") n.n_ports in
    let inps = List.filter (fun p -> p.pt_kind = "input") n.n_ports in
    let outps = List.filter (fun p -> p.pt_kind = "output") n.n_ports in
    let get_io name =
      try List.find (fun io -> io.io_name = name) n.n_ios
      with Not_found -> failwith "Hcl.output_actor" in
    let string_of_port p =
      let io = get_io p.pt_name in
      p.pt_name ^ ": " ^ io.io_type ^ (if p.pt_expr = "" then "" else " \"" ^ p.pt_expr ^ "\"") in 
    begin match params with
      [] ->
       fprintf oc "actor %s\n  in (%s)\n  out (%s);\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " inps)
         (Misc.string_of_list string_of_port ", " outps)
    | _ ->
       fprintf oc "actor %s\n  param (%s)\n  in (%s)\n  out (%s);\n"
         n.n_name
         (Misc.string_of_list string_of_port ", " params)
         (Misc.string_of_list string_of_port ", " inps)
         (Misc.string_of_list string_of_port ", " outps)
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
  | _ ->
     ()

let output_defn oc names edges n =
  let open Ir in
  let tuplify ios = match ios with
      [] -> "()"
    | [x] -> x
    | _ -> "(" ^ Misc.string_of_list Misc.id "," ios ^ ")" in
  let is_param_inp io = io.io_direction = "IN" && io.io_isConfig = "true" in
  let is_data_inp io = io.io_direction = "IN" && io.io_isConfig = "false" in
  let is_data_outp io = io.io_direction = "OUT" && io.io_isConfig = "false" in
  match n.n_kind with
  | "actor" ->
    let params = List.filter is_param_inp n.n_ios in
    let inps = List.filter is_data_inp n.n_ios in
    let outps = List.filter is_data_outp n.n_ios in
    let mk_edge_spec io = n.n_name, io.io_name in
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
  List.iter (output_actor ch) nodes;
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
  let names = if annot_file = "" then [] else read_annot_file annot_file in
  let oc = open_out fname  in
  if annot_file = "" then 
    fprintf oc "-- Generated by pi2hcl from file %s\n\n" fname
  else
    fprintf oc "-- Generated by pi2hcl from files %s and %s\n\n" fname annot_file;
  output oc names p.p_nodes p.p_edges;
  close_out oc;
  printf "Generated file %s\n" fname
