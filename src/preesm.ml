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

(* Preesm backend *)

open Printf
open Ssval
open Static

type preesm_config = {
    mutable xml_version: string;
    mutable xml_encoding: string;
    mutable top_name: string;
    mutable code_prefix: string;
    mutable algo_dir: string;
    mutable default_port_ann: string;
    mutable default_incl_dir: string;
    mutable default_src_dir: string
  }

let cfg = {
  xml_version = "1.0";
  xml_encoding = "UTF-8";
  top_name = "";
  code_prefix = "";
  algo_dir = "Algo";
  default_port_ann = "1";
  default_incl_dir = "../include";
  default_src_dir = "../src"
}

exception Error of string
        
(* type preesm_type = 
 *   | Integer of bool * int option (\* signed, width *\)
 *   | Char of bool (\* signed *\)
 *   | Boolean
 *   | Float
 *   | Double
 * (\* to be extended *\)
 * 
 * let rec type_of t  = 
 *   let open Types in 
 *   match real_type t with
 *   | TyConstr("bool", []) -> Boolean
 *   | TyConstr("float"}, _, _) -> if cfg.use_floats then Float else Double
 *   | TyConstr({tc_name="int"}, [sg], [sz]) ->
 *       begin match real_type sg, size_repr sz with
 *       | TyConstr({tc_name="_unsigned"},_,_), SzConst s -> Integer (false, Some s)
 *       | TyConstr({tc_name="_signed"},_,_), SzConst s -> Integer (true, Some s)
 *       | TyConstr({tc_name="_signed"},_,_), _ -> Integer (true, None)
 *       | TyConstr({tc_name="_unsigned"},_,_), _ -> Integer (false, None)
 *       | _, _ -> Integer (false, None)
 *       end
 *   | ty -> Misc.not_implemented ("PREESM translation of type " ^ Pr_type.string_of_type t) *)

let string_of_type t  = 
  match Types.real_type t with
  | TyConstr ("nat", []) -> "int"
  | TyConstr ("bool", []) -> "bool"
  | TyConstr (n, []) -> n
  | _ -> Misc.not_implemented ("PREESM translation of type " ^ Pr_type.string_of_type t)

let string_of_val v = match v with  
    SVNat n -> string_of_int n
  | SVBool b -> string_of_bool b
  | _ -> Misc.not_implemented ("PREESM translation of value " ^ Ssval.string_of_ssval v) 

let rec string_of_expr e =
  match e.Syntax.ne_desc with
    Syntax.NNat n -> string_of_int n
  | Syntax.NBool b -> string_of_bool b
  | Syntax.NVar v -> v
  | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar op},{Syntax.ne_desc=Syntax.NTuple [e1;e2]}) when Syntax.is_binop op -> 
      string_of_expr' e1 ^ op ^ string_of_expr' e2
  | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar op},e) when Syntax.is_unop op -> op ^ "(" ^ string_of_expr e ^ ")"
  (* | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar f}, es) ->
   *     f ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")" *)
  | _ -> Misc.not_implemented ("Preesm.string_of_expr: " ^  (Syntax.string_of_net_expr e))

and string_of_expr' e =
  if is_simple_expr e then string_of_expr e else "(" ^ string_of_expr e ^ ")"

and is_simple_expr e = match e.Syntax.ne_desc with
    Syntax.NNat _ -> true
  | Syntax.NBool _ -> true
  | Syntax.NVar v -> true
  | _ -> false

let lookup_box boxes bid = 
      try List.assoc bid boxes
      with Not_found -> Misc.fatal_error "Preesm.lookup_box"

let lookup_wire wires wid = 
      try List.assoc wid wires
      with Not_found -> Misc.fatal_error "Preesm.lookup_wire"

let is_param_box b = b.b_tag = LocalParamB || b.b_tag = InParamB

let output_actor_box_io oc dir is_config id ty =                            
  fprintf oc "          <param direction=\"%s\" isConfig=\"%s\" name=\"%s\" type=\"%s\"/>\n"
    dir (string_of_bool is_config) id (string_of_type ty)

let output_actor_box_inp oc is_param (id, (wid,ty,ann)) =
  output_actor_box_io oc "IN" is_param id ty

let output_actor_box_outp oc (id, (wids,ty,ann)) =
  output_actor_box_io oc "OUT" false (* TO FIX *) id ty

let output_actor_box_iinit oc (id, (wid,ty,ann)) =
  fprintf oc "        <param direction=\"IN\" isConfig=\"true\" name=\"%s\" type=\"%s\"/>\n" id (string_of_type ty)

let output_actor_box_port oc dir is_param (id, (wid,ty,ann)) =
  if is_param then
    fprintf oc "      <port kind=\"cfg_%s\" name=\"%s\"/>\n" dir id
  else
    fprintf oc "      <port kind=\"%s\" name=\"%s\" expr=\"%s\" annotation=\"NONE\"/>\n"
      dir id (if ann = "" then cfg.default_port_ann else ann)

let output_actor_box oc sp (i,b) =
  let is_param (id, (wid,ty,ann)) = 
    let w = lookup_wire sp.wires wid in
    is_param_box (Static.src_box_of_wire sp.boxes w) in
  match b.b_tag with
  | ActorB ->
     let id = box_name sp (i,b) in 
     let incl_file, loop_fn, init_fn, period = 
       begin match get_pragma_desc "code" b.b_name sp with 
       | [incl_file; loop_fn] -> incl_file, loop_fn, "", "0"
       | [incl_file; loop_fn; init_fn] -> incl_file, loop_fn, init_fn, "0"
       | _ -> Error.no_pragma_desc b.b_name; cfg.default_incl_dir ^ "/" ^ b.b_name ^ ".h", b.b_name, "", "0"
       end in
     fprintf oc "    <node id=\"%s\" kind=\"actor\" period=\"%s\">\n" id period;
     fprintf oc "      <data key=\"graph_desc\">%s</data>\n" incl_file;
     fprintf oc "      <loop name=\"%s\">\n" loop_fn;
     let param_ins, fifo_ins = List.partition is_param b.b_ins in 
     List.iter (output_actor_box_inp oc true) param_ins;
     List.iter (output_actor_box_inp oc false) fifo_ins;
     List.iter (output_actor_box_outp oc) b.b_outs;
     fprintf oc "      </loop>\n";
     if init_fn <> "" then  begin
         fprintf oc "      <init name=\"%s\">\n" init_fn;
         List.iter (output_actor_box_iinit oc) param_ins;
         fprintf oc "      </init>\n";
       end;
     List.iter (output_actor_box_port oc "input" true) param_ins;
     List.iter (output_actor_box_port oc "input" false) fifo_ins;
     List.iter (output_actor_box_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | GraphB ->
     let id = box_name sp (i,b) in 
     let incl_file = 
       begin match get_pragma_desc "code" b.b_name sp with 
       | [incl_file] -> incl_file
       | _ -> raise (Error ("cannot find valid #pragma description for actor " ^ b.b_name))
       end in
     fprintf oc "    <node id=\"%s\" kind=\"actor\">\n" id;
     fprintf oc "      <data key=\"graph_desc\">%s/%s</data>\n" cfg.algo_dir (Misc.replace_suffix "pi" incl_file);
     let param_ins, fifo_ins = List.partition is_param b.b_ins in 
     List.iter (output_actor_box_port oc "input" true) param_ins;
     List.iter (output_actor_box_port oc "input" false) fifo_ins;
     List.iter (output_actor_box_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | BcastB ->
     let id = box_name sp (i,b) in 
     fprintf oc "    <node id=\"%s\" kind=\"broadcast\">\n" id;
     let param_ins, fifo_ins = List.partition is_param b.b_ins in 
     List.iter (output_actor_box_port oc "input" true) param_ins;
     List.iter (output_actor_box_port oc "input" false) fifo_ins;
     List.iter (output_actor_box_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | SourceB ->
     let id = box_name sp (i,b) in 
     fprintf oc "    <node id=\"%s\" kind=\"src\">\n" id;
     List.iter (output_actor_box_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | SinkB ->
     let id = box_name sp (i,b) in 
     fprintf oc "    <node id=\"%s\" kind=\"snk\">\n" id;
     List.iter (output_actor_box_port oc "input" false) b.b_ins;
     fprintf oc "    </node>\n"
  | _ ->
      () 

let output_parameter oc sp (i,b) =
  match b.b_tag, b.b_val with
  | LocalParamB, v ->
     fprintf oc "    <node expr=\"%s\" id=\"%s\" kind=\"param\"/>\n" (string_of_expr v.bv_lit) b.b_name 
  | InParamB, _ ->
     fprintf oc "    <node id=\"%s\" kind=\"cfg_in_iface\"/>\n" b.b_name 
  | _ -> ()

let output_connexion oc sp (wid,(((s,ss),(d,ds)),ty,is_param_dep))=
  let src_name, src_slot =
    let b = lookup_box sp.boxes s in
    match b.b_tag with
    | ActorB | BcastB | GraphB -> box_name sp (s,b), fst (List.nth b.b_outs ss)
    | LocalParamB | InParamB  -> box_name sp (s,b), ""
    | SourceB -> box_name sp (s,b), box_name sp (s,b)
    | _ -> Misc.fatal_error "Preesm.output_connexion" in
  let dst_name, dst_slot =
    let b = lookup_box sp.boxes d in
    match b.b_tag with
    | ActorB | BcastB | GraphB -> box_name sp (d,b), fst (List.nth b.b_ins ds)
    | LocalParamB -> box_name sp (d,b), ""
    | SinkB | SourceB -> box_name sp (d,b), box_name sp (d,b) 
    | _ -> Misc.fatal_error "Preesm.output_connexion" in
  let mk_field name v = if v = "" then "" else Printf.sprintf "%s=\"%s\"" name v in
  fprintf oc "    <edge kind=\"%s\" source=\"%s\" %s target=\"%s\" %s %s/>\n"
    (if is_param_dep then "dependency" else "fifo")
    src_name
    (mk_field "sourceport" src_slot)
    dst_name
    (mk_field "targetport" dst_slot)
    (mk_field "type" (if is_param_dep then "" else string_of_type ty))

let output oc name sp = 
  fprintf oc "<?xml version=\"%s\" encoding=\"%s\"?>\n" cfg.xml_version cfg.xml_encoding;
  fprintf oc "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">\n";
  fprintf oc "  <key attr.name=\"parameters\" for=\"graph\" id=\"parameters\"/>\n";
  fprintf oc "  <key attr.name=\"variables\" for=\"graph\" id=\"variables\"/>\n";
  fprintf oc "  <key attr.name=\"arguments\" for=\"node\" id=\"arguments\"/>\n";
  fprintf oc "  <key attr.name=\"name\" attr.type=\"string\" for=\"graph\"/>\n";
  fprintf oc "  <key attr.name=\"graph_desc\" attr.type=\"string\" for=\"node\"/>\n";
  fprintf oc "  <graph edgedefault=\"directed\">\n";
  fprintf oc "    <data key=\"name\">%s</data>\n" name;
  List.iter (output_parameter oc sp) sp.boxes;
  List.iter (output_actor_box oc sp) sp.boxes;
  List.iter (output_connexion oc sp) sp.wires;
  fprintf oc "  </graph>\n";
  fprintf oc "</graphml>\n"

let dump fname sp =
  let name = if cfg.top_name = "" then Misc.file_prefix fname else cfg.top_name in
  let oc = open_out fname in
  output oc name sp;
  Logfile.write fname;
  close_out oc
