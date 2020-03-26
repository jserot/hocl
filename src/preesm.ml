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
open Interm
open Backend

type preesm_config = {
    mutable xml_version: string;
    mutable xml_encoding: string;
    mutable top_name: string;
    mutable code_prefix: string;
    mutable algo_dir: string;
    mutable default_port_rate: string;
    mutable default_incl_dir: string;
    mutable default_src_dir: string
  }

let cfg = {
  xml_version = "1.0";
  xml_encoding = "UTF-8";
  top_name = "";
  code_prefix = "";
  algo_dir = "Algo";
  default_port_rate = "1";
  default_incl_dir = "../include";
  default_src_dir = "../src"
}

(* exception Error of string *)
        
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
  | TyConstr ("int", []) -> "int"
  | TyConstr ("bool", []) -> "bool"
  | TyConstr (n, []) -> n
  | _ -> Misc.not_implemented ("PREESM translation of type " ^ Pr_type.string_of_type t)

(* let string_of_val v = match v with  
 *     SVInt n -> string_of_int n
 *   | SVBool b -> string_of_bool b
 *   | _ -> Misc.not_implemented ("PREESM translation of value " ^ Ssval.string_of_ssval v)  *)

(* let rec string_of_expr e =
 *   match e.Syntax.ne_desc with
 *     Syntax.NInt n -> string_of_int n
 *   | Syntax.NBool b -> string_of_bool b
 *   | Syntax.NVar v -> v
 *   | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar op},{Syntax.ne_desc=Syntax.NTuple [e1;e2]}) when Syntax.is_binop op -> 
 *       string_of_expr' e1 ^ op ^ string_of_expr' e2
 *   | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar op},e) when Syntax.is_unop op -> op ^ "(" ^ string_of_expr e ^ ")"
 *   (\* | Syntax.NApp ({Syntax.ne_desc=Syntax.NVar f}, es) ->
 *    *     f ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")" *\)
 *   | _ -> Misc.not_implemented ("Preesm.string_of_expr: " ^  (Syntax.string_of_net_expr e))
 * 
 * and string_of_expr' e =
 *   if is_simple_expr e then string_of_expr e else "(" ^ string_of_expr e ^ ")"
 * 
 * and is_simple_expr e = match e.Syntax.ne_desc with
 *     Syntax.NInt _ -> true
 *   | Syntax.NBool _ -> true
 *   | Syntax.NVar v -> true
 *   | _ -> false *)

let box_name i b =
  match b.b_tag with
  | LocalParamB | InParamB | SourceB | SinkB -> b.b_name
  | _ -> b.b_name ^ "_" ^ string_of_int i

(* let is_param_box b = b.b_tag = LocalParamB || b.b_tag = InParamB *)

let dump_actor_io oc dir is_config id ty =                            
  fprintf oc "          <param direction=\"%s\" isConfig=\"%s\" name=\"%s\" type=\"%s\"/>\n"
    dir (string_of_bool is_config) id (string_of_type ty)

let dump_actor_inp oc is_param (id, (wid,ty,ann)) =
  dump_actor_io oc "IN" is_param id ty

let dump_actor_outp oc (id, (wids,ty,ann)) =
  dump_actor_io oc "OUT" false (* TO FIX *) id ty

let dump_actor_iinit oc (id, (wid,ty,ann)) =
  fprintf oc "        <param direction=\"IN\" isConfig=\"true\" name=\"%s\" type=\"%s\"/>\n" id (string_of_type ty)

let dump_actor_port oc dir is_param (id, (wid,ty,annots)) =
  if is_param then
    fprintf oc "      <port kind=\"cfg_%s\" name=\"%s\"/>\n" dir id
  else
    let rate, other_anns =
      List.fold_left
        (fun (acc,acc') ann ->
          match ann with
          | Syntax.IA_Rate e -> Some e, acc'
          | Syntax.IA_Other s -> acc, s::acc')
        (None,[])
        annots in
    fprintf oc "      <port kind=\"%s\" name=\"%s\" expr=\"%s\" annotation=\"%s\"/>\n"
      dir
      id
      (match rate with Some e -> Syntax.string_of_rate_expr e | None -> cfg.default_port_rate)
      (Misc.string_of_list Misc.id " " other_anns)

let dump_actor oc ir g (i,b)  =
  let param_ins, data_ins = List.partition (is_param_input g.sg_wires) b.b_ins in 
  match b.b_tag with
  | ActorB ->
     let id = box_name i b in 
     let intf, impls =
       match List.assoc b.b_name ir.ir_nodes with
       | { sn_intf=intf; sn_impl=NI_Actor impl } -> intf, impl
       | { sn_intf=intf; sn_impl=_ } -> Misc.fatal_error "Preesm.dump_actor" (* should not happen *)
       | exception Not_found -> Error.missing_actor_impl "preesm" b.b_name in
     let attrs =
       try List.assoc "preesm" impls
       with Not_found -> Error.missing_actor_impl "preesm" b.b_name in
     let incl_file, loop_fn, init_fn, is_delay, param_exec = get_impl_fns "preesm" id attrs in
     let period = "0" in (* TO FIX *)
     fprintf oc "    <node id=\"%s\" kind=\"actor\" period=\"%s\">\n" id period;
     fprintf oc "      <data key=\"graph_desc\">%s</data>\n" incl_file;
     fprintf oc "      <loop name=\"%s\">\n" loop_fn;
     List.iter (dump_actor_inp oc true) param_ins;
     List.iter (dump_actor_inp oc false) data_ins;
     List.iter (dump_actor_outp oc) b.b_outs;
     fprintf oc "      </loop>\n";
     if init_fn <> "" then  begin
         fprintf oc "      <init name=\"%s\">\n" init_fn;
         List.iter (dump_actor_iinit oc) param_ins;
         fprintf oc "      </init>\n";
       end;
     List.iter (dump_actor_port oc "input" true) param_ins;
     List.iter (dump_actor_port oc "input" false) data_ins;
     List.iter (dump_actor_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | GraphB ->
     let id = box_name i b in 
     (* let intf, impl =
      *   match List.assoc b.b_name ir.ir_nodes with
      *   | { sn_intf=intf; sn_impl=NI_Graph g } -> intf, g
      *   | { sn_intf=intf; sn_impl=_ } -> Misc.fatal_error "Preesm.dump_actor" (\* should not happen *\)
      *   | exception Not_found -> Error.missing_actor_impl "preesm" b.b_name in *)
     let incl_file = b.b_name ^ ".pi" in
     fprintf oc "    <node id=\"%s\" kind=\"actor\">\n" id;
     fprintf oc "      <data key=\"graph_desc\">%s/%s</data>\n" cfg.algo_dir incl_file;
     List.iter (dump_actor_port oc "input" true) param_ins;
     List.iter (dump_actor_port oc "input" false) data_ins;
     List.iter (dump_actor_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | EBcastB
  | IBcastB ->
     let id = box_name i b in 
     fprintf oc "    <node id=\"%s\" kind=\"broadcast\">\n" id;
     List.iter (dump_actor_port oc "input" true) param_ins;
     List.iter (dump_actor_port oc "input" false) data_ins;
     List.iter (dump_actor_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  (* | DelayB ->
   *    let id = box_name i b in 
   *    let rate = match data_ins with
   *        [_,(_,_,r,_)] -> r
   *      | _ -> Error.illegal_interface "delay" b.b_name " (there should be exactly one input)" in
   *    fprintf oc "    <node id=\"%s\" kind=\"delay\" getter=\"\" setter=\"\" level=\"permanent\" expr=\"%s\">\n"
   *      id
   *      (Syntax.string_of_io_rate rate);
   *    (\* List.iter (dump_actor_port oc "input" true) param_ins; *\) (\* No explicit parameters for delays ? *\)
   *    List.iter (dump_actor_port oc "input" false) data_ins; 
   *    List.iter (dump_actor_port oc "output" false) b.b_outs;
   *    fprintf oc "    </node>\n" *)
  | SourceB ->
     let id = box_name i b in 
     fprintf oc "    <node id=\"%s\" kind=\"src\">\n" id;
     List.iter (dump_actor_port oc "output" false) b.b_outs;
     fprintf oc "    </node>\n"
  | SinkB ->
     let id = box_name i b in 
     fprintf oc "    <node id=\"%s\" kind=\"snk\">\n" id;
     List.iter (dump_actor_port oc "input" false) b.b_ins;
     fprintf oc "    </node>\n"
  | _ ->
      () 

let dump_parameter oc ~toplevel (i,b) =
  match b.b_tag, b.b_val, toplevel with
  | LocalParamB, v, _ ->
     fprintf oc "    <node expr=\"%s\" id=\"%s\" kind=\"param\"/>\n" (Syntax.string_of_core_expr v.bv_lit) b.b_name 
  | InParamB, v, true ->
     fprintf oc "    <node expr=\"%s\" id=\"%s\" kind=\"param\"/>\n" (Syntax.string_of_core_expr v.bv_lit) b.b_name 
  | InParamB, _, false ->
     fprintf oc "    <node id=\"%s\" kind=\"cfg_in_iface\"/>\n" b.b_name 
  | _ -> ()


let dump_connexion oc boxes (wid,((((s,ss),(d,ds)),ty) as w))=
  let src_name, src_slot =
    let b = get_src_box boxes w in
    match b.b_tag with
    | ActorB | EBcastB | IBcastB | GraphB (*| DelayB*) -> box_name s b, fst (List.nth b.b_outs ss)
    | LocalParamB | InParamB  -> box_name s b, ""
    | SourceB -> box_name s b, box_name s b
    | _ -> Misc.fatal_error "Preesm.output_connexion" in
  let dst_name, dst_slot =
    let b = get_dst_box boxes w in
    match b.b_tag with
    | ActorB | EBcastB | IBcastB | GraphB (*| DelayB*) -> box_name d b, fst (List.nth b.b_ins ds)
    | LocalParamB -> box_name d b, ""
    | SinkB | SourceB -> box_name d b, box_name d b 
    | _ -> Misc.fatal_error "Preesm.output_connexion" in
  let mk_field name v = if v = "" then "" else Printf.sprintf "%s=\"%s\"" name v in
  (* match kind with
   * | Semval.DataW | Semval.ParamW -> *)
     fprintf oc "    <edge kind=\"%s\" source=\"%s\" %s target=\"%s\" %s %s/>\n"
       (* (match kind with ParamW -> "dependency" | _ -> "fifo") *)
       (if Types.is_param_type ty then "dependency" else "fifo")
       src_name
       (mk_field "sourceport" src_slot)
       dst_name
       (mk_field "targetport" dst_slot)
       (* (mk_field "type" (match kind with DataW (\*| DelayW *\) -> string_of_type ty | ParamW -> "")) *)
       (mk_field "type" (if Types.is_param_type ty then "" else string_of_type ty))
  (* | DelayW ->
   *    fprintf oc "    <edge kind=\"%s\" source=\"%s\" %s target=\"%s\" %s %s>\n"
   *      "fifo"
   *      src_name
   *      (mk_field "sourceport" src_slot)
   *      dst_name
   *      (mk_field "targetport" dst_slot)
   *      (mk_field "type" (match kind with DataW | DelayW -> string_of_type ty | ParamW -> ""));
   *    fprintf oc "      <data key=\"delay\">%s</data>\n" "Delay";
   *    fprintf oc "    </edge>\n" *)

(* let is_regular_connexion sp (wid,(((s,ss),(d,ds)),ty,kind))=
 *   (\* Note: [kind] is still [DataW] before [shorten_delay_connexions] *\) 
 *     let bs = lookup_box sp.boxes s in
 *     let bd = lookup_box sp.boxes d in
 *     bs.b_tag <> DelayB && bd.b_tag <> DelayB *)

(* let shorten_delay_connexions sp cnxs =
 *   (\* ActorB1 -w1<regular>-> DelayB -w2<regular>-> ActorB2 ==> ActorB1 -w<delay>-> ActorB2 *\)
 *   let scan acc (wid,(((s,ss),(d,ds)),ty,kind)) =
 *     let b = lookup_box sp.boxes d in
 *     if b.b_tag = DelayB && kind=DataW then (\* For each DelayB box *\)
 *       let b1,bs1 = s,ss in   (\* Get source box *\)
 *       let _,(((_,_),(b2,bs2)),_,_) = (\* Get destination box *\)
 *         try List.find (function (wid,(((s,ss),(_,_)),ty,kind)) -> s=d) cnxs 
 *         with Not_found -> Misc.fatal_error "Preesm.shorten_delay_connexions" in (\* Should not happen *\)
 *       let w' = ((b1,bs1),(b2,bs2)), ty, DelayW in (\* Created wire *\)
 *       (new_wid(),w') :: acc
 *     else
 *       acc in
 *   List.fold_left scan [] cnxs *)

let is_actor_box (_,b) = match b.b_tag with
  | ActorB | EBcastB | GraphB | SourceB | SinkB  -> true
  | _ -> false

let is_param_box (_,b) = match b.b_tag with 
  | LocalParamB | InParamB -> true
  | _ -> false
       
let dump_graph ~toplevel path prefix ir id intf g = 
  let fname = path ^ id ^ ".pi" in
  let oc = open_out fname in
  fprintf oc "<?xml version=\"%s\" encoding=\"%s\"?>\n" cfg.xml_version cfg.xml_encoding;
  fprintf oc "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">\n";
  fprintf oc "  <key attr.name=\"parameters\" for=\"graph\" id=\"parameters\"/>\n";
  fprintf oc "  <key attr.name=\"variables\" for=\"graph\" id=\"variables\"/>\n";
  fprintf oc "  <key attr.name=\"arguments\" for=\"node\" id=\"arguments\"/>\n";
  fprintf oc "  <key attr.name=\"name\" attr.type=\"string\" for=\"graph\"/>\n";
  fprintf oc "  <key attr.name=\"graph_desc\" attr.type=\"string\" for=\"node\"/>\n";
  fprintf oc "  <key attr.name=\"delay\" attr.type=\"string\" for=\"edge\"/>\n";
  fprintf oc "  <graph edgedefault=\"directed\">\n";
  fprintf oc "    <data key=\"name\">%s</data>\n" id;
  let actors = List.filter is_actor_box g.sg_boxes in
  let parameters = List.filter is_param_box g.sg_boxes in
  List.iter (dump_parameter ~toplevel oc) parameters;
  List.iter (dump_actor oc ir g) actors;
  let regular_cnxs, delay_cnxs = g.sg_wires, [] in
  (* let regular_cnxs, delay_cnxs =
   *   List.fold_left
   *     (fun (acc,acc') ((wid,(((s,ss),(d,ds)),ty,kind)) as w) ->
   *       match (lookup_box ir.boxes s).b_tag, (lookup_box ir.boxes d).b_tag, kind with
   *       | DelayB, _, DataW -> acc, w::acc' (\* DelayB-> *\)
   *       | _, DelayB, DataW -> acc, w::acc' (\* ->DelayB *\)
   *       | _, _, _ -> w::acc, acc')            (\* other *\)
   *     ([],[])
   *     ir.wires in *)
  List.iter (dump_connexion oc g.sg_boxes) regular_cnxs;
  (* let delay_cnxs' = shorten_delay_connexions ir delay_cnxs in
   * List.iter (output_connexion oc ir) delay_cnxs'; *)
  fprintf oc "  </graph>\n";
  fprintf oc "</graphml>\n";
  Logfile.write fname;
  close_out oc

let dump_top_graph path prefix ir (id,g) =
  (* let name = if cfg.top_name = "" then Misc.file_prefix fname else cfg.top_name in *)
  dump_graph ~toplevel:true path prefix ir id g.tg_intf g.tg_impl

let dump path prefix ir =
  List.iter (dump_top_graph path prefix ir) ir.ir_graphs;
  List.iter
    (fun (id,(intf,g)) -> dump_graph ~toplevel:false path prefix ir id intf g)
    (collect_sub_graphs ir)
  
