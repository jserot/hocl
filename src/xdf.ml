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

(* XDF backend *)

open Printf
open Ssval
open Static

type xdf_config = {
    mutable xml_version: string;
    mutable xml_encoding: string;
    mutable target_package: string;
    mutable top_name: string;
    mutable default_int_size: int;
  }

let cfg = {
  xml_version = "1.0";
  xml_encoding = "UTF-8";
  target_package = "";
  top_name = "";
  default_int_size = 32;
}

type xdf_type = 
    Integer of int
  | Boolean
  | Real
(* to be extended *)

let rec type_of t  = 
  let open Types in 
  match real_type t with
  | TyConstr("bool", []) -> Boolean
  | TyConstr("float", []) -> Real
  | TyConstr("int", []) -> Integer (cfg.default_int_size)
  | TyConstr("nat", []) -> Integer (cfg.default_int_size)
  | _ -> Misc.not_implemented ("XDF translation of type " ^ Pr_type.string_of_type t)

(* let output_io_port oc dir name ty = 
 *   let open Types in
 *   fprintf oc "<Port kind=\"%s\" name=\"%s\">\n" dir name;
 *   begin match type_of ty with
 *   | Integer s ->
 *       fprintf oc "    <Type name=\"int\">\n";
 *       fprintf oc "        <Entry kind=\"Expr\" name=\"size\">\n";
 *       fprintf oc "            <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"%d\"/>\n" s;
 *       fprintf oc "        </Entry>\n";
 *       fprintf oc "    </Type>\n";
 *   | Boolean ->
 *       fprintf oc "    <Type name=\"bool\">\n";
 *       fprintf oc "    </Type>\n"
 *   | Real ->
 *       fprintf oc "    <Type name=\"real\">\n";
 *       fprintf oc "    </Type>\n"
 *   end;
 *   fprintf oc "</Port>\n" *)

(* let output_port oc (i,b) =
 *   match b.ib_tag with
 *     InpB Syntax.StreamIO -> output_io_port oc "Input" b.ib_name b.ib_typ
 *   | OutB Syntax.StreamIO ->output_io_port oc "Output" b.ib_name b.ib_typ
 *   | _ -> () *)

let string_of_type t  = 
  match type_of t with
    Integer s -> "Integer"
  | Boolean -> "Boolean"
  | Real -> "Real"

let string_of_val v = match v with  
    SVNat n -> string_of_int n
  | SVBool b -> string_of_bool b
  | _ -> Misc.not_implemented ("XDF translation of value " ^ Ssval.string_of_ssval v) 

let output_inst_param oc (name,(ty,v)) =
  fprintf oc "  <Parameter name=\"%s\">\n" name; 
  fprintf oc "    <Expr kind=\"Literal\" literal-kind=\"%s\" value=\"%s\"/>\n" (string_of_type ty) (string_of_val v);
  fprintf oc "  </Parameter>\n"

let box_id sp i b = Static.box_name sp (i,b)

let full_actor_name n = match cfg.target_package with
    "" -> n
  | p -> p ^ "." ^ n (* ^ "_act" *)
       
let output_instance oc sp (i,b) =
  match b.b_tag with
  | ActorB ->
      fprintf oc "<Instance id=\"%s\">\n" (box_id sp i b);
      fprintf oc "  <Class name=\"%s\"/>\n" (full_actor_name b.b_name);
      List.iter (output_inst_param oc) b.b_params;
      fprintf oc "</Instance>\n"
  | _ ->
      () 

let output_connexion oc sp (wid,(((s,ss),(d,ds)),ty,is_param_dep))=
  let lookup bid = 
      try List.assoc bid sp.boxes
      with Not_found -> Misc.fatal_error "Xdf.output_connexion" in
  let src_id, src_slot =
    let b = lookup s in
    box_id sp s b, fst (List.nth b.b_outs ss) in
    (*  match b.b_tag with
     *   RegularB -> box_id s b, fst (List.nth b.ib_outs ss)
     * | InpB Syntax.StreamIO -> "", b.ib_name
     * | _ -> Misc.fatal_error "Xdf.output_connexion" in *)
  let dst_id, dst_slot =
    let b = lookup d in
    box_id sp d b, fst (List.nth b.b_ins ds) in
    (*  match b.ib_tag with
     *   RegularB -> box_id d b, fst (List.nth b.ib_ins ds)
     * | OutB Syntax.StreamIO -> "", b.ib_name 
     * | _ -> Misc.fatal_error "Xdf.output_connexion" in *)
  fprintf oc "<Connection dst=\"%s\" dst-port=\"%s\" src=\"%s\" src-port=\"%s\">\n" dst_id dst_slot src_id src_slot;
  (* begin try
   *     match List.assoc ("w" ^ string_of_int wid) anns with
   *     | "fifo_size", sz -> 
   *         fprintf oc "  <Attribute kind=\"Value\" name=\"bufferSize\">\n";
   *         fprintf oc "    <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"%d\"/>\n" sz;
   *         fprintf oc "  </Attribute>\n"
   *     | _, _ -> () 
   * with
   *   Not_found -> () *)
  fprintf oc "</Connection>\n"

let output oc sp = 
  fprintf oc "<?xml version=\"%s\" encoding=\"%s\"?>\n" cfg.xml_version cfg.xml_encoding;
  fprintf oc "<XDF name=\"%s\">\n" cfg.top_name; 
  (* List.iter (output_port oc) sp.b_boxes; *)
  List.iter (output_instance oc sp) sp.boxes;
  List.iter (output_connexion oc sp) sp.wires;
  fprintf oc "</XDF>\n"

let dump fname sp =
  if cfg.top_name = "" then cfg.top_name <- Misc.file_prefix fname;
  let oc = open_out fname in
  output oc sp;
  Logfile.write fname;
  close_out oc
