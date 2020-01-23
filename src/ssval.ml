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

(* Static semantic domain *)

open Types
open Pr_type

type ss_val =
  | SVInt of int
  | SVBool of bool
  | SVUnit
  | SVTuple of ss_val list
  | SVClos of sv_clos
  | SVPrim of (ss_val -> ss_val)
  | SVCons of ss_val * ss_val
  | SVList of ss_val list
  | SVNil
  | SVAct of sv_box * ss_val list (* param values *)
  | SVGraph of sv_box * ss_val list (* param values *)
  | SVLoc of idx * sel * typ * sv_tag (* node index, output selector, type, tag *)
  (* | SVLoc of idx * sel * typ * ss_val (\* node index, output selector, type, parameter value (when applicable) *\) *)
  | SVWire of idx * sv_wire

and sv_tag =
  | SV_Source
  | SV_Sink
  | SV_Param
  | SV_None

and sv_clos =
  { cl_pat: Syntax.net_pattern;
    cl_exp: Syntax.net_expr;
    mutable cl_env: (string * ss_val) list }

and idx = int
and sel = int

and sv_loc = idx * sel

and sv_box = {   (* Instanciable "box" (actor or graph) *)
    sb_id: string;
    sb_kind: sv_box_kind;
    sb_params: (string * typ) list;
    sb_ins: (string * typ * Syntax.io_annot list) list;
    sb_outs: (string * typ * Syntax.io_annot list) list;
    sb_typ: typ_scheme;
}

and sv_box_kind = BRegular | BBcast
                           
and sv_wire = (sv_loc * sv_loc) * typ * wire_kind   (* src, dest, type, kind *)

and wire_kind =
  | DataW   (* Data dependency *)
  | ParamW  (* Parameter dependency *)
  (* | DelayW  (\* Special case for handling delays in the Preesm backend *\) *)

let sv_no_loc = -1, -1

let new_wire ty kind = (sv_no_loc,sv_no_loc), ty, kind
                     
let is_static_const = function
    SVInt _ | SVBool _ -> true
  | _ -> false

let list_of_cons v =
  let rec h = function
    | SVCons (v1,v2) -> v1 :: h v2
    | SVNil -> []
    | _ -> Misc.fatal_error "Ssval.list_of_cons" in
  SVList (h v)

let cons_of_list v =
  let rec h = function
  | [] -> SVNil
  | v::vs -> SVCons (v, h vs) in
  match v with
  | SVList l -> h l
  | _ -> Misc.fatal_error "Ssval.cons_of_list"

let rec size_of_ssval v = match v with
  | SVNil -> 0
  | SVCons (_,v) -> 1 + size_of_ssval v
  | SVTuple vs -> List.length vs
  | _ -> 1

(* Printing *)

let rec output_ss_value oc v = output_string oc (string_of_ssval v)

and  string_of_ssval v = match v with
  | SVInt v -> string_of_int v
  | SVBool v -> string_of_bool v
  | SVUnit -> "()"
  | SVNil -> "[]"
  | SVCons (v1,v2) -> string_of_ssval v1 ^ "::" ^ string_of_ssval v2
  | SVLoc (l,s,ty,_) -> "Loc(" ^ string_of_int l ^ "," ^ string_of_int s 
  | SVPrim p -> "Prim(...)"
  | SVAct _ -> "Actor(...)"
  | SVGraph _ -> "Graph(...)"
  | SVClos _ -> "Clos(...)"
  | SVTuple vs -> "(" ^ Misc.string_of_list string_of_ssval "," vs ^ ")"
  | SVList vs -> "[" ^ Misc.string_of_list string_of_ssval "," vs ^ "]"
  | SVWire (id, ((l,l'),ty,kind)) ->
     "Wire(" ^ string_of_svloc l ^ "," ^ string_of_svloc l' ^ "," ^ string_of_type ty ^ "," ^ string_of_wire_kind kind  ^ ")"

and string_of_svloc (l,s) =  "(" ^ string_of_int l ^ "," ^ string_of_int s ^ ")"
and string_of_wire_kind = function DataW -> "data" | ParamW -> "param" (* | DelayW -> "delay" *)

and output_ss_val_list oc sep l = Misc.output_list output_value oc sep l
