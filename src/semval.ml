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

(* Semantic values *)

open Types
open Pr_type

type sem_val =
  | SVInt of int
  | SVBool of bool
  | SVUnit
  | SVTuple of sem_val list
  | SVClos of sv_clos
  | SVPrim of (sem_val -> sem_val)
  | SVCons of sem_val * sem_val
  | SVList of sem_val list
  | SVNil
  | SVNode of sv_node * sem_val list (* node, with param values *)
  | SVLoc of idx * sel * typ * sv_tag (* node index, output selector, type, tag *)
  | SVWire of idx * sv_wire

and sv_tag =
  | SV_Source
  | SV_Sink
  | SV_Param
  | SV_None

and sv_clos = {
  cl_pat: Syntax.net_pattern;
  cl_exp: Syntax.net_expr;
  mutable cl_env: (string * sem_val) list
  }

and idx = int
and sel = int

and sv_loc = idx * sel

and sv_node = {   (* Node model (actor or (sub)graph) *)
  sn_id: string;
  sn_kind: sv_node_kind;
  sn_params: (string * typ) list;
  sn_ins: (string * typ * Syntax.io_annot list) list;
  sn_outs: (string * typ * Syntax.io_annot list) list;
  sn_typ: typ_scheme; (* TO SEE : rather [typ] ? Does it make sense to have polymorphic nodes ? *)
  }

and sv_node_kind =
  | SV_Actor
  (* | SV_Bcast *)
  | SV_Graph
                           
and sv_wire = (sv_loc * sv_loc) * typ (** wire_kind*)   (* src, dest, type *)

(* and wire_kind =
 *   | DataW   (\* Data dependency *\)
 *   | ParamW  (\* Parameter dependency *\)
 *   (\* | DelayW  (\\* Special case for handling delays in the Preesm backend *\\) *\) *)

let sv_no_loc = -1, -1

(* let new_wire ty kind = (sv_no_loc,sv_no_loc), ty, kind *)
let new_wire ty = (sv_no_loc,sv_no_loc), ty
                     
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

(* Printing *)

let rec output_sem_value oc v = output_string oc (string_of_semval v)

and  string_of_semval v = match v with
  | SVInt v -> string_of_int v
  | SVBool v -> string_of_bool v
  | SVUnit -> "()"
  | SVNil -> "[]"
  | SVCons (v1,v2) -> string_of_semval v1 ^ "::" ^ string_of_semval v2
  | SVLoc (l,s,ty,_) -> "Loc(" ^ string_of_int l ^ "," ^ string_of_int s 
  | SVPrim p -> "Prim(...)"
  | SVNode _ -> "Node(...)"
  | SVClos _ -> "Clos(...)"
  | SVTuple vs -> "(" ^ Misc.string_of_list string_of_semval "," vs ^ ")"
  | SVList vs -> "[" ^ Misc.string_of_list string_of_semval "," vs ^ "]"
  | SVWire (id, ((l,l'),ty)) ->
     "Wire(" ^ string_of_svloc l ^ "," ^ string_of_svloc l' ^ "," ^ string_of_type ty ^ ")"

and string_of_svloc (l,s) =  "(" ^ string_of_int l ^ "," ^ string_of_int s ^ ")"
(* and string_of_wire_kind = function DataW -> "data" | ParamW -> "param" (\* | DelayW -> "delay" *\) *)

and output_sem_val_list oc sep l = Misc.output_list output_value oc sep l
