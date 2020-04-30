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

(* Semantic values for evaluation of functional graph descriptions *)

type sem_val =
  | SVLoc of sv_loc
  | SVNode of sv_node
  | SVTuple of sem_val list
  | SVClos of sv_clos
  | SVUnit
  | SVInt of int
  | SVBool of bool
  | SVPrim of (sem_val -> sem_val)
  | SVNil
  | SVCons of sem_val * sem_val
  | SVList of sem_val list
  | SVWire of wid

and sv_clos = {
  cl_pat: Syntax.pattern;
  cl_exp: Syntax.expr;
  mutable cl_env: (string * sem_val) list
  }

and sem_env = (string * sem_val) list  (** E *)
            
(* Graph locations *)
            
and sv_loc = bid * sel * Types.typ (* box index, slot number, typ *) (** ell *)

(* Nodes *)

and sv_node = {
  sn_id: string;
  sn_kind: node_kind;
  sn_params: (string * sem_val * Types.typ * Syntax.io_annot list) list;
  sn_req: bool; (* [true] if node requests parameters and these have not been supplied *)
  sn_ins: (string * Types.typ * Syntax.io_annot list) list;
  sn_outs: (string * Types.typ * Syntax.io_annot list) list;
  }

and node_kind = ActorN | GraphN
            
(* Boxes *)

and sv_box = {
  b_id: bid;
  b_tag: box_tag;
  b_model: string;                  (* Name of the instanciated node *)
  b_ins: (string * wid * Types.typ * Syntax.io_annot list) array;      
  b_outs: (string * wid list * Types.typ * Syntax.io_annot list) array;
  b_val: b_val;                     (* For graph [inParam] boxes ([SVUnit] otherwise) *)
  }

and box_tag = 
    ActorB
  | SourceB
  | SinkB
  | GraphB
  | RecB
  | InParamB
  | LocalParamB
  | BcastB

and b_val = { 
    bv_lit: Syntax.expr;     (* Original expression *)
    bv_sub: Syntax.expr;     (* Original expression after substitution of dependencies (ex: "k+1" -> "i1+1") *)
    bv_val: sem_val          (* Statically computed value - SVUnit if N/A *)
  }
                                             
and bid = int
and wid = int
and sel = int

and box_env = (bid * sv_box) list  (** B *)
            
and sv_wire = sv_loc * sv_loc (* src, dst *)
  
and wire_env = (wid * sv_wire) list (** W *)

(* Printing *)

let string_of_node_kind = function | ActorN -> "actor" | GraphN -> "graph"

let string_of_node_io (id,ty,anns) = id

let string_of_svloc (l,s) =  "(" ^ string_of_int l ^ "," ^ string_of_int s ^ ")"

let string_of_node n =
  "Node("
  ^ string_of_node_kind n.sn_kind ^ ","
  ^ "[" ^ Misc.string_of_list string_of_node_io "," n.sn_ins ^ "]," 
  ^ "[" ^ Misc.string_of_list string_of_node_io "," n.sn_outs ^ "])"

let rec  string_of_semval v = match v with
  | SVInt v -> string_of_int v
  | SVBool v -> string_of_bool v
  | SVUnit -> "()"
  | SVNil -> "[]"
  | SVCons (v1,v2) -> string_of_semval v1 ^ "::" ^ string_of_semval v2
  | SVLoc (l,s,ty) -> "Loc(" ^ string_of_int l ^ "," ^ string_of_int s 
  | SVPrim p -> "Prim(...)"
  | SVNode _ -> "Node(...)"
  | SVClos _ -> "Clos(...)"
  | SVTuple vs -> "(" ^ Misc.string_of_list string_of_semval "," vs ^ ")"
  | SVList vs -> "[" ^ Misc.string_of_list string_of_semval "," vs ^ "]"
  | SVWire wid -> "Wire " ^ string_of_int wid

let string_of_io_type ~typed ty = if typed then ":" ^ Pr_type.string_of_type ty else ""
                                
let string_of_bin ~typed (id,wid,ty,anns) = id ^ (string_of_io_type ~typed ty) ^ "(<-W" ^ string_of_int wid ^ ")"
let string_of_bout ~typed (id,wids,ty,anns) = 
    id  ^ (string_of_io_type ~typed ty) ^ "(->["
  ^ (Misc.string_of_list (function wid -> "W" ^ string_of_int wid) "," wids) ^ "])"

let string_of_box ~typed b =
  Printf.sprintf "%s ins=[%s] outs=[%s]"
        b.b_model
        (Misc.string_of_array (string_of_bin ~typed) ","  b.b_ins)
        (Misc.string_of_array (string_of_bout ~typed) ","  b.b_outs)

let string_of_wire ~typed ((s,ss,ty),(d,ds,_)) =
  Printf.sprintf "(B%d,%d) -> (B%d,%d) %s" s ss d ds (string_of_io_type ~typed ty)

    
