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

(* Abstract syntax *)

open Location

(* Type expressions *)

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: location;
    mutable te_typ: Types.typ }

and type_expression_desc =
  | Typeconstr of string

type program = {
    types: type_decl list;
    values: val_decl list;
    nodes: node_decl list;
  }

and type_decl =
  { td_desc: type_decl_desc;
    td_loc: location }

and type_decl_desc = string * type_desc

and type_desc =               
  | TD_Abstract

and node_decl =
  { nd_desc: node_decl_desc;
    nd_loc: location }

and node_decl_desc = string * node_desc
              
and node_desc = 
  { n_intf: node_intf;
    n_impl: node_impl }

and node_intf = {
    n_id: string;
    n_isgraph: bool;
    n_params: param_decl list;
    n_ins: io_decl list;
    n_outs: io_decl list
  }

and param_decl =
  { pm_desc: param_desc;
    pm_loc: location;
    mutable pm_typ: Types.typ }

and param_desc = string * type_expression * expr option * io_annot list (* name, type, value for graph decls, annotations *)
               
and io_decl =
  { io_desc: io_desc;
    io_loc: location;
    mutable io_typ: Types.typ }

and io_desc = string * type_expression * io_annot list

and io_annot = string * annot_val (* name, value *)

and annot_val =
  AN_Expr of expr
| AN_String of string
             

and node_impl = 
  | NM_Actor of actor_desc
  | NM_Fun of graph_fun_desc
  | NM_Struct of graph_struct_desc

and actor_desc = actor_impl list

and actor_impl = string * (string * string) list  (* Target, list of named attributes *)              

and graph_fun_desc = val_decl list

and val_decl =
  { vd_desc: val_desc;
    vd_loc: location }

and val_desc = bool * binding list (* isrec, defns *)

and binding =
  { b_desc: binding_desc;
    b_loc: location }

and binding_desc = pattern * expr 

and pattern =
  { p_desc: pattern_desc;
    p_loc: location;
    mutable p_typ: Types.typ }

and pattern_desc =
  | Pat_var of string 
  | Pat_tuple of pattern list
  | Pat_ignore
  | Pat_unit
  | Pat_nil
  | Pat_cons of pattern * pattern
  | Pat_list of pattern list

and expr = 
  { e_desc: expr_desc;
    e_loc: location;
    mutable e_typ: Types.typ }

and expr_desc =
   | EVar of string
   | EApp of expr * expr
   | ETuple of expr list
   | EFun of pattern * expr (* single match here ! *)
   | ELet of bool * binding list * expr 
   | EUnit
   | EInt of int
   | EBool of bool
   | EBinop of string * expr * expr
   | EIf of expr * expr * expr
   | ENil
   | ECons of expr * expr
   | EList of expr list
   | EListElem of expr * expr
   | EMatch of expr * binding list

and graph_struct_desc =
  { gs_wires: wire_decl list;
    gs_boxes: box_decl list }

and wire_decl =
  { wr_desc: wire_desc;
    wr_loc: location }

and wire_desc = string * type_expression

and box_decl =
  { bx_desc: box_decl_desc;
    bx_loc: location }

and box_decl_desc = string * box_desc

and box_desc = 
  { bx_node: string; (* Name of the instanciated node *)
    bx_params: expr list;
    bx_ins: string list;
    bx_outs: string list }

(* Transformations *)
  
let subst_expr vs e = 
  let rec subst e = match e.e_desc with
   | EVar v when List.mem_assoc v vs -> { e with e_desc = EVar (List.assoc v vs) }
   | EBinop (op,e1,e2) -> { e with e_desc = EBinop (op, subst e1, subst e2) }
   | _ -> e in
  subst e

(* Aux *)
  
let basic_expr_equal expr1 expr2 =  (* Structural comparison *)
  let rec cmp e1 e2 = match e1.e_desc, e2.e_desc with
    | EVar v1, EVar v2 -> v1=v2
    | EInt i1, EInt i2 -> i1=i2
    | EBool b1, EBool b2 -> b1=b2
    | EBinop (op1,e11,e12), EBinop (op2,e21,e22) -> op1=op2 && cmp e11 e21 && cmp e12 e22
    | _, _ -> false in
  cmp expr1 expr2

(* Program manipulation *)

let empty_program = { types=[]; values=[]; nodes=[] }

let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
    types= p1.types @ p2.types;
    values= p1.values @ p2.values;
    nodes= p1.nodes @ p2.nodes;
  }

(* Printing *)

let string_of_type_expr = function
  | { te_desc=Typeconstr c } -> c

let rec string_of_expr_desc = function
   | EVar v -> v
   | EApp (e1,e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
   | ETuple es -> "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
   | EFun (p,e) -> "<fun>"
   | ELet (isrec, bs,e) ->
      "let " ^ (if isrec then "rec " else "") ^ string_of_bindings bs ^ "in " ^ string_of_expr e
   | EUnit -> "()"
   | EInt n -> string_of_int n
   | EBool b -> string_of_bool b
   | EBinop (op,e1,e2) -> string_of_expr e1 ^ op ^ string_of_expr e2 (* TODO: add parens when necessary *)
   | EIf (e1,e2,e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
   | ENil -> "[]"
   | ECons (e1,e2) -> string_of_expr e1 ^ "::" ^ string_of_expr e2
   | EList es -> "[" ^ Misc.string_of_list string_of_expr "," es ^ "]"
   | EListElem (e1,e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
   | EMatch (e1,bs) -> "match " ^ string_of_expr e1 ^ " with " ^ Misc.string_of_list string_of_binding " | " bs

and string_of_expr e = string_of_expr_desc e.e_desc

and string_of_bindings bs = Misc.string_of_list string_of_binding " and " bs
                          
and string_of_binding_desc (p,e) = string_of_pattern p ^ " = " ^ string_of_expr e

and string_of_binding b = string_of_binding_desc b.b_desc

and string_of_pattern p = string_of_pattern_desc p.p_desc

and string_of_pattern_desc = function
    Pat_var v -> v
  | Pat_tuple ps -> "(" ^ Misc.string_of_list string_of_pattern "," ps ^ ")"
  | Pat_ignore -> "_"
  | Pat_unit -> "()"
  | Pat_nil -> "[]"
  | Pat_cons (p1,p2) -> string_of_pattern p1 ^ "::" ^ string_of_pattern p2
  | Pat_list ps -> "[" ^ Misc.string_of_list string_of_pattern "," ps ^ "]"

let string_of_val_desc (isrec,bs)  =
  Printf.sprintf "  val %s%s" (if isrec then "rec " else "") (string_of_bindings bs)

let string_of_val_decl d = string_of_val_desc d.vd_desc

let string_of_fun_graph_defn d =
    Misc.string_of_list string_of_val_decl "\n" d

let rec string_of_struct_graph_defn s =
    Misc.string_of_list string_of_wire_decl "\n" s.gs_wires
  ^ "\n"
  ^ Misc.string_of_list string_of_box_decl "\n" s.gs_boxes

and string_of_wire_decl { wr_desc = (id,t) } = "  wire " ^ id ^ " : " ^ string_of_type_expr t

and string_of_box_decl { bx_desc = (id,b) } = 
  "  box " ^ id ^ " : " ^ b.bx_node
  ^ "(" ^ Misc.string_of_list string_of_expr "," b.bx_params ^ ")" 
  ^ "(" ^ Misc.string_of_list Fun.id "," b.bx_ins ^ ")"
  ^ "(" ^ Misc.string_of_list Fun.id "," b.bx_outs ^ ")"

let string_of_node_impl i =
  match i with
  | NM_Actor _ -> "actor"
  | NM_Fun g -> "fun\n" ^ string_of_fun_graph_defn g
  | NM_Struct g -> "struct\n" ^ string_of_struct_graph_defn g


let string_of_expr e = string_of_expr_desc e.e_desc

let string_of_node_io_desc (id,t,ann) = id ^ ":" ^ string_of_type_expr t

let string_of_node_io io = string_of_node_io_desc io.io_desc

let string_of_node_param_desc (id,t,e,anns) =
  let string_of_opt_exp = function None -> "" | Some e -> "=" ^ string_of_expr e in
  id ^ ":" ^ string_of_type_expr t ^ string_of_opt_exp e

let string_of_node_param param = string_of_node_param_desc param.pm_desc

let string_of_type_decl (id,d) = match d with
  | TD_Abstract -> id 
                         
let string_of_node_intf i =
  "node " ^ i.n_id
    ^ " param (" ^ Misc.string_of_list string_of_node_param ", " i.n_params ^ ")"
    ^ " in (" ^ Misc.string_of_list string_of_node_io ", " i.n_ins ^ ")"
    ^ " out (" ^ Misc.string_of_list string_of_node_io ", " i.n_outs ^ ")"

(* let string_of_node_intf intf = string_of_node_intf_desc intf.ni_desc *)

let dump_node {nd_desc=(id,n)} = Printf.printf "%s = %s\n" (string_of_node_intf n.n_intf) (string_of_node_impl n.n_impl)
let dump_type d = Printf.printf "type %s\n" (string_of_type_decl d.td_desc)
let dump_value d = Printf.printf "%s\n" (string_of_val_decl d)

let rec dump_program p =
  Printf.printf "Program ---------------\n";
  Printf.printf "- Types ---------------\n";
  List.iter dump_type p.types;
  Printf.printf "- Global values -------\n";
  List.iter dump_value p.values;
  let graphs, nodes = List.partition (fun {nd_desc=(_,n)} -> n.n_intf.n_isgraph) p.nodes in
  Printf.printf "- Nodes ---------------\n";
  List.iter dump_node nodes;
  Printf.printf "- Graphs ---------------\n";
  List.iter dump_node graphs
