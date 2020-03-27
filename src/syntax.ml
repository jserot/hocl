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
  | Typetuple of type_expression list
  | Typeconstr of string * type_expression list

(* Programs *)
                
type program =
  { types: type_decl list;    (* User-defined types *)
    values: value_decl list;  (* Values *)
    nodes: node_decl list;    (* Node models *)
    graphs: graph_decl list } (* Top-level graphs *)

and type_decl =
  { td_desc: tdecl_desc;
    td_loc: location }
and tdecl_desc =
  | Opaque_type_decl of string                                   (* name *)

and value_decl = net_defn
              
and node_decl = 
  { n_intf: node_intf;
    n_impl: node_impl }

and node_intf = 
  { ni_desc: ni_desc;
    ni_loc: location }

and ni_desc = {
    n_id: string;
    n_kind: node_kind;
    n_params: param_decl list;
    n_ins: io_decl list;
    n_outs: io_decl list
  }

and node_kind = NRegular | NBcast
                          
and param_decl =
  { pm_desc: param_desc;
    pm_loc: location }

and param_desc = string * type_expression * core_expr option (* values only for top level graphs *)

and io_decl =
  { io_desc: io_desc;
    io_loc: location }

and io_desc = string * type_expression * io_annot list

and io_annot = string * string (* name, value *)
                          
(* and io_annot =
 *   IA_Rate of rate_expr
 * | IA_Other of string *)
             
and node_impl = {
    nm_desc: nm_desc;
    nm_loc: location; }

and nm_desc = 
  | NM_None
  | NM_Actor of actor_desc
  | NM_Struct of graph_struct_desc
  | NM_Fun of graph_fun_desc

and actor_desc = actor_impl list

and actor_impl = string * (string * string) list  (* Target, list of named attributes *)              
            
and graph_decl = 
  { g_desc: graph_desc;
    g_loc: location }

and graph_desc = {
    g_id: string;
    g_params: param_decl list;
    g_ins: io_decl list;
    g_outs: io_decl list;
    g_defn: graph_defn
  }

and graph_defn =
  { gd_desc: graph_defn_desc;
    gd_loc: location }

and graph_defn_desc = 
  | GD_Struct of graph_struct_desc
  | GD_Fun of graph_fun_desc

and graph_struct_desc =
  { gs_wires: gwire_decl list;
    gs_nodes: gnode_decl list }

and gwire_decl =
  { gw_desc: graph_wire_desc;
    gw_loc: location }

and graph_wire_desc = string * type_expression

and gnode_decl =
  { gn_desc: graph_node_desc;
    gn_loc: location }

and graph_node_desc = string * graph_node

and graph_node = 
  { gn_name: string; (* Name of the instanciated model *)
    gn_params: core_expr list;
    gn_ins: string list;
    gn_outs: string list }

and graph_fun_desc = net_defn list

and net_defn =
  { nd_desc: net_defn_desc;
    nd_loc: location }

and net_defn_desc = bool * net_binding list

and net_binding =
  { nb_desc: net_binding_desc;
    nb_loc: location }

and net_binding_desc = net_pattern * net_expr 

and net_pattern =
  { np_desc: net_pattern_desc;
    np_loc: location;
    mutable np_typ: Types.typ }

and net_pattern_desc =
  | NPat_var of string 
  | NPat_tuple of net_pattern list
  | NPat_nil
  | NPat_cons of net_pattern * net_pattern
  | NPat_bundle of net_pattern list
  | NPat_unit
  | NPat_ignore

and net_expr =
  { ne_desc: net_expr_desc;
    ne_loc: location;
    mutable ne_typ: Types.typ }

and net_expr_desc =
   | NVar of string
   | NApp of net_expr * net_expr
   | NPApp of net_expr * core_expr list (* Parameter application. Ex: [foo<1,2>] *)
   | NTuple of net_expr list
   | NLet of bool * net_binding list * net_expr (* rec/non rec*)
   | NFun of net_pattern * net_expr (* single match here ! *)
   | NNil
   | NCons of net_expr * net_expr
   | NBundle of net_expr list
   | NBundleElem of net_expr * net_expr
   | NIf of net_expr * net_expr * net_expr
   | NMatch of net_expr * net_binding list
   | NBool of bool
   | NInt of int
   | NUnit

and core_expr = 
  { ce_desc: core_expr_desc;
    ce_loc: location;
    mutable ce_typ: Types.typ }

and core_expr_desc =
   | EVar of string
   | EInt of int
   | EBool of bool
   | EBinop of string * core_expr * core_expr
(* TO BE EXTENDED ? Should we include user-defined or primitive fn application ? *)

and param_expr = core_expr
and rate_expr = core_expr

let core_expr_equal expr1 expr2 =  (* Structural comparison *)
  let rec cmp e1 e2 = match e1.ce_desc, e2.ce_desc with
    | EVar v1, EVar v2 -> v1=v2
    | EInt i1, EInt i2 -> i1=i2
    | EBool b1, EBool b2 -> b1=b2
    | EBinop (op1,e11,e12), EBinop (op2,e21,e22) -> op1=op2 && cmp e11 e21 && cmp e12 e22
    | _, _ -> false in
  cmp expr1 expr2

(* Aux fns *)

let is_fun_definition = function
  { nb_desc={np_desc=NPat_var _}, {ne_desc=NFun (_,_)} } -> true
| _ -> false

let no_annot = ""

(* Program manipulation *)

let empty_program = { types=[]; values=[]; nodes=[]; graphs=[] }

let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
    types= p1.types @ p2.types;
    values= p1.values @ p2.values;
    nodes= p1.nodes @ p2.nodes;
    graphs= p1.graphs @ p2.graphs;
  }

(* Printing *)

let is_unop op = List.mem op [ "not" ]      (* TO ADJUST WITH PARSER/LEXER *)

let is_binop op = List.mem op [             (* TO ADJUST WITH PARSER/LEXER *)
  "+"; "-"; "*"; "/"; "%";
  "<"; ">"; "="; "!="; "<>"
  ]

let is_rbinop op = List.mem op [             (* TO ADJUST WITH PARSER/LEXER *)
  "+"; "-"; "*"; "/"; "%"
  ]
   
let rec string_of_ty_expr te = string_of_ty_exp te.te_desc

and string_of_ty_exp = function
  | Typeconstr (c, []) -> c
  | Typeconstr (c, [t]) -> string_of_ty_expr t ^ " " ^ c
  | Typeconstr (c, ts) -> "(" ^ Misc.string_of_list string_of_ty_expr "," ts ^ ") " ^ c
  | Typetuple ts -> "(" ^ Misc.string_of_list string_of_ty_expr "*" ts ^ ")"

let rec string_of_net_expr ne = string_of_net_exp ne.ne_desc

and string_of_net_exp = function
   | NVar v -> v
   | NPApp (e,ps) ->
      string_of_net_expr e  ^ "<" ^ Misc.string_of_list string_of_core_expr "," ps ^ ">"
   | NApp (e1,e2) -> string_of_net_expr e1 ^ " " ^ string_of_net_expr e2
   | NTuple es -> "(" ^ Misc.string_of_list string_of_net_expr "," es ^ ")"
   | NLet (isrec,nbs,e) ->
         "let " ^ if isrec then "rec " else " " ^  Misc.string_of_list string_of_net_binding "and" nbs
       ^ " in " ^ string_of_net_expr e
   | NFun (p,e) -> "<fun>"
   | NNil -> "[]"
   | NCons (e1,e2) -> string_of_net_expr e1 ^ "::" ^ string_of_net_expr e2
   | NBundle es -> "[" ^ Misc.string_of_list string_of_net_expr "," es ^ "]"
   | NBundleElem (e1,e2) -> string_of_net_expr e1 ^ "[" ^ string_of_net_expr e2 ^ "]"
   | NIf (e1,e2,e3) -> "if " ^ string_of_net_expr e1 ^ " then " ^ string_of_net_expr e2 ^ " else " ^ string_of_net_expr e3
   | NMatch (e1,bs) -> "match " ^ string_of_net_expr e1 ^ " with " ^ Misc.string_of_list string_of_net_binding " | " bs
   | NBool b -> string_of_bool b
   | NInt n -> string_of_int n
   | NUnit -> "()"

and string_of_net_binding nb = string_of_net_bind nb.nb_desc

and string_of_net_bind (np,ne) = string_of_net_pattern np ^ " = " ^ string_of_net_expr ne

and string_of_net_pattern np = string_of_net_pat np.np_desc

and string_of_net_pat = function
    NPat_var v -> v
  | NPat_tuple ps -> "(" ^ Misc.string_of_list string_of_net_pattern "," ps ^ ")"
  | NPat_nil -> "[]"
  | NPat_cons (p1,p2) -> string_of_net_pattern p1 ^ "::" ^ string_of_net_pattern p2
  | NPat_bundle ps -> "[" ^ Misc.string_of_list string_of_net_pattern "," ps ^ "]"
  | NPat_unit -> "()"
  | NPat_ignore -> "_"

and string_of_core_expr e = string_of_core_exp e.ce_desc

and string_of_core_exp = function
   | EVar v -> v
   | EInt n -> string_of_int n
   | EBool n -> string_of_bool n
   | EBinop (op,e1,e2) -> string_of_core_expr' e1 ^ op ^ string_of_core_expr' e2

and string_of_core_expr' e =
  if is_simple_core_expr e
  then string_of_core_expr e
  else "(" ^ string_of_core_expr e ^ ")"

and is_simple_core_expr e = match e.ce_desc with
  | EVar _ -> true
  | EInt _ -> true
  | EBool _ -> true
  | EBinop _ -> false
              
let is_constant_core_expr e =
  match e.ce_desc with
  | EInt _ -> true
  | EBool _ -> true
  | _ -> false
       
let subst_core_expr vs e = 
  let rec subst e = match e.ce_desc with
   | EVar v when List.mem_assoc v vs -> { e with ce_desc = EVar (List.assoc v vs) }
   | EBinop (op,e1,e2) -> { e with ce_desc = EBinop (op, subst e1, subst e2) }
   | _ -> e in
  subst e

let string_of_rate_expr = string_of_core_expr

let string_of_io_annot (name,value) = name ^ "=" ^ value
(* let string_of_io_annot = function
 *   | IA_Rate e -> "rate=" ^ string_of_rate_expr e
 *   | IA_Other s -> "other=" ^ s *)
let string_of_io_annots = function
    [] -> ""
  | anns -> "{" ^ Misc.string_of_list string_of_io_annot "," anns ^ "}"

let string_of_type_decl d = match d.td_desc with
  | Opaque_type_decl id -> id

let string_of_opt_param_value v = match v with
  | None -> ""
  | Some v' -> "=" ^ string_of_core_expr v'
let string_of_io_decl {io_desc=id,ty,anns} = id ^ ": " ^ string_of_ty_expr ty ^ string_of_io_annots anns
let string_of_param_decl {pm_desc=id,ty,v} = id ^ ": " ^ string_of_ty_expr ty ^ string_of_opt_param_value v
                               
let string_of_node_kind = function NRegular -> "node" | NBcast -> "bcast"
                                                                  
let string_of_node_intf d =
  let i = d.ni_desc in
  string_of_node_kind i.n_kind ^ " " ^ i.n_id
    ^ " params (" ^ Misc.string_of_list string_of_param_decl ", " i.n_params ^ ")"
    ^ " in (" ^ Misc.string_of_list string_of_io_decl ", " i.n_ins ^ ")"
    ^ " out (" ^ Misc.string_of_list string_of_io_decl ", " i.n_outs ^ ")"

let string_of_node_impl d =
  match d.nm_desc with
  | NM_None -> "None"
  | NM_Actor _ -> "Actor(...)"
  | NM_Struct _ -> "Struct(...)"
  | NM_Fun _ -> "Fun(...)"

let rec string_of_graph_decl d =
  let g = d.g_desc in
  "graph " ^ g.g_id
    ^ " params (" ^ Misc.string_of_list string_of_param_decl ", " g.g_params ^ ")"
    ^ " in (" ^ Misc.string_of_list string_of_io_decl ", " g.g_ins ^ ")"
    ^ " out (" ^ Misc.string_of_list string_of_io_decl ", " g.g_outs ^ ")"
    ^ " = " ^ string_of_graph_defn g.g_defn

and string_of_graph_defn d = match d.gd_desc with
  | GD_Struct s -> "struct\n" ^ string_of_graph_struct s ^ "\nend"
  | GD_Fun s -> "fun\n" ^ string_of_graph_fun s ^ "\nend"

and string_of_graph_struct s =
    Misc.string_of_list string_of_wire_decl "\n" s.gs_wires
  ^ "\n"
  ^ Misc.string_of_list string_of_gnode_decl "\n" s.gs_nodes

and string_of_wire_decl { gw_desc = (id,t) } = "  wire " ^ id ^ " : " ^ string_of_ty_expr t

and string_of_gnode_decl { gn_desc = (id, n) } = 
  "  node " ^ id ^ " : " ^ n.gn_name
  ^ "(" ^ Misc.string_of_list string_of_core_expr "," n.gn_params ^ ")" 
  ^ "(" ^ Misc.string_of_list Misc.id "," n.gn_ins ^ ")"
  ^ "(" ^ Misc.string_of_list Misc.id "," n.gn_outs ^ ")"

and string_of_graph_fun ds =
  Misc.string_of_list string_of_net_defn "\n" ds
  
and string_of_net_defn d = match d.nd_desc with
    r, bs -> string_of_rec r ^ Misc.string_of_list string_of_net_binding " and " bs

and string_of_rec = function true -> " rec " | false -> ""

let dump_type d = Printf.printf "type %s\n" (string_of_type_decl d)
let dump_value d = Printf.printf "val %s\n" (string_of_net_defn d)
let dump_node d = Printf.printf "%s = %s\n" (string_of_node_intf d.n_intf) (string_of_node_impl d.n_impl)
let dump_graph d = Printf.printf "%s\n" (string_of_graph_decl d)

let rec dump_program p =
  Printf.printf "Types ---------------\n";
  List.iter dump_type p.types;
  Printf.printf "Global values -------\n";
  List.iter dump_value p.values;
  Printf.printf "Nodes ---------------\n";
  List.iter dump_node p.nodes;
  Printf.printf "Graphs --------------\n";
  List.iter dump_graph p.graphs
