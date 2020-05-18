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

(* Typing *)

open Syntax
open Types
open Error

type tenv = {  (* TE *)
  te_cons: (string * int) list; (* constructor name, arity *)
  te_vars: (string * typ) list; (* type variables *)
  }

type venv = (string * typ_scheme) list (* VE *)

let no_loc = Location.no_location
           
(* let lookup_type tenv loc id =
 *     try List.assoc id tenv
 *     with Not_found -> unbound_type id loc *)

let lookup_value venv loc id = 
    try List.assoc id venv
    with Not_found -> unbound_value id loc

(* Typing programs *)

type typed_program = {
  tp_values: (string * typ_scheme) list;
  tp_nodes: (string * typed_node) list;
  }

and typed_node = {
    tn_sig: typ_scheme;
    tn_defns: (string * typ_scheme) list;
  }

(* Unification *)

let try_unify site ?(relax=false) ty1 ty2 loc =
  try
    Types.unify ~relax ty1 ty2
  with 
    TypeConflict(t1, t2) -> wrong_type_err site t1 t2 loc
  | TypeCircularity(t1, t2) -> circular_type_err site t1 t2 loc

(* Typing type expressions *)

let rec type_of_full_type_expression tenv te =
  (* Full version. Updates [tenv] with introduced type variables *)
  let ty, tenv' =
    match te.te_desc with
    | Typeconstr (c,args) ->
       check_type_arity c te.te_loc tenv args;
       let tenv', tys = Misc.fold_left_map type_of_full_type_expression tenv args in
       TyConstr (c, tys),
       tenv'
    | Typevar v ->
       begin
         try
           List.assoc v tenv.te_vars,
           tenv
         with Not_found ->
           let t = Types.new_type_var () in
           t,
           { tenv with te_vars = tenv.te_vars @ [v,t] }
       end in
  te.te_typ <- Types.real_type ty;
  tenv', ty

and type_of_type_expression tenv te =
  (* Restricted version. Lookup [tenv] only *)
  let ty =
    match te.te_desc with
    | Typeconstr (c,args) ->
       check_type_arity c te.te_loc tenv args;
       TyConstr (c, List.map (type_of_type_expression tenv) args)
    | Typevar v ->
       illegal_type_var te.te_loc in
  te.te_typ <- Types.real_type ty;
  ty

and check_type_arity c loc tenv args = 
  let arity =
    try List.assoc c tenv.te_cons
    with Not_found -> unbound_type c loc in
  let nargs = List.length args in
  if nargs <> arity then
    wrong_type_arity c arity nargs loc

let rec type_pattern p =
  (* Returns the type of pattern [p] and the corresponding bindings *)
  (* Ex: [ type_pattern _ "v" = ('a, ["v",'a]) ]
         [ type_pattern _ "(x,y)" = ('a*'b, ["x",'a; "y",'b]) ] *)
  let ty, bindings = match p.p_desc with
  | Pat_var id ->
      let ty = new_type_var() in
      ty, [id, trivial_scheme ty]
  | Pat_ignore ->
      let ty = new_type_var() in
      ty, []
  | Pat_tuple ps ->
     let r = List.map type_pattern ps in
     type_product (List.map fst r),
     List.concat (List.map snd r)
  | Pat_unit ->
     type_unit, []
  | Pat_nil ->
      type_list (new_type_var ()), []
  | Pat_cons(p1, p2) ->
      let ty1, bs1 = type_pattern p1 in
      let ty2, bs2 = type_pattern p2 in 
      try_unify "pattern" (type_list ty1) ty2 p.p_loc;
      (ty2, bs1 @ bs2)
  | Pat_list [] ->
      let ty = type_list (new_type_var()) in
      ty, []
  | Pat_list (p::ps) ->
      let (ty1, bs1) = type_pattern p in
      let bs' =
        List.fold_left 
          (fun bs p ->
            let ty', bs' = type_pattern p in
            try_unify "pattern" ty1 ty' p.p_loc;
            bs @ bs')
          []
          ps in
      (type_list ty1, bs') in
  p.p_typ <- Types.real_type ty;
  ty, bindings

(* Typing expressions *)
  
(* TE, VE |- NExp => tau *)
  
let rec type_expression ?(relax=false) tenv venv expr =
  let ty = match expr.e_desc with
  | EVar id ->
     type_instance (lookup_value venv expr.e_loc id)
  | ETuple es ->
     type_product (List.map (type_expression tenv venv) es)
  | EApp(fn, arg) ->
      let ty_fn = type_expression tenv venv fn in
      let ty_arg = type_expression tenv venv arg in
      let ty_result = new_type_var () in
      try_unify "expression" ~relax:false ty_fn (type_arrow ty_arg ty_result) expr.e_loc;
      ty_result
  | EFun (pat,exp) ->
      let ty_argument = new_type_var ()
      and ty_result = new_type_var () in
      let ty_pat, bindings = type_pattern pat in
      try_unify "pattern" ty_pat ty_argument pat.p_loc;
      let ty_expr = type_expression tenv (bindings @ venv) exp in
      try_unify "expression" ty_expr ty_result expr.e_loc;
      type_arrow ty_argument ty_result
  | ELet (isrec, defns, body) ->
     let venv' = type_definitions expr.e_loc isrec tenv venv defns in
     type_expression tenv (venv' @ venv) body
  | EUnit ->
     type_unit
  | EInt _ ->  type_int
  | EBool _ ->  type_bool
  | EBinop (op, e1, e2) ->
     let ty_op = type_instance (lookup_value venv expr.e_loc op) in
     let ty_e1 = type_expression tenv venv e1 in
     let ty_e2 = type_expression tenv venv e2 in
     let ty_result = new_type_var () in
     try_unify ~relax "expression" ty_op (type_arrow (type_pair ty_e1 ty_e2) ty_result) expr.e_loc ;
     ty_result
  | EIf (e1,e2,e3) ->
      let ty_e1 = type_expression tenv venv e1 in
      let ty_e2 = type_expression tenv venv e2 in
      let ty_e3 = type_expression tenv venv e3 in
      try_unify "expression" ty_e1 type_bool expr.e_loc ;
      try_unify "expression" ty_e2 ty_e3 expr.e_loc;
      ty_e2 
  | ECons (e1, e2) ->
      let ty_e1 = type_expression tenv venv e1 in
      let ty_e2 = type_expression tenv venv e2 in
      try_unify "expression" (type_list ty_e1) ty_e2 expr.e_loc;
      ty_e2
  | EList [] ->
     type_list (new_type_var ())
  | EList (e1::es) ->
      let ty = type_expression tenv venv e1 in
      List.iter 
        (function e' -> try_unify "expression" ty (type_expression tenv venv e') expr.e_loc)
        es;
      type_list ty
  | EListElem (l, i) ->
      let ty_l = type_expression tenv venv l in
      let ty_i = type_expression tenv venv i in
      let ty_e = new_type_var () in
      try_unify "expression" ty_l (type_list ty_e) expr.e_loc;
      try_unify "expression" ty_i type_int expr.e_loc;
      ty_e
  | ENil -> 
      type_list (new_type_var ())
  | EMatch (expr,cases) ->
      let ty_argument = type_expression tenv venv expr
      and ty_result = new_type_var () in
      let type_case {b_desc=(pat,exp); b_loc=loc} =
        let (ty_pat, bindings) = type_pattern pat in
        try_unify "pattern" ty_pat ty_argument loc;
        let ty_expr = type_expression tenv (bindings@venv) exp in
        try_unify "expression" ty_expr ty_result loc in
      List.iter type_case cases;
      ty_result
  | EQuote e ->
     type_param (type_expression ~relax:true tenv venv e)
  in
  expr.e_typ <- Types.real_type ty;
  ty

and type_definitions loc isrec tenv venv defns =
  let ty_pats, venvs =
    List.map (fun {b_desc=(pat,exp)} -> type_pattern pat) defns |> List.split in
  let venv' =
    if isrec
    then List.concat venvs @ venv 
    else venv in
  let ty_exps =
    List.map (fun {b_desc=(pat,exp)} -> type_expression tenv venv' exp)  defns in
  List.iter2
    (fun ty1 ty2 -> try_unify (if isrec then "recursive definition" else "definition") ty1 ty2 loc)
    ty_pats ty_exps;
  let venv'' =
    List.map2
      (fun {b_desc=(pat,exp)} ty_exp -> extract_type_bindings venv pat (real_type ty_exp))
      defns
      ty_exps |> List.concat in
  venv''

(* [VE] |-p Pattern, tau => VE' *)
(* VE is here added because it is needed by the [generalize] function *)
                                  
and extract_type_bindings venv pat ty =
  match pat.p_desc, Types.real_type ty with
  | Pat_var id, ty ->
      [id, generalize venv ty]
  | Pat_tuple ps, TyProduct ts ->
      List.flatten (List.map2 (extract_type_bindings venv) ps ts)
  | Pat_ignore, _ ->
      []
  | Pat_unit, TyConstr("unit",[]) ->
      []
  | Pat_cons(p1, p2), (TyConstr("list", [ty1]) as ty2) ->
      extract_type_bindings venv p1 ty1 @ extract_type_bindings venv p2 ty2
  | Pat_list ps, TyConstr ("list", [t]) ->
     List.flatten
       (List.map
          (function p -> extract_type_bindings venv p t)
          ps)
  | Pat_nil, TyConstr("list", _) ->
      []
  | _, _ ->
     Misc.fatal_error "extract_type_bindings"

(* TE, VE |- ValDecl => VE' *)
          
and type_val_decl tenv venv venv' {vd_desc=(isrec,defns); vd_loc=loc} =
  let venv'' = type_definitions loc isrec tenv (venv'@venv) defns in
  venv' @ venv''

(* TE |- NodeIO => \tau, VE' *)

let rec type_const_expression tenv expr = match expr.e_desc with
  | EInt _ ->  type_int
  | EBool _ ->  type_bool
  | EQuote e -> type_param (type_const_expression tenv e)
  | _ -> Misc.fatal_error "Typing.type_const_expr"

let type_node_io (tenv,venv) ({io_desc=(id,te,e,_); io_loc=loc} as io) = 
  let tenv', ty = type_of_full_type_expression tenv te in
  begin match e with  (* TO FIX: this should be carried out only for param inputs *)
  | Some e' ->
     let ty' = type_const_expression tenv e' in
     try_unify ~relax:true "constant expression" ty ty' loc
  | None ->
      ()
   end;
  (* let ty' = type_wire t in *)
  io.io_typ <- Types.real_type ty;
  (tenv', venv@[id,ty]),
  ty
  
(* TE |- NodeIOs => \tau, VE' *)

let type_node_outps tenv outps =
  (* [... out (o1:t1, ..., on:tn] gives [t1 * ... * tn] *)
  let (tenv',venv'), ts = Misc.fold_left_map type_node_io (tenv,[]) outps in
  let ty = 
  match ts with
  | [] -> type_unit (* should not happen in this version *)
  | [t] -> t
  | _ -> type_product ts in
  ty, tenv', venv'

let type_node_ios tenv tr inps =
  (* [... in (i1:t1, ..., im:tm] gives [t1 -> ... -> tm -> tr] *)
  let (tenv',venv'), ts = Misc.fold_left_map type_node_io (tenv,[]) inps in
  let ty = 
  match ts with
  | [] -> type_arrow type_unit tr (* should not happen in this version *)
  | _ ->  List.fold_right (fun t t' -> type_arrow t t') ts tr in
  ty, tenv', venv'

(* TE, VE |- ValDecls => VE' *)

let type_val_decls tenv venv decls =
  List.fold_left (type_val_decl tenv venv) [] decls

(* TE |- WireDecl => VE *)
  
let type_wire_decl tenv { wr_desc = (id,te) } = id, type_of_type_expression tenv te

(* TE |- WireDecls => VE *)

let type_wire_decls tenv decls =
  List.map (type_wire_decl tenv ) decls

(* TE,VE |- BoxDecl => VE *)

let rec type_box_decl tenv venv { bx_desc = (id,b); bx_loc = loc } =
  (* Typing a box decl suchs [box n: f (i1:t1,...) (o1:t'1,...)] gives type
     [(t1 wire*...) -> (t'1 wire*...)]
     We also check that the resulting type unifies with the type of [f] obtained from the environment *)
  let lookup io = match io.e_desc with
    | EVar id -> type_instance (lookup_value venv loc id)
    | _ -> type_expression ~relax:true tenv venv io in
  let ty_ins = match b.bx_ins with
    | [] -> [type_unit]
    | _ -> List.map lookup b.bx_ins in
  let ty_outs = List.map lookup b.bx_outs in
  (* let ty = type_arrow (type_product ty_ins) (type_product ty_outs) in *)
  let ty = List.fold_right type_arrow ty_ins (type_product ty_outs) in
  let ty_f = type_instance (lookup_value venv loc b.bx_node) in
  try_unify ~relax:true "box declaration" ty_f ty loc;
  let type_io io ty = match io.e_desc with EVar id -> id, ty | _ -> Misc.fatal_error "Typing.eval_node_decl" in
  List.map2 type_io b.bx_outs ty_outs

(* TE,VE |- BoxDecls => VE' *)

let type_box_decls tenv venv decls =
  List.map (type_box_decl tenv venv) decls |> List.concat
  
(* TE, VE |- NodeDecl => VE' *)
     
let rec type_node_decl tenv (venv,nodes) {nd_desc=(id,n); nd_loc=loc} =
  let t_o, tenv', ve_o = type_node_outps tenv n.n_intf.n_outs in
  let tr, tenv'', ve_i = type_node_ios tenv' t_o n.n_intf.n_ins in
  let t = Types.generalize [] tr in
  let ts =
    begin match n.n_impl with
    | NM_Actor _ -> []
    | NM_Fun vdecls -> 
       let ve' = type_val_decls tenv (List.map (fun (id,t) -> id, trivial_scheme t) ve_i @ venv) vdecls in
       check_output_types loc (List.map (fun (id,t) -> id, type_instance t) ve') ve_o;
       ve'
    | NM_Struct s ->
       let ve_w = type_wire_decls tenv s.gs_wires in
       let venv' = List.map (fun (id,t) -> id, trivial_scheme t) (ve_i @ ve_w @ ve_o) in
       let ve' = type_box_decls tenv (venv' @ venv) s.gs_boxes in
       check_output_types loc ve' ve_o;
       []
    end in
  (id,t)::venv,
  (id,{tn_sig=t; tn_defns=ts})::nodes

(* VE' \subset VE *)
  
and check_output_types loc ve' ve =
  (* Check that for each symbol [id] occuring both in [ve'] and [ve], the associated types unify *)
  List.iter
    (fun (id, t') ->
      match List.assoc_opt id ve with
      | None -> ()
      | Some t -> try_unify "output" t' t loc)
    ve'
        
(* TE |- NodeDecls => VE' *)

let type_node_decls tenv venv ndecls =
  List.fold_left (type_node_decl tenv) (venv,[]) ndecls

(* TE |- TypeDecl => TE' *)
     
let rec type_type_decl tenv {td_desc=(id,td)} =
  match td with
    | TD_Abstract -> id, 0

(* TE |- TypeDecls => VE' *)

let type_type_decls tenv tdecls =
  let tycons = List.map (type_type_decl tenv) tdecls in
  { tenv with te_cons = tenv.te_cons @ tycons }

(* |- Program => VE' *)
  
let rec type_program (tycons,venv) p = 
  let tenv = { te_cons = tycons; te_vars = [] } in
  let tenv' = type_type_decls tenv p.types in 
  let venv' = type_val_decls tenv' venv p.values in 
  let venv'', typed_nodes = type_node_decls tenv' (venv'@venv) p.nodes in 
  { tp_values=venv'; tp_nodes = typed_nodes }

(* Printing *)

let rec dump_typed_program tp =
  Printf.printf "Typed program ---------------\n";
  Printf.printf "- Global values -------------\n";
  List.iter dump_typed_value tp.tp_values;
  Printf.printf "- Nodes ---------------------\n";
  List.iter dump_typed_node tp.tp_nodes;
  Printf.printf "----------------------------------\n"

and dump_typed_value (name, ts) =
  Pr_type.reset_type_var_names ();
  Printf.printf "val %s : %s\n" name (Pr_type.string_of_type_scheme ts);
  flush stdout

and dump_typed_node (name, n) =
  Pr_type.reset_type_var_names ();
  Printf.printf "node %s : %s\n" name (Pr_type.string_of_type_scheme n.tn_sig);
  List.iter (fun (id,ty) -> Printf.printf "  val %s: %s\n" id (Pr_type.string_of_type_scheme ty)) n.tn_defns;
  flush stdout

let dump_typing_environment title (tenv,venv) = 
  Printf.printf "%s ---------------\n" title;
  List.iter dump_typed_value venv;
  Printf.printf "----------------------------------\n"
