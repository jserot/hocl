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

open Syntax
open Types
open Error

let auto_type_decl = ref false

type typing_env = {
  te_types: (string * int) list;           (* type constructors (name, arity) *)
  te_values: (string * typ_scheme) list
  }

let augment_types env tenv = { tenv with te_types = env @ tenv.te_types }
let augment_values env tenv = { tenv with te_values = env @ tenv.te_values }

let empty_tenv = { te_types = []; te_values = [] }

let lookup_type loc tenv id = 
    try List.assoc id tenv.te_types
    with Not_found -> unbound_type_err id loc

let lookup_value loc tenv id = 
    try List.assoc id tenv.te_values
    with Not_found -> unbound_value_err id loc

(* For debug only *)
    
let rec dump_global_typing_environment title tenv = 
  Printf.printf "%s ---------------\n" title;
  List.iter dump_type tenv.te_types;
  List.iter (dump_value "val") tenv.te_values;
  Printf.printf "----------------------------------\n"

and dump_type (name, arity) =
  Printf.printf "type %s (arity=%d)\n" name arity

and dump_value tag (name, ty_sch) =
  Pr_type.reset_type_var_names ();
  Printf.printf "%s %s : %s\n" tag name (Pr_type.string_of_type_scheme ty_sch)

(* Typing programs *)

type typed_program = {
  (* tp_types: (string * type_desc) list;     (\* Type constructors *\) *)
  tp_values: (string * typ_scheme) list;
  tp_nodes: (string * typed_intf) list;
  tp_graphs: (string * (typed_intf * typed_desc)) list;
  }

and typed_intf = {   (* Actors and graphs *) 
   t_params: (string * typ) list;                                    
   t_ins: (string * (typ * Syntax.io_annot list)) list;             
   t_outs: (string * (typ * Syntax.io_annot list)) list;           
   t_real_ins: (string * (typ * Syntax.io_annot list)) list; (* Excluding I/Os with [unit] type *)            
   t_real_outs: (string * (typ * Syntax.io_annot list)) list;           
   t_sig: typ_scheme;         (* external type signature: [t_ins -> t_outs] or [t_params -> t_ins -> t_outs] *)
  (* TO FIX ? Should we allow nodes (and/or resp. graphs) to have a _polymorphic_ interface ? 
     Example:
       node id in (i: 'a) out (o: 'a);
       graph main (i: int) out (o: int) val o = id i end;
     This does not make sense as soon as a concrete implementation is required for actor [id], does it ? *)
  }

and typed_desc = (* Only for graphs *)
  TD_Struct of typed_wire list * typed_node list
| TD_Fun of typed_defn list

and typed_wire = string * typ
and typed_node = string * typ

and typed_defn = string * typ_scheme
  
(* Unification *)

let try_unify site ty1 ty2 loc =
  try
    Types.unify ty1 ty2
  with 
    TypeConflict(t1, t2) -> wrong_type_err site t1 t2 loc
  | TypeCircularity(t1, t2) -> circular_type_err site t1 t2 loc

(* Typing type expressions *)

let rec type_of_type_expression tenv te =
  let rec type_of te =
    match te.te_desc with
    | Typeconstr (c, []) ->
       if List.mem_assoc c tenv.te_types || !auto_type_decl then type_constr c []
       else undeclared_type_ctor c te.te_loc
    | Typeconstr (c, ts) ->
       if List.mem_assoc c tenv.te_types then
         if List.length ts = List.assoc c tenv.te_types then
           type_constr c (List.map (type_of_type_expression tenv) ts)
         else type_ctor_mismatch c te.te_loc
       else undeclared_type_ctor c te.te_loc
    | Typetuple ts -> type_product (List.map type_of ts) in
  let ty = type_of te in
  te.te_typ <- ty;
  ty

(* Typing core expressions *)
  
let rec type_core_expression genv expr =
  let lookup id = type_instance (lookup_value expr.ce_loc genv id) in
  let type_unparam t = match real_type t with
  | TyConstr ("param",[t']) -> t'
  | _ -> t in
  let ty = match expr.ce_desc with
  | EVar id -> type_unparam (lookup id)
  | EInt _ ->  type_int
  | EBool _ ->  type_bool
  | EBinop (op, e1, e2) ->
     let ty_op = lookup op in
     let ty_e1 = type_core_expression genv e1 in
     let ty_e2 = type_core_expression genv e2 in
     let ty_result = new_type_var () in
     try_unify "expression" ty_op (type_arrow (type_pair ty_e1 ty_e2) ty_result) expr.ce_loc;
     ty_result in
  expr.ce_typ <- ty;
  ty


(* Typing type decls *)

(* TE |- TyDecl => TE' *)
  
let type_type_decl tenv { td_desc=t } = match t with
  | Opaque_type_decl name -> name, 0
  (* TODO: add at least type synonymes, like [type tau = int] *)

(* Typing patterns *)
                             
let rec type_pattern tenv p =
  (* Returns the type pattern [p] and the corresponding bindings *)
  (* Ex: [ type_pattern _ "v" = ('a, ["v",'a]) ]
         [ type_pattern _ "(x,y)" = ('a*'b, ["x",'a; "y",'b]) ] *)
  let ty, bs = match p.np_desc with
  | NPat_var id ->
      let ty = new_type_var() in
      ty, [id, trivial_scheme ty]
  | NPat_unit ->
     type_unit, []
  | NPat_ignore ->
      let ty = new_type_var() in
      ty, []
  | NPat_tuple ps ->
     let r = List.map (type_pattern tenv) ps in
     type_product (List.map fst r),
     List.concat (List.map snd r)
  | NPat_nil ->
      type_bundle (new_type_var ()) None, []
  | NPat_cons(p1, p2) ->
      let ty1, bs1 = type_pattern tenv p1 in
      let ty2, bs2 = type_pattern tenv p2 in (* TO FIX : should [tenv] here be augmented with [bs1] ? *)
      try_unify "pattern" (type_bundle ty1 None) ty2 p.np_loc;
      (ty2, bs1 @ bs2)
  | NPat_bundle [] ->
      let ty = type_bundle (new_type_var()) None in
      ty, []
  | NPat_bundle (p::ps) ->
      let (ty1, bs1) = type_pattern tenv p in
      let bs' =
        List.fold_left 
          (fun bs p ->
            let ty', bs' = type_pattern tenv p in
            try_unify "pattern" ty1 ty' p.np_loc;
            bs @ bs')
          []
          ps in
      (type_bundle ty1 None, bs') in
  p.np_typ <- ty;
  ty, bs

(* Typing net expressions *)
  
(* TE, VE |- NExp => tau *)
  
let rec type_net_expression genv expr =
  let ty = match expr.ne_desc with
  | NVar id ->
     type_instance (lookup_value expr.ne_loc genv id)
  | NPApp(fn, pvs) ->
      let ty_fn = type_net_expression genv fn in
      let ty_params = type_product (List.map (type_param_expression genv) pvs) in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_params ty_result) expr.ne_loc;
      ty_result
  | NTuple es ->
     type_product (List.map (type_net_expression genv) es)
  | NApp(fn, arg) ->
      let ty_fn = type_net_expression genv fn in
      let ty_arg = type_net_expression genv arg in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_arg ty_result) expr.ne_loc;
      ty_result
  | NFun (pat,exp) ->
      let ty_argument = new_type_var ()
      and ty_result = new_type_var () in
      let ty_pat, bindings = type_pattern genv pat in
      try_unify "pattern" ty_pat ty_argument pat.np_loc;
      let ty_expr = type_net_expression (genv |> augment_values bindings) exp in
      try_unify "expression" ty_expr ty_result exp.ne_loc;
      type_arrow ty_argument ty_result
  | NMatch (exp,cases) ->
      let ty_argument = type_net_expression genv exp
      and ty_result = new_type_var () in
      let type_case { nb_desc = pat,exp } =
        let (ty_pat, bindings) = type_pattern genv pat in
        try_unify "pattern" ty_pat ty_argument pat.np_loc;
        let ty_expr = type_net_expression (genv |> augment_values bindings) exp in
        try_unify "expression" ty_expr ty_result exp.ne_loc in
      List.iter type_case cases;
      ty_result
  | NLet (rec_flag, [defn], body) ->
      let bindings = type_definition rec_flag genv defn in
      type_net_expression (genv |> augment_values bindings) body
  | NLet (_, _, _) ->
     failwith "Typing.type_net_expression: multi-let"
  | NCons (e1, e2) ->
      let ty_e1 = type_net_expression genv e1 in
      let ty_e2 = type_net_expression genv e2 in
      try_unify "expression" (type_bundle ty_e1 None) ty_e2 expr.ne_loc;
      ty_e2
  | NBundle [] ->
     type_bundle (new_type_var ()) None
  | NBundle (e1::es) ->
      let ty = type_net_expression genv e1 in
      List.iter 
        (function e' -> try_unify "expression" ty (type_net_expression genv e') expr.ne_loc)
        es;
      type_bundle ty None
  | NBundleElem (l, i) ->
      let ty_l = type_net_expression genv l in
      let ty_i = type_net_expression genv i in
      let ty_e = new_type_var () in
      try_unify "expression" ty_l (type_bundle ty_e None) expr.ne_loc;
      try_unify "expression" ty_i type_int expr.ne_loc;
      ty_e
  | NNil -> 
      type_bundle (new_type_var ()) None
  | NBool b -> type_bool
  | NInt n ->  type_int
  | NUnit -> type_unit
  | NIf (e1,e2,e3) ->
      let ty_e1 = type_net_expression genv e1 in
      let ty_e2 = type_net_expression genv e2 in
      let ty_e3 = type_net_expression genv e3 in
      try_unify "expression" ty_e1 type_bool e1.ne_loc;
      try_unify "expression" ty_e2 ty_e3 expr.ne_loc;
      ty_e2 in
  expr.ne_typ <- ty;
  ty

and type_param_expression tenv ce = Types.type_param @@ type_core_expression tenv ce
                            
(* [TE] |-p Pattern, tau => VE *)
(* TE is here added because it is needed by the [generalize] function *)
                                  
and extract_type_bindings tenv loc pat ty = match (pat.np_desc, Types.real_type ty) with
  | NPat_var id, ty ->
      [id, generalize tenv.te_values ty]
  | NPat_tuple ps, TyProduct ts ->
      List.flatten (List.map2 (extract_type_bindings tenv loc) ps ts)
  | NPat_tuple ps, (TyConstr("bundle", [ty1]) as ty2) -> (* Implicit bundle -> tuple conversion *)
      let ty' = TyProduct (Misc.list_make (List.length ps) ty1) in
      extract_type_bindings tenv loc pat ty'
  | NPat_cons(p1, p2), (TyConstr("bundle", [ty1]) as ty2) ->
      extract_type_bindings tenv loc p1 ty1 @ extract_type_bindings tenv loc p2 ty2
  | NPat_bundle ps, TyConstr ("bundle", [t]) ->
     List.flatten
       (List.map
          (function p -> extract_type_bindings tenv loc p t)
          ps)
  | NPat_nil, TyConstr("bundle", _) ->
      []
  | NPat_ignore, _ ->
      []
  | NPat_unit, TyConstr("unit",[]) ->
      []
  | _, _ -> Misc.fatal_error "extract_type_bindings"

(* TE, VE |- ValDecl => VE' *)
          
and type_definition is_rec genv { nb_desc=(pat,exp); nb_loc=loc } =
  let ty_pat, bindings = type_pattern empty_tenv pat in (* TO FIX: should we [genv] here instead of [empty] ? *)
  let genv' =
    if is_rec
    then genv |> augment_values bindings
    else genv in
  let ty_exp = type_net_expression genv' exp in
  try_unify "definition" ty_pat ty_exp loc;
  extract_type_bindings genv loc pat ty_exp

let type_application loc ty_fn ty_arg = 
  let ty_fn', tvs = full_type_instance ty_fn in
  let ty_result = new_type_var () in
  try_unify "application" ty_fn' (type_arrow ty_arg ty_result) loc;
  ty_fn', ty_result, tvs

let type_application2 loc ty_fn ty_param ty_arg = 
  let ty_fn', tvs = full_type_instance ty_fn in
  let ty_result = new_type_var () in
  try_unify "application" ty_fn' (type_arrow2 ty_param ty_arg ty_result) loc;
  ty_fn', ty_result, tvs

(* and type_definitions tenv is_rec bindings =
 *   let ty_pats, aug_envs =
 *     List.split
 *       (List.map
 *         (function { nb_desc=(npat,nexp); nb_loc=loc } -> type_net_pattern npat)
 *         defns) in
 *   let tenv' =
 *     if is_rec
 *     then { tenv with te_values = List.concat aug_envs @ tenv.te_values }
 *     else tenv in
 *   let ty_exps = List.map
 *       (function { nb_desc = (npat,nexp); nb_loc=loc } ->
 *         type_network_expr tenv' nexp, loc)
 *       defns in
 *   List.iter2
 *     (fun ty1 (ty2,loc) -> try_unify (if is_rec then "recursive definition" else "definition") ty1 ty2 (ELoc loc))
 *     ty_pats ty_exps;
 *   let tenv'' = List.flatten
 *       (List.map2
 *          (fun {nb_desc=(npat,_); nb_loc=loc} (texp,loc) -> 
 *              extract_type_bindings loc tenv' npat (real_type texp))
 *          defns
 *          ty_exps) in
 *   tenv'' *)

(* Typing net defns *)
  
let type_net_defn genv { nd_desc=d } = match d with
  | is_rec, [binding] -> type_definition is_rec genv binding
  | _, _ -> Misc.not_implemented "Typing.typing multi val definitions (val p1=e1 and p2=e2 and ...)"

(* Typing (sub-)graph decls *)

(* TE,VE |- GraphStructDefn => VE' *)
          
let rec type_struct_graph_desc loc genv intf g =
  let ty_wires = List.map (type_wire_decl genv) g.gs_wires in
  let genv' = genv
              |> augment_values (List.map (fun (id,ty) -> id, trivial_scheme ty) intf.t_params)
              |> augment_values (List.map (fun (id,(ty,_)) -> id, trivial_scheme ty) intf.t_ins)
              |> augment_values (List.map (fun (id,(ty,_)) -> id, trivial_scheme ty) intf.t_outs)
              |> augment_values (List.map (fun (id,ty) -> id, trivial_scheme ty) ty_wires) in
  let ty_nodes = List.map (type_node genv') g.gs_nodes in
  TD_Struct (ty_wires, ty_nodes)

(* TE |- Wire => VE *)
  
and type_wire_decl tenv { gw_desc = (id,te) } = id, type_wire @@ type_of_type_expression tenv te

(* TE,VE |- Node => VE *)

and type_node genv { gn_desc = (id,g); gn_loc = loc } =
  (* Typing a node decl such [node n: f (p1:t1,...) (i1:t'1,...) (o1:t''1,...)] gives type
     [(t1 param*...) -> (t'1 wire*...) -> (t''1 wire*...)]
     We also check that the resulting type unifies with the type of [f] obtained from the environment *)
  let lookup id = type_instance (lookup_value loc genv id) in
  let ty_ins = List.map lookup g.gn_ins in
  let ty_outs = List.map lookup g.gn_outs in
  let ty_params = List.map (type_param_expression genv) g.gn_params in
  let ty = type_sig ty_params ty_ins ty_outs in
  let ty_f = lookup g.gn_name in
  try_unify "node declaration" ty_f ty loc;
  id, ty 

and type_sig ty_params ty_ins ty_outs = 
 match ty_params with
    | [] -> type_arrow (type_product ty_ins) (type_product ty_outs)
    | _ -> type_arrow2 (type_product ty_params) (type_product ty_ins) (type_product ty_outs)

(* TE,VE |- GraphFunDefn => VE' *)

let rec type_fun_graph_desc genv intf defns =
  let genv' = genv
              |> augment_values (List.map (fun (id,ty) -> id, trivial_scheme ty) intf.t_params)
              |> augment_values (List.map (fun (id,(ty,_)) -> id, trivial_scheme ty) intf.t_ins) in
  let ty_defns, locs = 
    List.fold_left
      (fun (env,locs) d ->
        let env' = type_net_defn (genv' |> augment_values env) d in
        env @ env',
        locs @ List.map (fun (id,_) -> id, d.nd_loc) env')
          (* For each defined symbol - there may be several for a single defn - we keep track of
             the location of the enclosing defn *)
      ([],[])
      defns in
  (* TODO : check that the type infered for each symbol occuring in [intf.t_outs] matches that infered for
     the corresponding definition *)
  List.iter 
    (fun (id,ty) ->
      if List.mem_assoc id intf.t_outs then 
        let ty' = fst (List.assoc id intf.t_outs) in
        let loc = try List.assoc id locs with Not_found -> Misc.fatal_error "type_fun_graph_defn" in
        try_unify "graph output definition" (type_instance ty) ty' loc)
    ty_defns;
  TD_Fun ty_defns

(* Typing node decls *)

let is_real_io (id,(ty,_)) = not (is_unit_type ty)

let rec type_node_intf tenv { ni_desc=n } =
  let ty_params =
    List.map
      (fun { pm_desc = (id,te,_) } ->
        let ty = type_of_type_expression tenv te
        in id, type_param ty)
      n.n_params in
  let tenv' = tenv |> augment_values @@ List.map (fun (id,ty) -> id, trivial_scheme ty) ty_params in
  let ty_ins = type_io tenv' n.n_ins in
  let ty_outs = type_io tenv' n.n_outs in
  let type_of io = fst (snd io) in
  { t_params = ty_params;
    t_ins = ty_ins;
    t_outs = ty_outs;
    t_real_ins = List.filter is_real_io ty_ins;
    t_real_outs = List.filter is_real_io ty_outs;
    t_sig =
      trivial_scheme (type_sig (List.map snd ty_params) (List.map type_of ty_ins) (List.map type_of ty_outs)) }

(* TE |- Io => \tau, VE' *)
  
and type_io tenv = function
      [] ->
       ["_", (type_unit, [])]
    | ios ->
       List.map
         (fun { io_desc = (id,te,anns) } ->
           (* let _ = List.iter (type_io_annotation tenv) anns in *)
           let ty = type_of_type_expression tenv te in
           id, (type_wire ty, anns))
         ios

(* and type_io_annotation tenv ann = match ann with
 *   | IA_Rate e -> try_unify "rate expression" (type_core_expression tenv e) type_int e.ce_loc
 *   | IA_Other _ -> () *)

let type_node_impl genv ty_intf { nm_desc=m; nm_loc=loc } = (* TO FIX !!!!!!!!!! *)
  match m with
  | NM_None 
  | NM_Actor _ -> () (* Nothing to check here, all is the interface declaration *)
  | NM_Struct desc ->
     let ty_impl = type_struct_graph_desc loc genv ty_intf desc in
     (* TODO : match ty_intf and ty_impl ! *)
     ()
  | NM_Fun desc ->
     let ty_impl = type_fun_graph_desc genv ty_intf desc in
     (* TODO : match ty_intf and ty_impl ! *)
     ()

(* TE, VE |- NodeDecl => VE' *)
     
let rec type_node_decl (typed_nodes,tenv) n =
  let id = n.n_intf.ni_desc.n_id in
  let typed_intf = type_node_intf tenv n.n_intf in
  let _ = type_node_impl tenv typed_intf n.n_impl in
  (id, typed_intf)::typed_nodes,
  augment_values [id,typed_intf.t_sig] tenv

(* Typing (top) graph decls *)

let type_graph_decl (acc,genv) { g_desc=g } =
  (* Typing a graph declaration consists in
     - typing its interface, and adding the corresponding signature to the current NTE 
     - typing its definition, collecting the result and checking that it matches the interface *)
  let type_param_value v = match v with
    | None -> new_type_var ()
    | Some v -> type_core_expression empty_tenv v in
  let type_param { pm_desc = id,te,v; pm_loc = loc } =
    let ty = type_of_type_expression genv te in
    let ty' = type_param_value v in
    try_unify "param value" ty ty' loc;
    id, ty in
  let type_of io = fst (snd io) in
  let ty_params = List.map type_param g.g_params in
  let ty_ins = type_io genv g.g_ins in
  let ty_outs = type_io genv g.g_outs in
  let ty_intf =
    { t_params = ty_params;
      t_ins = ty_ins;
      t_outs = ty_outs;
      t_real_ins = List.filter is_real_io ty_ins;
      t_real_outs = List.filter is_real_io ty_outs;
      t_sig =
        trivial_scheme (type_sig (List.map snd ty_params) (List.map type_of ty_ins) (List.map type_of ty_outs)) } in
  let ty_desc = 
    begin match g.g_defn.gd_desc with
    | GD_Struct desc -> type_struct_graph_desc g.g_defn.gd_loc genv ty_intf desc
    | GD_Fun desc -> type_fun_graph_desc genv ty_intf desc
    end in
  let acc' = (g.g_id, (ty_intf, ty_desc)) :: acc in
  let genv' = genv |> augment_values [g.g_id,ty_intf.t_sig] in
  List.rev acc', genv'
  
(* TE, VE |- ValDecl => VE' *)
  
let type_value_decl (acc,tenv) d =
  let typed_values = type_net_defn tenv d in
  typed_values @ acc,
  augment_values typed_values tenv
    
(* TE,VE |- Program => VE_v, VE_n, VE_G *)
  
let rec type_program tenv p = 
  let typed_types = p.types |> List.map (type_type_decl tenv) in
  let tenv_t = tenv |> augment_types typed_types in
  let typed_values, tenv_v = List.fold_left type_value_decl ([],tenv_t) p.values in
  let typed_nodes, tenv_n = List.fold_left type_node_decl ([],tenv_v) p.nodes in
  let typed_graphs, _ = List.fold_left type_graph_decl ([],tenv_n) p.graphs in
  { tp_values = typed_values;
    tp_nodes = typed_nodes;
    tp_graphs = typed_graphs }

(* Printing *)

let rec dump_typed_program tp =
  Printf.printf "Typed program ---------------\n";
  List.iter dump_typed_value tp.tp_values;
  List.iter dump_typed_node tp.tp_nodes;
  List.iter dump_typed_graph tp.tp_graphs;
  Printf.printf "----------------------------------\n"

and dump_typed_value (name, ts) =
  Pr_type.reset_type_var_names ();
  Printf.printf "val %s : %s\n" name (Pr_type.string_of_type_scheme ts);
  flush stdout

and dump_typed_node (name, ti) =
  Pr_type.reset_type_var_names ();
  Printf.printf "node %s : %s\n" name (Pr_type.string_of_type_scheme ti.t_sig);
  flush stdout

and dump_typed_graph (name, (ti,td)) =
  Pr_type.reset_type_var_names ();
  Printf.printf "graph %s : %s\n" name (Pr_type.string_of_type_scheme ti.t_sig);
  dump_typed_graph_desc td;
  flush stdout

and dump_typed_graph_desc td = match td with
  | TD_Struct (ty_wires, ty_nodes) ->
     List.iter (fun (id,ty) -> Printf.printf "  wire %s: %s\n" id (Pr_type.string_of_type ty)) ty_wires;
     List.iter (fun (id,ty) -> Printf.printf "  node %s: %s\n" id (Pr_type.string_of_type ty)) ty_nodes
  | TD_Fun ty_defns ->
     List.iter (fun (id,ty) -> Printf.printf "  val %s: %s\n" id (Pr_type.string_of_type_scheme ty)) ty_defns
  
