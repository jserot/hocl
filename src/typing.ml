open Syntax
open Types
open Error

type tenv = (string * typ) list        (* TE *)
type venv = (string * typ_scheme) list (* VE *)

let no_loc = Location.no_location
           
let lookup_type tenv loc id =
    try List.assoc id tenv
    with Not_found -> unbound_type id loc

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

let try_unify site ty1 ty2 loc =
  try
    Types.unify ty1 ty2
  with 
    TypeConflict(t1, t2) -> wrong_type_err site t1 t2 loc
  | TypeCircularity(t1, t2) -> circular_type_err site t1 t2 loc

(* Typing type expressions *)

let rec type_of_type_expression tenv te =
  let ty =
    match te.te_desc with
    | Typeconstr c -> lookup_type tenv te.te_loc c in
  te.te_typ <- Types.real_type ty;
  ty
  

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
  
let rec type_expression tenv venv expr =
  let ty = match expr.e_desc with
  | EVar id ->
     type_instance (lookup_value venv expr.e_loc id)
  | ETuple es ->
     type_product (List.map (type_expression tenv venv) es)
  | EApp(fn, arg) ->
      let ty_fn = type_expression tenv venv fn in
      let ty_arg = type_expression tenv venv arg in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_arg ty_result) expr.e_loc;
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
     try_unify "expression" ty_op (type_arrow (type_pair ty_e1 ty_e2) ty_result) expr.e_loc ;
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
      ty_result in
  expr.e_typ <- Types.real_type ty;
  ty

(* and type_definitions isrec tenv venv defns =
 *      let venvs = List.map (type_definition tenv venv) defns in
 *      List.concat venvs *)

(* and type_definition tenv venv (pat,exp) =
 *   let ty_exp = type_expression tenv venv exp in
 *   extract_type_bindings venv pat ty_exp *)
                            
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

(* TE |- NodeParam => \tau, VE' *)

let type_const_expression tenv expr = match expr.e_desc with
  | EInt _ ->  type_int
  | EBool _ ->  type_bool
  | _ -> Misc.fatal_error "Typing.type_const_expr"

let type_node_param tenv ({pm_desc=(id,te,e,_); pm_loc=loc} as p) = 
  let ty = type_of_type_expression tenv te in
  begin match e with
  | Some e' ->
     let ty' = type_const_expression tenv e' in
     try_unify "constant expression" ty ty' loc
  | None ->
     ()
  end;
  p.pm_typ <- Types.real_type ty;
  ty,
  [id,ty]

(* TE |- NodeIO => \tau, VE' *)

let type_node_io tenv ({io_desc=(id,te,_)} as io) = 
  let ty = type_wire (type_of_type_expression tenv te) in
  io.io_typ <- Types.real_type ty;
  ty,
  [id,ty]
  
(* TE |- NodeIOs => \tau, VE' *)
(* TE |- NodeParams => \tau, VE' *)

let type_node_intf_components type_component tenv ios =
  let ts, venvs = List.map (type_component tenv) ios |> List.split in
  match ts, venvs with
  | [], [] -> type_unit, [] (* should not happen in this version *)
  | [t], [ve] -> t, ve
  | _, _ -> type_product ts, List.concat venvs

let type_node_ios tenv ios = type_node_intf_components type_node_io tenv ios
let type_node_params tenv params = type_node_intf_components type_node_param tenv params

(* TE, VE |- ValDecls => VE' *)

let type_val_decls tenv venv decls =
  List.fold_left (type_val_decl tenv venv) [] decls

(* TE |- WireDecl => VE *)
  
let type_wire_decl tenv { wr_desc = (id,te) } = id, type_wire (type_of_type_expression tenv te)

(* TE |- WireDecls => VE *)

let type_wire_decls tenv decls =
  List.map (type_wire_decl tenv ) decls

(* TE,VE |- BoxDecl => VE *)

let rec type_box_decl tenv venv { bx_desc = (id,b); bx_loc = loc } =
  (* Typing a box decl suchs [box n: f (p1:t1,...) (i1:t'1,...) (o1:t''1,...)] gives type
     [(t1*...) -> (t'1 wire*...) -> (t''1 wire*...)]
     We also check that the resulting type unifies with the type of [f] obtained from the environment *)
  let lookup id = type_instance (lookup_value venv loc id) in
  let ty_ins = List.map lookup b.bx_ins in
  let ty_outs = List.map lookup b.bx_outs in
  let ty_params = List.map (type_expression tenv venv) b.bx_params in
  let ty =
    match ty_params with
    | [] -> type_arrow (type_product ty_ins) (type_product ty_outs)
    | _ -> type_arrow2 (type_product ty_params) (type_product ty_ins) (type_product ty_outs) in
  let ty_f = lookup b.bx_node in
  try_unify "box declaration" ty_f ty loc;
  List.combine b.bx_outs ty_outs

and type_sig ty_params ty_ins ty_outs = 
 match ty_params with
    | [] -> type_arrow (type_product ty_ins) (type_product ty_outs)
    | _ -> type_arrow2 (type_product ty_params) (type_product ty_ins) (type_product ty_outs)

(* TE,VE |- BoxDecls => VE' *)

let type_box_decls tenv venv decls =
  List.map (type_box_decl tenv venv) decls |> List.concat
  
(* TE, VE |- NodeDecl => VE' *)
     
let rec type_node_decl tenv (venv,nodes) {nd_desc=(id,n); nd_loc=loc} =
  let t_p, ve_p = type_node_params tenv n.n_intf.n_params in
  let t_i, ve_i = type_node_ios tenv n.n_intf.n_ins in
  let t_o, ve_o = type_node_ios tenv n.n_intf.n_outs in
  let t = match n.n_intf.n_params with
    | [] -> trivial_scheme (type_arrow t_i t_o)
    | _ -> trivial_scheme (type_arrow2 t_p t_i t_o) in
  let ts =
    begin match n.n_impl with
    | NM_Actor _ -> []
    | NM_Fun vdecls -> 
       let ve' = type_val_decls tenv (List.map (fun (id,t) -> id, trivial_scheme t) (ve_i @ ve_p) @ venv) vdecls in
       check_output_types loc (List.map (fun (id,t) -> id, type_instance t) ve') ve_o;
       ve'
    | NM_Struct s ->
       let ve_w = type_wire_decls tenv s.gs_wires in
       let venv' = List.map (fun (id,t) -> id, trivial_scheme t) (ve_i @ ve_p @ ve_w @ ve_o) in
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
  let ty = match td with
    | TD_Abstract -> type_constr id [] in
  id, ty

(* TE |- TypeDecls => VE' *)

let type_type_decls tenv tdecls =
  List.map (type_type_decl tenv) tdecls 

(* |- Program => VE' *)
  
let rec type_program (tenv,venv) p = 
  let tenv' = type_type_decls tenv p.types in 
  let venv' = type_val_decls (tenv'@tenv) venv p.values in 
  let venv'', typed_nodes = type_node_decls (tenv'@tenv) (venv'@venv) p.nodes in 
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
