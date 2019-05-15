open Syntax
open Types
open Error

let auto_type_decl = ref false

type typing_env = {
  te_types: (string * int) list;           (* type constructors (name, arity) *)
  te_values: (string * typ_scheme) list
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

(* Typing patterns *)
                             
let rec type_pattern tenv p =
  let ty, tenv' = match p.np_desc with
  | NPat_var id ->
      let ty = new_type_var() in
      ty, { tenv with te_values = (id, trivial_scheme ty) :: tenv.te_values }
  | NPat_unit ->
     type_unit, tenv
  | NPat_ignore ->
      let ty = new_type_var() in
      ty, tenv
  | NPat_tuple ps ->
      let tenv', tys =
        List.fold_left 
          (fun (tenv,ts) p -> let t, tenv' = type_pattern tenv p in (tenv', t::ts))
          (tenv, [])
          ps in
      (type_product (List.rev tys), tenv')
  | NPat_nil ->
      type_list (new_type_var ()) None, tenv
  | NPat_cons(p1, p2) ->
      let (ty1, tenv1) = type_pattern tenv p1 in
      let (ty2, tenv2) = type_pattern tenv1 p2 in
      try_unify "pattern" (type_list ty1 None) ty2 p.np_loc;
      (ty2, tenv2)
  | NPat_list [] ->
      let ty = type_list (new_type_var()) None in
      ty, tenv
  | NPat_list (p::ps) ->
      let (ty1, tenv1) = type_pattern tenv p in
      let tenv' =
        List.fold_left 
          (fun tenv p ->
            let ty', tenv' = type_pattern tenv p in
            try_unify "pattern" ty1 ty' p.np_loc;
            tenv')
          tenv
          ps in
      (type_list ty1 None, tenv') in
  p.np_typ <- ty;
  ty, tenv'

(* Typing expressions *)
  
let rec type_expression tenv expr =
  let ty = match expr.ne_desc with
  | NVar id ->
        begin try type_instance (List.assoc id tenv.te_values)
        with Not_found -> unbound_value_err id expr.ne_loc end
  | NTuple es -> type_product (List.map (type_expression tenv) es)
  | NApp(fn, arg) ->
      let ty_fn = type_expression tenv fn in
      let ty_arg = type_expression tenv arg in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_arg ty_result) expr.ne_loc;
      ty_result
  | NFun (pat,exp) ->
      let ty_argument = new_type_var ()
      and ty_result = new_type_var () in
      let (ty_pat, tenv') = type_pattern tenv pat in
      try_unify "pattern" ty_pat ty_argument pat.np_loc;
      let ty_expr = type_expression tenv' exp in
      try_unify "expression" ty_expr ty_result exp.ne_loc;
      type_arrow ty_argument ty_result
  | NMatch (exp,cases) ->
      let ty_argument = type_expression tenv exp
      and ty_result = new_type_var () in
      let type_case { nb_desc = pat,exp } =
        let (ty_pat, tenv') = type_pattern tenv pat in
        try_unify "pattern" ty_pat ty_argument pat.np_loc;
        let ty_expr = type_expression tenv' exp in
        try_unify "expression" ty_expr ty_result exp.ne_loc in
      List.iter type_case cases;
      ty_result
  | NLet (rec_flag, [binding], body) ->
      let tenv' = { tenv with te_values = type_definition rec_flag tenv binding @ tenv.te_values } in
      type_expression tenv' body
  | NLet (_, _, _) ->
     failwith "Typing.type_expression: multi-let"
  | NCons (e1, e2) ->
      let ty_e1 = type_expression tenv e1 in
      let ty_e2 = type_expression tenv e2 in
      try_unify "expression" (type_list ty_e1 None) ty_e2 expr.ne_loc;
      ty_e2
  | NList [] ->
     type_list (new_type_var ()) None
  | NList (e1::es) ->
      let ty = type_expression tenv e1 in
      List.iter 
        (function e' -> try_unify "expression" ty (type_expression tenv e') expr.ne_loc)
        es;
      type_list ty None
  | NListElem (l, i) ->
      let ty_l = type_expression tenv l in
      let ty_i = type_expression tenv i in
      let ty_e = new_type_var () in
      try_unify "expression" ty_l (type_list ty_e None) expr.ne_loc;
      try_unify "expression" ty_i type_nat expr.ne_loc;
      ty_e
  | NNil -> 
      type_list (new_type_var ()) None
  | NBool b -> type_bool
  | NNat n ->  type_nat
  | NUnit -> type_unit
  | NIf (e1,e2,e3) ->
      let ty_e1 = type_expression tenv e1 in
      let ty_e2 = type_expression tenv e2 in
      let ty_e3 = type_expression tenv e3 in
      try_unify "expression" ty_e1 type_bool e1.ne_loc;
      try_unify "expression" ty_e2 ty_e3 expr.ne_loc;
      ty_e2 in
  expr.ne_typ <- ty;
  ty

and extract_type_bindings tenv loc pat ty = match (pat.np_desc, Types.real_type ty) with
  | NPat_var id, ty ->
      [id, generalize tenv.te_values ty]
  | NPat_tuple ps, TyProduct ts ->
      List.flatten (List.map2 (extract_type_bindings tenv loc) ps ts)
  | NPat_cons(p1, p2), (TyConstr("list", [ty1]) as ty2) ->
      extract_type_bindings tenv loc p1 ty1 @ extract_type_bindings tenv loc p2 ty2
  | NPat_list ps, TyConstr ("list", [t]) ->
     List.flatten
       (List.map
          (function p -> extract_type_bindings tenv loc p t)
          ps)
  | NPat_nil, TyConstr("list", _) ->
      []
  | NPat_ignore, _ ->
      []
  | NPat_unit, TyConstr("unit",[]) ->
      []
  | _, _ -> Misc.fatal_error "extract_type_bindings"

and type_definition is_rec tenv { nb_desc=(pat,exp); nb_loc=loc } =
  let ty_pat, tenv' = type_pattern tenv pat in
  let augmented_env = if is_rec then tenv' else tenv in
  let ty_exp = type_expression augmented_env exp in
  try_unify "definition" ty_pat ty_exp loc;
  extract_type_bindings tenv loc pat ty_exp

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

(* Typing type decls *)

let type_type_decl tenv { td_desc=t } = match t with
  | Opaque_type_decl name -> name, 1
                           
(* Typing actor decls *)

type typed_actor = {
   at_params: (string * typ) list;        (* params *)
   at_ins: (string * typ) list;           (* inputs *)
   at_outs: (string * typ) list;          (* outputs *)
   at_sig: typ_scheme;         (* external type signature: [t_ins -> t_outs] or [t_params -> t_ins -> t_outs] *)
  }

let type_actor_decl tenv { ad_desc=a } =
  let type_io = function
      [] -> ["_", type_unit]
    | ios -> List.map (fun (id,te) -> id, type_of_type_expression tenv te) ios in
  let ty_ins = type_io a.a_ins in
  let ty_outs = type_io a.a_outs in
  let ty_params = List.map (fun (id,te) -> id, type_of_type_expression tenv te) a.a_params in
  let ty = match ty_params with
    | [] -> type_arrow
              (type_product (List.map snd ty_ins))
              (type_product (List.map snd ty_outs))
    | _ -> type_arrow2
             (type_product (List.map snd ty_params))
             (type_product (List.map snd ty_ins))
             (type_product (List.map snd ty_outs)) in
  a.a_id,
  { at_params = ty_params;
    at_ins = ty_ins;
    at_outs = ty_outs;
    at_sig = trivial_scheme ty }

let type_param_decl tenv { pd_desc=(id,te,e); pd_loc=loc } =
  let ty = type_of_type_expression tenv te in
  let ty' = type_expression tenv e in
  try_unify "parameter" ty ty' loc;
  (id, (* type_param *) ty)

(* Typing top-level net defns *)
  
let type_net_defn tenv { nd_desc=d } = match d with
  | is_rec, [binding] -> type_definition is_rec tenv binding
  | _, _ -> failwith "Typing.type_net_defn(multi-net)"
  
(* Typing programs *)

type typed_program = {
  (* tp_types: (string * type_desc) list;     (\* Type constructors *\) *)
  tp_params: (string * typ) list;
  tp_actors: (string * typed_actor) list;
  tp_defns: (string * typ_scheme) list;
  }

let rec type_program tenv p = 
  let typed_types = List.map (type_type_decl tenv) p.types in
  let tenv' = { tenv with te_types = tenv.te_types @ typed_types } in 
  let typed_params, tenv'' =
    List.fold_left
      (fun (tps,tenv) p ->
        let id, ty = type_param_decl tenv p in
        (id,ty)::tps, { tenv with te_values = tenv.te_values @ [id, generalize [] ty] })
      ([], tenv')
      p.params in
  let typed_actors =  List.map (type_actor_decl tenv'') p.actors in
  let tenv''' = { tenv'' with te_values = List.map (function (id,a) -> id,a.at_sig) typed_actors @ tenv.te_values } in
  let typed_defns =
    List.fold_left
      (fun env d ->
        let env' = type_net_defn { tenv'' with te_values = tenv''.te_values @ env } d in
        env @ env')
      tenv'''.te_values
      p.defns in
  { tp_params = typed_params;
    tp_actors = typed_actors;
    tp_defns = typed_defns }

(* Printing *)

let rec dump_typed_program builtins tp =
  Printf.printf "Typed program ---------------\n";
  List.iter dump_typed_actor tp.tp_actors;
  List.iter dump_typed_param tp.tp_params;
  List.iter (dump_typed_value builtins) tp.tp_defns;
  Printf.printf "----------------------------------\n"

and dump_typed_param (name, ty) =
  Printf.printf "parameter %s : %s\n" name (Pr_type.string_of_type ty);
  flush stdout

and dump_typed_actor (name, a) =
  Pr_type.reset_type_var_names ();
  Printf.printf "actor %s : %s\n" name (Pr_type.string_of_type_scheme a.at_sig);
  flush stdout

and dump_typed_value builtins (name, ty_sch) =
  if not (List.mem_assoc name builtins.te_values ) then begin
      Pr_type.reset_type_var_names ();
      Printf.printf "val %s : %s\n" name (Pr_type.string_of_type_scheme ty_sch);
      flush stdout
    end

