open Syntax
open Error
open Misc
open Types
open Typing
open Printf
open Pr_type
open Ssval
open Location

type static_env = (string * ss_val) list

type box_tag = 
    ActorB
  (* | ParamB of io_kind *)
  | DummyB  (* Temporary boxes used for handling recursive defns *)

and ss_box = {
    b_id: int;
    b_tag: box_tag;
    b_name: string;                           (* For regular boxes, name of the instanciated actor *)
    b_typ: typ;                               (* "Functional" type, i.e. either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
    (* b_tysig: typ;                             (\* "Signature" type, i.e. [t_params * t_ins * t_vars * t_outs] *\) *)
    (* b_tvbs: typ var_bind list;                (\* Type var instantiations (when the box derives from a polymorphic actor) *\) *)
    b_ins: (string * (wid * typ)) list;
    b_outs: (string * (wid list * typ)) list;
    (* b_params: (string * (Expr.e_val * typ)) list;      (\* Parameters, with their actual values *\) *)
}

and wid = int
and bid = int

type ss_wire = (sv_loc * sv_loc) * typ 

(* The result of the static analysis *)

type static_program = { 
    (* e_vals: (string * Expr.e_val) list; *)
    nvals: (string * ss_val) list;
    gacts: (string * sa_desc) list;
    boxes: (bid * ss_box) list;
    wires: (wid * ss_wire) list;
    (* gcsts: (string * gc_desc) list;             (\* Global constants *\)
     * gtyps: (string * Typing.tc_desc) list;      (\* Globally defined types *\) *)
  }

and sa_desc = {                                                     (* Actors *)
    sa_desc: Ssval.sv_act;                                          (* Definition *)
    (* mutable ac_insts: (ss_box, bid) ActInsts.t;                     (\* Instances *\) *)
  }

(* Box creation *)

let new_bid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_box name ty ins outs =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=ActorB; b_name=name; b_ins=ins; b_outs=outs; b_typ=ty }

let new_dummy_box name ty =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=DummyB; b_name=name; b_ins=[]; b_outs=["r",([0],ty)]; b_typ=ty }

let boxes_of_wire boxes (((s,ss),(d,ds)),ty) = 
  try
    List.assoc s boxes, List.assoc d boxes
  with Not_found -> 
    fatal_error "Static.boxes_of_wire"  (* should not happen *)

let src_box_of_wire boxes (w:ss_wire) = fst (boxes_of_wire boxes w)
let dst_box_of_wire boxes (w:ss_wire) = snd (boxes_of_wire boxes w)

(*** NETWORK LEVEL ***)

(* Rules NE [,B] |-n NPat, rho => NE', W *)

exception Matching_fail

let is_output nenv id =
  try match List.assoc id nenv with
      SVLoc(_,_,_,true) as loc -> Some loc
    | _ -> None
    with Not_found -> None

let rec net_matching toplevel nenv npat r = match npat.np_desc, r with
  | NPat_var id, r' ->
      begin match toplevel, is_output nenv id, r' with
      | false, _, _ -> [id, r], []
      | true, None, _ -> [id, r], []
      | true, Some (SVLoc(l',_,ty',_)), SVLoc(l,s,ty,_) -> [], [new_wid(),(((l,s),(l',0)),ty')]
      | true, _, _ -> fatal_error "matching: cannot bind output" end
  | NPat_tuple ps, SVTuple rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching toplevel nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_list ps, SVList rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching toplevel nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_list ps, SVCons _ ->
      net_matching toplevel nenv npat (list_of_cons r)
  | NPat_unit, _ -> [], []
  | NPat_ignore, _ -> [], []
  | _, _ -> raise Matching_fail

(* TODO : unify [net_matching] and [matching] *)
          
let rec matching pat v = match pat.np_desc, v with
  | NPat_var id, _ -> [id, v]
  | NPat_tuple ps, SVTuple vs when List.length ps = List.length vs ->
      List.flatten (List.map2 matching ps vs)
  | NPat_nil, SVNil -> []
  | NPat_unit, SVUnit -> []
  (* | Pat_ignore, _ -> [] *)
  | NPat_cons(p1, p2), SVCons(v1, v2) ->
      let env1 = matching p1 v1 in
      let env2 = matching p2 v2 in
      env1 @ env2
  | NPat_cons _, SVList _ ->
      matching pat (cons_of_list v)
  | _, _ -> raise Matching_fail

(* Rule: NE |- NExp => rho,B,W *)

let rec eval_net_expr tp nenv expr =
  match expr.ne_desc with
  | NNat n -> SVNat n, [], []
  | NBool b -> SVBool b, [], []
  | NUnit -> SVUnit, [], []
  | NVar v ->
      if List.mem_assoc v nenv then List.assoc v nenv, [], []
      else unbound_value_err v expr.ne_loc
  | NNil -> SVNil, [], []
  | NTuple es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr tp nenv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVTuple rs, bs, ws
  | NCons (e1,e2) ->
      let v1, bs1, ws1 = eval_net_expr tp nenv e1 in
      let v2, bs2, ws2 = eval_net_expr tp nenv e2 in
      SVCons (v1,v2), bs1@bs2, ws1@ws2
  | NList es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr tp nenv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVList rs, bs, ws
  | NListElem (l,i) ->
      let v1, bs1, ws1 = eval_net_expr tp nenv l in
      let v2, bs2, ws2 = eval_net_expr tp nenv i in
      eval_list_access expr.ne_loc v1 v2, bs1@bs2, ws1@ws2
  | NIf (e1,e2,e3) ->
     let v1, bs1, ws1 = eval_net_expr tp nenv e1 in
     let v', bs', ws' =
       begin match v1 with
       | SVBool true -> eval_net_expr tp nenv e2
       | SVBool false -> eval_net_expr tp nenv e3
       | _ -> illegal_expression expr (* should not happen *)
       end in 
     v', bs1@bs', ws1@ws'
  | NApp (fn, arg) ->
      let val_fn, bs_f, ws_f = eval_net_expr tp nenv fn in
      let val_arg, bs_a, ws_a = eval_net_expr tp nenv arg in
      let r, bs'', ws'' =
        eval_net_application tp (bs_a@bs_f,ws_a@ws_f) nenv expr.ne_loc val_fn val_arg (real_type arg.ne_typ) in
      r, bs'' @ bs_a @ bs_f, ws'' @ ws_a @ ws_f
  | NFun (npat,nexp) ->
      SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv}, [], []
  | NLet (isrec, defns, body) ->
      let nenv',boxes,wires = eval_net_defns expr.ne_loc false isrec tp nenv defns in
      let r',boxes',wires' = eval_net_expr tp (nenv' @ nenv) body in
      r', boxes' @ boxes, wires' @ wires
  | NMatch (e, bs) ->
      let v, bs', ws' = eval_net_expr tp nenv e in
      let rec seq_match = function 
        | [] -> matching_failure expr.ne_loc
        | { nb_desc=(p,e) } :: bs ->
            begin try
              let nenv' = matching p v in
              eval_net_expr tp (nenv' @ nenv) e
            with Matching_fail ->
              seq_match bs end in
      let v', bs'', ws'' = seq_match bs in
      v', bs'@bs'', ws'@ws''

and eval_net_application tp (bs,ws) nenv loc val_fn val_arg ty_arg = 
  match val_fn with
  | SVPrim f -> f val_arg, [], []
  | SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv'} ->   (* RULE NAPP CLO *)
      let nenv'', _ =
        begin try net_matching false [] npat val_arg
        with Matching_fail -> binding_error npat.np_loc
        end in
      eval_net_expr tp (nenv'' @ nenv') nexp
  | SVAct a ->
      (* if List.length a.sa_params > 0                     (\* RULE NAPP ACT 2 *\)
       * && List.for_all (function _,_,None -> true | _,_,Some _ -> false) a.sa_params then
       *   let actual_params =
       *     begin
       *       match val_arg, ty_arg with
       *       | SVVal v, ty ->
       *           [v,ty]
       *       | SVTuple vs, Tproduct ts ->
       *           begin try
       *             List.map2 (fun v t -> match v with SVVal v' -> v',t | _ -> raise (Invalid_argument "")) vs ts
       *           with
       *             Invalid_argument _ -> failwith "Static.eval_net_application" (\* should not happen *\)
       *           end
       *       | _, _ ->
       *           illegal_parameters a.sa_id loc "must be int(s) or bool(s)"
       *     end in
       *   instanciate_actor_desc a actual_params, [], []
       * else                                               (\* Rule NAPP ACT 1 *\) *)
        instanciate_actor tp nenv loc a val_arg
  | _ ->
          illegal_application loc

(* Rule TE,EE,NE |- let/net [rec] npat1=nexp1 ... npatn=nexpn => NE', B, W *)

and eval_net_defns loc toplevel isrec tp nenv bindings =
  if not isrec then                                        (* NON RECURSIVE CASE *)
    let eval_net_binding {nb_desc=npat,nexp} =
      let v, bs', ws' = eval_net_expr tp nenv nexp in
      let nenv', ws'' =
        begin try 
          if toplevel
          then net_matching true nenv npat v
          else net_matching false [] npat v
        with Matching_fail ->  binding_error npat.np_loc end in
      nenv', bs', ws'@ws'' in
    let nenvs', boxes', wires' = Misc.list_split3 (List.map eval_net_binding bindings) in 
    List.concat nenvs',
    List.concat boxes',
    List.concat wires'
  else                                                     (* RECURSIVE CASE *)
    if List.for_all is_fun_definition bindings             (* All recursive _functions_ *)
    then begin
      let rec_cls =
        List.map
          (function
              {nb_desc={np_desc=NPat_var v}, {ne_desc=NFun (p,e)}; nb_loc=loc} ->
                v, {cl_pat=p; cl_exp=e; cl_env=[]}
            | _ ->
                failwith "Static.eval_net_defns")  (* should not happen *)
          bindings in
      let nenv' = List.map (function (v,cl) -> (v, SVClos cl)) rec_cls in
      List.iter (function (v,cl) -> cl.cl_env <- nenv' @ nenv) rec_cls;
      nenv', [], []
    end
    else if not (List.exists is_fun_definition bindings)   (* No recursive function *)
    then begin
      let rec_env, bs =
        List.fold_left
          (fun (env,bs) {nb_desc=npat,nexp} ->
            let env',bs' = create_rec_bindings toplevel nenv npat in
            env @ env', bs @ bs')
          ([],[])
          bindings in
      let vs', bs', ws' =
        List.fold_left
          (fun (vs,bs,ws) {nb_desc=npat,nexp} ->
             let v',bs',ws' = eval_net_expr tp (nenv @ rec_env) nexp in
             vs @ [v'], bs @ bs', ws @ ws')
          ([],[],[])
          bindings in
      let nenv', ws'' =
        List.fold_left2
          (fun (env,ws) {nb_desc=npat,nexp} v ->
            let env',ws' =
              begin try
                if toplevel
                then net_matching true nenv npat v
                else net_matching false [] npat v
              with Matching_fail ->  binding_error npat.np_loc end in
            env @ env', ws @ ws')
          ([],[])
          bindings vs' in
      let substs = List.map (mk_subst loc nenv') rec_env in
      let ws''' = List.fold_left (fun ws subst -> List.map (apply_subst subst) ws) (ws'@ws'') substs in
      nenv', bs', ws'''
    end
    else
      illegal_rec_definition loc

and eval_list_access loc v1 v2 = 
  match v1, v2 with
  | SVList vs, SVNat k ->
     if k < List.length vs
     then List.nth vs k 
     else invalid_list_index k loc
  | SVCons _, SVNat _ ->
     eval_list_access loc (list_of_cons v1) v2
  | _, _ -> fatal_error "Static.eval_list_access" (* should not happen thx to TC *)

(* RULES REC PATTERN 1, 2 :  |-r NPat => NE, B *)

and create_rec_bindings toplevel nenv npat =
  match npat.np_desc with
    | NPat_var id ->
        begin match toplevel, is_output nenv id with
          false, _
        | true, None ->
            let ty = type_copy npat.np_typ in
            let l, b = new_dummy_box id ty in
            [id, SVLoc(l,0,ty,false)], [(l,b)]
        | true, Some l ->
            (* Note 2013-04-12, JS
               Outputs are _not_ added to the recursive environment *)
            [], []
        end
    | NPat_tuple ps ->
        List.fold_left
          (fun (ne,bs) p ->
            let ne', bs' = create_rec_bindings toplevel nenv p in
            ne' @ ne, bs' @ bs)
          ([],[])
          ps
    | _ -> not_implemented "this kind of recursive pattern"
(*     | NPat_bundle ps -> not_implemented "recursive bundle pattern" *)

and mk_subst loc nenv (rid,rv) =
  match rv, List.assoc rid nenv with
    SVLoc(l,s,_,_), SVLoc(l',s',_,_) -> (l,s),(l',s')
  | _, _ -> illegal_rec_definition loc

and apply_subst ((i,s),(i',s')) (wid,((src,dst),ty)) =
  let sub (k,l) =
    if k=i then (i',s') else (k,l) in
  (wid, ((sub src, sub dst), ty))


(* Auxilliaries *)

(* and instanciate_actor_desc a actual_params = 
 *   let renv =
 *     Misc.foldl_index
 *       (fun i r (v,_) -> match v with
 *       | Expr.Val_int (n,_) -> (i+1,n)::r
 *       | _ -> r)
 *       []
 *      actual_params in
 *   SVAct
 *     { sa_id = a.sa_id;
 *       sa_params = List.map2
 *         (fun (id,formal_ty,_) (v,actual_ty) -> (id,actual_ty,Some v))
 *         a.sa_params
 *         actual_params;
 *       sa_typ = { a.sa_typ with ts_body = Types.deref renv a.sa_typ.ts_body };
 *       sa_fulltyp = { a.sa_fulltyp with ts_body = Types.deref renv a.sa_fulltyp.ts_body };
 *       sa_ins = List.map (function (id,ty) -> (id, Types.deref renv ty)) a.sa_ins;
 *       sa_outs = List.map (function (id,ty) -> (id, Types.deref renv ty)) a.sa_outs;
 *       sa_vars = List.map (function (id,(v,ty),loc) -> (id, (apply_option (deref_exp renv) v,Types.deref renv ty), loc)) a.sa_vars;
 *       sa_rules = List.map (function (rule,ty,rsig,loc) -> (deref_rule renv rule, Types.deref renv ty, rsig, loc)) a.sa_rules;
 *       sa_impl = a.sa_impl;
 *       sa_types = a.sa_types } *)
          
and instanciate_actor tp nenv loc a args =
  (* let senv = List.fold_left (fun acc (id,v) -> match v with SVVal v' -> (id,v')::acc | _ -> acc) [] nenv in *)
  let tyins, tyouts, (*typarams,*) tyact = instanciate_actor_ios loc a args in
  let bins = List.map (fun (id,ty) -> (id,(0,ty))) a.sa_ins in
  let bouts = List.map (fun (id,ty) -> (id,([0],ty))) a.sa_outs in
  (* let bins = List.map2 (fun (id,_) ty -> (id,(0,ty))) a.sa_ins (list_of_types tyins) in
   * let bouts = List.map2 (fun (id,_) ty -> (id,([0],ty))) a.sa_outs (list_of_types tyouts) in *)
  let l, b = new_box a.sa_id tyact bins bouts in
  (* let actual_type = type_product [typarams;tyins;tyvars;tyouts] in *)
  (* add_actor_instance globals a.sa_id (actual_type,actual_fn_params) b l; *)
  let mk_wire l v = match v with
    SVLoc(i,j,ty,_) -> new_wid(), (((i,j),l),ty)
  | _ -> illegal_application loc in
  let tyins' = list_of_types tyins in
  let tyouts' = list_of_types tyouts in
  match tyins', a.sa_ins, tyouts', a.sa_outs, args with
  | [], [_], [], [_], SVUnit ->                                                 (* APP_0_0 *)
     SVUnit,
     [l,b],
     []
  | [t], [_], [], [_], SVLoc(l1,s1,ty,false) ->                                 (* APP_1_0 *)
     let w = ((l1,s1),(l,0)), t in
     SVUnit,
     [l,b],
     [new_wid(),w]
  | ts, _, [], [_], SVTuple vs when List.length ts > 1 ->                       (* APP_m_0 *)
     let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
     SVUnit,
     [l,b],
     ws''
  | [], [_], [t], [_], SVUnit ->                                                (* APP_0_1 *)
      SVLoc (l,0,t,false),
      [l,b],
      []
  | [], [_], ts, _, SVUnit when List.length ts > 1 ->                           (* APP_0_n *)
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts),
      [l,b],
      []
  | [t], _, [t'], _, SVLoc(l1,s1,ty,false) ->                                   (* APP_1_1 *)
      let w = ((l1,s1),(l,0)), t in
      SVLoc (l,0,t',false),
      [l,b],
      [new_wid(),w]
  | [t], _, ts', _, SVLoc(l1,s1,ty,false) when List.length ts' > 1 ->           (* APP_1_n *)
      let w = ((l1,s1),(l,0)), t in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts'),
      [l,b],
      [new_wid(),w]
  | ts, _, [t'], _, SVTuple vs when List.length ts > 1 ->                       (* APP_m_1 *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
      SVLoc (l,0,t',false),
      [l,b],
      ws''
  | ts, _, ts', _, SVTuple vs when List.length ts > 1 && List.length ts' > 1 -> (* APP_m_n *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts'),
      [l,b],
      ws''
  | _ ->
      illegal_application loc
  (* match List.length a.sa_ins, List.length a.sa_outs, args with
   * | 1, 1, SVUnit ->                                                  (\* APP_0_1 *\)
   *     SVLoc (l,0,List.nth tyouts' 0,false),
   *     [l,b],
   *     []
   * | 1, n, SVUnit ->                                                  (\* APP_0_n *\)
   *     SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) tyouts'),
   *     [l,b],
   *     []
   * | 1, 1, SVLoc(l1,s1,ty,false) ->                                    (\* APP_1_1 *\)
   *     let w = ((l1,s1),(l,0)), List.nth tyins' 0 in
   *     SVLoc (l,0,List.nth tyouts' 0,false),
   *     [l,b],
   *     [new_wid(),w]
   * | 1, n, SVLoc(l1,s1,ty,false) ->                                    (\* APP_1_n *\)
   *     let w = ((l1,s1),(l,0)), List.nth tyins' 0 in
   *     SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) tyouts'),
   *     [l,b],
   *     [new_wid(),w]
   * | m, 1, SVTuple vs ->                                               (\* APP_m_1 *\)
   *     let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
   *     SVLoc (l,0,List.nth tyouts' 0,false),
   *     [l,b],
   *     ws''
   * | m, n, SVTuple vs ->                                               (\* APP_m_n *\)
   *     let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
   *     SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) tyouts'),
   *     [l,b],
   *     ws''
   * | _ ->
   *     illegal_application loc *)

and instanciate_actor_ios loc a args =
  (* let ty_param = match a.sa_params with
   * | [] -> type_unit
   * | [_,ty,_] -> type_copy ty
   * | ps -> type_product (List.map (function (_,ty,_) -> type_copy ty) ps) in *)
  let rec type_of v = match v with
    (* TODO : handle the case of actors with NO arg ? *)
    SVLoc(l,s,ty,false) -> ty
  | SVTuple vs ->
      type_product
        (List.map
           (function
               SVLoc(l,s,ty,false) -> ty
             | _ -> illegal_application loc)
           vs)
  | SVCons (v', _) -> 
     type_product (Misc.list_repl (size_of_ssval v) (type_of v'))
  | SVUnit -> type_unit
  | _ -> illegal_application loc in
  let ty_arg = type_of args in
  (* match a.sa_params (\* ,a.sa_vars *\) with
   *   [] -> *)
      let ty_fn, ty_res, _ = type_application loc a.sa_typ ty_arg in
      ty_arg, ty_res, (*type_unit,*) ty_fn
  (* | _ ->
   *     let ty_fn, ty_res, tv_bindings, sv_bindings = type_application2 (ELoc loc) a.sa_typ ty_param ty_arg in
   *     ty_arg, ty_res, ty_param, ty_fn, tv_bindings, sv_bindings *)

(* RULE |- ActDecl => NE *)

let rec build_actor_desc tp { ad_desc = a } =
  let ta = List.assoc a.a_id tp.tp_actors in
  let index n i t = n ^ string_of_int (i+1), t in
  a.a_id,
  { sa_id = a.a_id;
    sa_ins = List.mapi (index "i") ta.at_ins;
    sa_outs = List.mapi (index "o") ta.at_outs;
    sa_typ = ta.at_sig }

(* RULE NE,B,W |- NetDecl => NE',B',W *)

(* let eval_simple_net_expr nenv expr =
 *   match expr.ne_desc with
 *   | NVar v ->
 *       if List.mem_assoc v nenv then 
 *         begin match List.assoc v nenv with
 *           SVVal v -> v
 *         | _ -> illegal_expression expr.ne_loc
 *         end
 *       else
 *         unbound_value_err v expr.ne_loc
 *   | NConst c -> Expr.eval_const expr.ne_loc c
 *   | NArray1Const es -> 
 *       let a = Array1.of_list (List.map (Expr.eval_const expr.ne_loc) es) in
 *       Expr.Val_array1 (Array1.size a, a)
 *   | NArray2Const ess -> 
 *       let a = Array2.of_list (List.map (List.map (Expr.eval_const expr.ne_loc)) ess) in
 *       Expr.Val_array2 (Array2.size a, a)
 *   | NArray3Const esss -> 
 *       let a = Array3.of_list (List.map (List.map (List.map (Expr.eval_const expr.ne_loc))) esss) in
 *       Expr.Val_array3 (Array3.size a, a)
 *   | _ -> illegal_expression expr.ne_loc *)

let eval_net_decl tp (nenv,boxes,wires) { nd_desc = isrec, defns; nd_loc=loc } = 
  (* TODO: inject senv here below .. *)
  let nenvs', boxes', wires' = eval_net_defns loc true isrec tp nenv defns in 
  nenv @ nenvs',
  boxes @ boxes',
  wires @ wires'

(* RULE W |- B => B' *)

exception BoxWiring of string * bid * wid

let rec update_wids wires (bid,b) =
  try
    bid, { b with
           b_ins = List.mapi
                     (fun sel (id,(_,ty)) -> id, (find_src_wire wires bid sel, ty))
                     (List.filter (fun (id,(_,ty)) -> not (is_unit_type ty)) b.b_ins);
           b_outs = List.mapi
                      (fun sel (id,(_,ty)) -> id, (find_dst_wire wires bid sel, ty))
                     (List.filter (fun (id,(_,ty)) -> not (is_unit_type ty)) b.b_outs) }
  with
    BoxWiring (where,bid,sel) -> unwired_box where b.b_name sel

and find_src_wire wires bid sel =
  let find wids (wid, ((_,(d,ds)),_)) = if d=bid && ds=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] -> raise (BoxWiring ("input",bid,sel))
  | [w] -> w
  | ws -> fatal_error "find_src_wire: more than one source wire" (* should not happen ! *)

and find_dst_wire wires bid sel =
  let find wids (wid, (((s,ss),_),_)) = if s=bid && ss=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] ->  raise (BoxWiring ("output",bid,sel))
  | ws -> ws

(* RULE NE |- Program => NE,B,W *)

let build_static tp senv p =
  let actors  = List.map (build_actor_desc tp) p.actors in
  let ne', bs', ws' =
    List.fold_left
      (eval_net_decl tp)
      (List.map (fun (id,a) -> id, SVAct a) actors @ senv, [], [])
      p.defns in
  let bs'' = List.map (update_wids ws') bs' in
  { nvals = ne';
    boxes = bs'';
    wires = ws';
    gacts = List.map (fun (id,a) -> id, { sa_desc=a }) actors }

(* Printing *)

(* let print_net_value pfx tp (name,r) =
 *   let type_of name = 
 *     try
 *       string_of_type_scheme
 *         (List.assoc
 *            name
 *            ((List.map (function (id,t) -> id,trivial_scheme t) tp.tp_ios)))
 *     with Not_found -> 
 *       begin try
 *         string_of_type_scheme
 *           ((List.assoc name (tp.tp_actors)).at_sig)
 *       with Not_found -> "?" end in
 *   printf "%sval %s : %s = %a\n" pfx name (type_of name) output_ss_value r;
 *   flush stdout *)

let string_of_typed_bin (id,(wid,ty)) = id ^ ":" ^ string_of_type ty ^ "(<-W" ^ string_of_int wid ^ ")"
let string_of_typed_bout (id,(wids,ty)) = 
    id ^ ":" ^ string_of_type ty ^ "(->["
  ^ (Misc.string_of_list (function wid -> "W" ^ string_of_int wid) "," wids) ^ "])"

let string_of_typed_io (id,ty) = id ^ ":" ^ string_of_type ty

let print_actor (id,ac) = 
  Pr_type.reset_type_var_names ();
  let a = ac.sa_desc in 
  Printf.printf "%s: %s : ins=[%s] outs=[%s]\n"
    a.sa_id
    (string_of_type_scheme a.sa_typ)
    (* (Misc.string_of_list string_of_typed_param ","  a.sa_params) *)
    (Misc.string_of_list string_of_typed_io ","  a.sa_ins)
    (Misc.string_of_list string_of_typed_io ","  a.sa_outs)

let print_box (i,b) =
  Pr_type.reset_type_var_names ();
  Printf.printf "%s%d: %s (ins=[%s] outs=[%s])\n"
        (match b.b_tag with ActorB -> "B" | DummyB -> "D")
        i
        b.b_name
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)

let print_wire (i,(((s,ss),(d,ds)),ty)) =
  Printf.printf "W%d: %s: (B%d,%d) -> (B%d,%d)\n" i (string_of_type ty) s ss d ds

(* let print_value tp (name,v) =
 *   let type_of name = 
 *       begin try string_of_type_scheme (List.assoc name tp.tp_vals)
 *       with Not_found -> "?" end in
 *   printf "val %s : %s = %s\n" name (type_of name) (Expr.string_of_val v);
 *   flush stdout *)

let dump_static sp =
  printf "Static environment ---------------\n";
  (* printf "- Values --------\n";
   * List.iter (print_value tp) sp.e_vals; *)
  printf "- Actors --------------------------\n";
  List.iter print_actor sp.gacts;
  printf "- Boxes --------------------------\n";
  List.iter print_box sp.boxes;
  printf "- Wires --------------------------\n";
  List.iter print_wire sp.wires;
  printf "----------------------------------\n"

