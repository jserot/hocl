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
open Error
open Misc
open Types
open Typing
open Printf
open Pr_type
open Ssval

type cfg = {
  mutable insert_bcasts: bool;
  (* mutable insert_fifos: bool; *)
  bcast_name: string;
  fifo_name: string;
  }

let cfg = {
  insert_bcasts = false;
  (* insert_fifos = false; *)
  bcast_name = "bcast";
  fifo_name = "fifo";
  }
         
type static_env = (string * ss_val) list

type box_tag = 
    ActorB
  | BcastB
  | SourceB
  | SinkB
  | GraphB
  | LocalParamB 
  | InParamB 
  | DummyB  (* Temporary boxes used for handling recursive defns *)

and ss_box = {
    b_id: int;
    b_tag: box_tag;
    b_name: string;                  (* For regular (resp. param) boxes, name of the instanciated actor (resp. param) *)
    b_typ: typ;                      (* "Functional" type, i.e. either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
    (* b_tvbs: typ var_bind list;    (\* Type var instantiations (when the box derives from a polymorphic actor) *\) *)
    b_params: (string * (typ * ss_val)) list;      (* Parameters, with their actual values *)
    b_ins: (string * (wid * typ * Syntax.io_annot)) list;
    b_outs: (string * (wid list * typ * Syntax.io_annot)) list;
    mutable b_val: b_val;            (* For parameter boxes *)
}

and wid = int
and bid = int

and b_val = { 
    bv_lit: net_expr;     (* Original expression *)
    bv_sub: net_expr;     (* After dependency binding *)
    bv_val: ss_val        (* Statically computed value - SVUnit if N/A *)
  }

type ss_wire = (sv_loc * sv_loc) * typ * bool   (* src, dest, type, is parameter dependency *)

(* The result of the static analysis *)

type static_program = { 
    gparams: (string * ss_box) list;
    gacts: (string * sa_desc) list;
    boxes: (bid * ss_box) list;
    wires: (wid * ss_wire) list;
    nvals: (string * ss_val) list;
    (* gcsts: (string * gc_desc) list;             (\* Global constants *\)
     * gtyps: (string * Typing.tc_desc) list;      (\* Globally defined types *\) *)
    pragmas: Syntax.pragma_desc list
  }

and sa_desc = {                                                     (* Actors *)
    sa_desc: Ssval.sv_act;                                          (* Definition *)
    sa_insts: bid list                                              (* Instances *)
  }

let is_valid_param_value = function
  | SVNat _ | SVBool _ -> true  (* MAY BE ADJUSTED  *)
  | _ -> false

let box_name sp (i,b) =
  match b.b_tag with
  | BcastB ->
     b.b_name ^ "_" ^ string_of_int i
  | ActorB ->
     let a =
       try List.assoc b.b_name sp.gacts
       with Not_found -> Misc.fatal_error ("Static.box_name(" ^ b.b_name ^ ")") (* should not happen *) in
     if List.length a.sa_insts > 1
     then b.b_name ^ "_" ^ string_of_int i
     else b.b_name
  | _ ->
     b.b_name

(* Box creation *)

let new_bid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let no_bval =
  let no_expr = {ne_desc=NUnit; ne_loc=Location.no_location; ne_typ=no_type } in
  { bv_lit=no_expr; bv_sub=no_expr; bv_val=SVUnit }
          
let new_box kind name ty params ins outs =
  let bid = new_bid () in
  let tag = match kind with A_Regular -> ActorB | A_Bcast -> BcastB | A_Graph -> GraphB in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=params; b_ins=ins; b_outs=outs; b_typ=ty; b_val=no_bval }

let new_io_box kind name ty (*params*) =
  let bid = new_bid () in
  let tag = match kind with Io_src -> SourceB | Io_snk -> SinkB in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=[](*params*); b_ins=[]; b_outs=[]; b_typ=ty; b_val=no_bval }

let new_param_box name kind ty v =
  let bid = new_bid () in
  let tag = match kind with P_Input -> InParamB | P_Local -> LocalParamB in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=[]; b_ins=[]; b_outs=[]; b_typ=ty; b_val=v }

let new_dummy_box name ty =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=DummyB; b_name=name; b_params=[]; b_ins=[]; b_outs=["r",([0],ty,no_annot)]; b_typ=ty; b_val=no_bval }

let boxes_of_wire boxes (((s,ss),(d,ds)),ty,_) = 
  try
    List.assoc s boxes, List.assoc d boxes
  with Not_found -> 
    fatal_error "Static.boxes_of_wire"  (* should not happen *)

let src_box_of_wire boxes (w:ss_wire) = fst (boxes_of_wire boxes w)
let dst_box_of_wire boxes (w:ss_wire) = snd (boxes_of_wire boxes w)

(*** NETWORK LEVEL ***)

(* Rules NE [,B] |-n NPat, rho => NE', W *)

exception Matching_fail

let rec net_matching toplevel nenv npat r = match npat.np_desc, r with
  | NPat_var id, r' ->
     [id, r], []
  | NPat_tuple ps, SVTuple rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching toplevel nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_bundle ps, SVList rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching toplevel nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_bundle ps, SVCons _ ->
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
  | NPat_cons _, SVTuple vs ->
     matching pat (cons_of_list (SVList vs))
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
  | NBundle es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr tp nenv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVList rs, bs, ws
  | NBundleElem (l,i) ->
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
  match val_fn, val_arg with
  | SVPrim f, _ -> f val_arg, [], []
  | SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv'}, _ ->   (* RULE NAPP CLO *)
      let nenv'', _ =
        begin try net_matching false [] npat val_arg
        with Matching_fail -> binding_error npat.np_loc
        end in
      eval_net_expr tp (nenv'' @ nenv') nexp
  | SVAct a, _ ->
     begin match a.sa_params with
     | [] ->
        (* Actor with all parameter values set *)
        instanciate_actor tp nenv loc a val_arg
     | vs when List.for_all (function _,_,None -> false | _,_,Some _ -> true) vs ->
        (* Parameter-less actor *)
        instanciate_actor tp nenv loc a val_arg
     | vs when List.for_all (function _,_,None -> true | _,_,Some _ -> false) vs ->
        (* The actor accepts parameters but they have not been set yet *)
        begin
          try 
            let actual_params =
              begin
                let is_valid_param = function SVLoc _ -> true | _ -> false in (* Crude approx. *)
                match val_arg, ty_arg with
                | SVTuple vs, TyProduct ts when List.for_all is_valid_param vs -> List.combine vs ts
                |  v, ty when is_valid_param v -> [v,ty]
                | _, _ -> invalid_actor_param a.sa_id loc 
              end in
            SVAct { a with sa_params = List.map2 (fun (id,ty,_) (v,ty') -> (id,ty,Some v)) a.sa_params actual_params },
            [],
            []
          with
            Invalid_argument _ -> failwith "Static.eval_net_application" (* should not happen *)
        end
     | _ ->
        illegal_application loc
     end
  | SVLoc (l, s, TyArrow(ty, ty'), v), _ when is_unit_type ty ->
     (* Special case for parameter-less source boxes *)
     SVLoc (l, s, ty', v),
     [],
     []
  | SVLoc (l, s, TyArrow(ty'', TyArrow(ty, ty')), SVUnit), SVLoc (l1,s1,_,_) when is_unit_type ty ->
     (* Special case for parameterized source boxes *)
     let w = ((l1,s1),(l,s)), ty'', true in
     SVLoc (l, s, TyArrow(ty,ty'), val_arg),
     [],
     [new_wid(),w]
  | SVLoc (l, s, TyArrow(ty, ty'), v), SVLoc (l1,s1,_,_) when is_unit_type ty' ->
     (* Special case for parameter-less sink boxes *)
     let w = ((l1,s1),(l,s)), ty, false in
     SVUnit,
     [],
     [new_wid(),w]
  | SVLoc (l, s, TyArrow(ty'', TyArrow(ty, ty')), SVUnit), SVLoc (l1,s1,_,_) when is_unit_type ty' ->
     (* Special case for parameterized sink boxes *)
     let w = ((l1,s1),(l,s)), ty'', true in
     SVLoc (l, s, TyArrow(ty,ty'), val_arg),
     [],
     [new_wid(),w]
  | _, _ ->
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
       let ty = type_copy npat.np_typ in
       let l, b = new_dummy_box id ty in
       [id, SVLoc(l,0,ty,SVUnit)], [(l,b)]
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

and apply_subst ((i,s),(i',s')) (wid,((src,dst),ty,b)) =
  let sub (k,l) =
    if k=i then (i',s') else (k,l) in
  (wid, ((sub src, sub dst), ty, b))


(* Auxilliaries *)

and instanciate_actor tp nenv loc a args =
  let tyins, tyouts, typarams, tyact = instanciate_actor_ios loc a args in
  let bins =
      List.map (fun (id,ty,_) -> (id,(0,ty,no_annot))) a.sa_params 
    @ List.map (fun (id,ty,ann) -> (id,(0,ty,ann))) a.sa_ins in
  let bparams =
    let static_value v = match v with
      | Some (SVLoc (_,_,_,v')) -> v'
      | Some v' -> v'
      | None -> Misc.fatal_error "Static.instanciate_actor.static_value" in
    List.map (function (id,ty,v) -> id,(ty, static_value v)) a.sa_params in
  let bouts = List.map (fun (id,ty,ann) -> (id,([0],ty,ann))) a.sa_outs in
  let l, b = new_box a.sa_kind a.sa_id tyact bparams bins bouts in
  let mk_wire b l v = match v with
    SVLoc(i,j,ty,_) -> new_wid(), (((i,j),l),ty,b)
  | _ -> illegal_application loc in
  let tyins' = list_of_types tyins in
  let tyouts' = list_of_types tyouts in
  let wps =
    List.mapi
      (fun i p -> match p with (id,ty,Some v) -> mk_wire true (l,i) v | _ -> illegal_application loc)
      a.sa_params in
  let np = List.length a.sa_params in
  let args' = match tyins', args with
    | ts, SVCons _ when List.length ts > 1 ->  (* Bundle to tuple conversion *)
       begin match list_of_cons args with
       | SVList vs when List.length vs = List.length ts -> SVTuple vs
       | _ -> illegal_application loc
       end
    | _, _ -> args in
  match tyins', a.sa_ins, tyouts', a.sa_outs, args' with
  | [], [_], [], [_], SVUnit ->                                                 (* APP_0_0 *)
     SVUnit,
     [l,b],
     wps
  | [t], [_], [], [_], SVLoc(l1,s1,ty,_) ->                                     (* APP_1_0 *)
     let w = ((l1,s1),(l,np)), t, false in
     SVUnit,
     [l,b],
     wps @ [new_wid(),w]
  | ts, _, [], [_], SVTuple vs when List.length ts > 1 ->                       (* APP_m_0 *)
     let ws'' = Misc.list_map_index (fun i v -> mk_wire false (l,np+i) v) vs in
     SVUnit,
     [l,b],
     wps @ ws''
  | [], [_], [t], [_], SVUnit ->                                                (* APP_0_1 *)
      SVLoc (l,0,t,SVUnit),
      [l,b],
      []
  | [], [_], ts, _, SVUnit when List.length ts > 1 ->                           (* APP_0_n *)
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,SVUnit)) ts),
      [l,b],
      wps
  | [t], _, [t'], _, SVLoc(l1,s1,ty,_) ->                                       (* APP_1_1 *)
      let w = ((l1,s1),(l,np)), t, false in
      SVLoc (l,0,t',SVUnit),
      [l,b],
      wps @ [new_wid(),w]
  | [t], _, ts', _, SVLoc(l1,s1,ty,_) when List.length ts' > 1 ->               (* APP_1_n *)
      let w = ((l1,s1),(l,np)), t, false in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,SVUnit)) ts'),
      [l,b],
      wps @ [new_wid(),w]
  | ts, _, [t'], _, SVTuple vs when List.length ts > 1 ->                       (* APP_m_1 *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire false (l,np+i) v) vs in
      SVLoc (l,0,t',SVUnit),
      [l,b],
      wps @ ws''
  | ts, _, ts', _, SVTuple vs when List.length ts > 1 && List.length ts' > 1 -> (* APP_m_n *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire false (l,np+i) v) vs in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,SVUnit)) ts'),
      [l,b],
      wps @ ws''
  | _ ->
      illegal_application loc

and instanciate_actor_ios loc a args =
  let ty_param = match a.sa_params with
  | [] -> type_unit
  | [_,ty,_] -> ty
  | ps -> type_product (List.map (function (_,ty,_) -> ty) ps) in
  let rec type_of v = match v with
    (* TODO : handle the case of actors with NO arg ? *)
    SVLoc(_,_,ty,_) -> ty
  | SVTuple vs ->
      type_product
        (List.map
           (function
               SVLoc(_,_,ty,_) -> ty
             | _ -> illegal_application loc)
           vs)
  | SVCons (v', _) -> 
     type_product (Misc.list_repl (size_of_ssval v) (type_of v'))
  | SVUnit -> type_unit
  | _ -> illegal_application loc in
  let ty_arg = type_of args in
  match a.sa_params with
    [] ->
      let ty_fn, ty_res, _ = type_application loc a.sa_typ ty_arg in
      ty_arg, ty_res, type_unit, ty_fn
  | _ ->
      let ty_fn, ty_res, _ = type_application2 loc a.sa_typ ty_param ty_arg in
      ty_arg, ty_res, ty_param, ty_fn

(* RULE |- ActDecl => NE *)

let rec build_actor_desc tp { ad_desc = a } =
  let ta = List.assoc a.a_id tp.tp_actors in
  a.a_id,
  { sa_kind = a.a_kind;
    sa_id = a.a_id;
    sa_params = List.map (fun (id,ty) -> id,ty,None) ta.at_params;
    sa_ins = ta.at_ins;
    sa_outs = ta.at_outs;
    sa_typ = ta.at_sig }

(* RULE NE,B,W |- NetDecl => NE',B',W *)

let eval_net_decl tp (nenv,boxes,wires) { nd_desc = isrec, defns; nd_loc=loc } = 
  (* TODO: inject senv here below .. *)
  let nenvs', boxes', wires' = eval_net_defns loc true isrec tp nenv defns in 
  nenv @ nenvs',
  boxes @ boxes',
  wires @ wires'

let subst_deps names e =
  let rec subst e = match e.ne_desc with
  | NVar v ->
     begin match Misc.list_pos v names with
     | None -> e
     | Some k -> { e with ne_desc = NVar ("i" ^ string_of_int (k+1)) }
     end
  | NNat _ | NBool _ | NUnit -> e
  | NTuple es -> { e with ne_desc = NTuple (List.map subst es) }
  | NApp (fn, arg) -> { e with ne_desc = NApp (subst fn, subst arg) }
  | _ -> Misc.fatal_error "Static.subst_deps" 
  in
  subst e
  
let rec eval_param_decl tp nenv (ne,bs,ws) { pd_desc=id,kind,ty,e; pd_loc=loc } =
  let ty = List.assoc id tp.tp_params in
  let bv, dep_params = match kind with
    | P_Local ->
       begin
         match eval_net_expr tp (nenv @ ne) e with
         | v, _, _' when is_valid_param_value v -> 
            let dep_params = extract_dep_params tp ne e in 
            { bv_lit = e;
              bv_sub = subst_deps (List.map fst dep_params) e;
              bv_val = v }, dep_params
         | _ -> invalid_param_value id loc
       end
    | P_Input ->
       no_bval, [] in
  let l, b = new_param_box id kind ty bv in
  let ws' =
    List.map 
      (function
       | id, SVLoc (l',s',ty,_) -> new_wid (), (((l',s'),(l,0)),ty,true)
       | _, _ -> invalid_param_value id loc)
      dep_params in
  (id,SVLoc (l,0,ty,bv.bv_val)) :: ne, (l,b) :: bs, ws'@ws

and extract_dep_params tp env e =
  let rec extract e = match e.ne_desc with
  | NNat n -> []
  | NBool b -> []
  | NApp (fn, arg) -> extract fn @ extract arg
  | NVar v ->
     if List.mem_assoc v tp.tp_params then
       try [v, List.assoc v env]
       with Not_found -> fatal_error "Static.extract_dep_params" (* should not happen *)
     else
       []
  | NTuple es -> List.concat (List.map extract es)
  | _ -> invalid_param_expr e.ne_loc in
  extract e

let rec eval_io_decl tp nenv (ne,bs,ws) { io_desc=kind,id,params,ty; io_loc=loc } =
  let ty = List.assoc id tp.tp_ios in
  let l, b = new_io_box kind id ty in
  (id,SVLoc (l,0,ty,SVUnit)) :: ne, (l,b) :: bs, ws (* TODO: attach params *)
    
(* RULE W |- B => B' *)

(* Network transformations *)

(* Insert BCASTers 
 * 
 *        +--------+         +--------+          +--------+         +---------+          +--------+
 *        |        |         |        |          |        |         |         |    w1'   |        |
 *        |        |    w1   |        |          |        |    w'   |        0|--------->|        |
 *        |   A1 k1|-------->|k2 A2   |    ===>  |   A1 k1|-------->|0 Bcast  |          |k2 A2   |
 *        |        |\        |        |          |        |         |        1|----+     |        |
 *        |        | |       |        |          |        |         |         |    |     |        |
 *        +--------+ |       +--------+          +--------+         +---------+    |     +--------+
 *                   |                                                             |
 *                   |       +--------+                                            |     +--------+
 *                   |       |        |                                        w2' |     |        |
 *                   |  w2   |        |                                            |     |        |
 *                   +------>|k3 A3   |                                            +---->|k3 A3   |
 *                           |        |                                                  |        |
 *                           |        |                                                  |        |
 *                           +--------+                                                  +--------+
*)

let new_bcast_box ty wid wids =
  let bid = new_bid () in
  let bos = Misc.list_map_index (fun i wid -> "o_" ^ string_of_int (i+1), ([wid],ty,no_annot)) wids in 
  bid, { b_id=bid; b_tag=BcastB; b_name=cfg.bcast_name; b_params=[]; b_ins=["i",(wid,ty,no_annot)]; b_outs=bos; b_typ=ty; b_val=no_bval }

let rec is_bcast_box boxes bid = (find_box boxes bid).b_name = cfg.bcast_name

and find_box boxes bid = 
    try List.assoc bid boxes
    with Not_found -> Misc.fatal_error "Static.find_box: cannot find box from id" (* should not happen *)

let rec insert_bcast bid oidx (boxes,wires,box) bout = 
  match bout with
  | (id, ([],_,_)) -> boxes, wires, box       (* should not happen ? *)
  | (id, ([wid],_,_)) -> boxes, wires, box    (* no need to insert here *)
  | (id, (wids,ty,ann)) ->                    (* the relevant case : a box output connected to several wires *)
      let wid' = new_wid () in
      let m, mb = new_bcast_box ty wid' wids in
      let box' = { box with b_outs = Misc.assoc_replace id (function _ -> [wid'],ty,ann) box.b_outs } in
      let wires' = Misc.foldl_index (update_wires m) wires wids in
      let boxes' = Misc.assoc_replace bid (function b -> box') boxes in
      (m,mb) :: boxes', (wid',(((bid,oidx),(m,0)),ty,is_dep_wire wires (List.hd wids))) :: wires', box'

and is_dep_wire wires wid =
  try
    match List.assoc wid wires with
    | _,_,b -> b
  with
  | Not_found -> 
     Misc.fatal_error "Static.is_dep_wire"

and update_wires s' j wires wid = 
  Misc.assoc_replace wid (function ((s,ss),(d,ds)),ty,b -> ((s',j),(d,ds)),ty,b) wires 

let insert_bcast_after (boxes,wires) (bid,box) = 
  match box.b_tag with
  | LocalParamB
  | InParamB
  | SourceB 
  | ActorB -> (* TO BE FIXED ? *)
      let boxes', wires', box' = Misc.foldl_index (insert_bcast bid) (boxes,wires,box) box.b_outs in
      boxes', wires'
  | _ ->
      boxes, wires

let insert_bcasters sp = 
  let boxes', wires' = List.fold_left insert_bcast_after (sp.boxes,sp.wires) sp.boxes in
  { sp with boxes = boxes'; wires = wires' }

(* Insert FIFOs - not used here 
 * 
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *        |       |    w    |       |          |       |    w'   |        |    w''   |        |
 *        |  A1  k|-------->|k' A2  |    ===>  |  A1  k|-------->|0 FIFO 0|--------->|k' A2   |
 *        |       |         |       |          |       |         |        |          |        |
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *
*)

(* let new_fifo_box ty w wid' wid'' =
 *   let bid = new_bid () in
 *   bid, { b_id=bid; b_tag=ActorB; b_name=cfg.fifo_name; b_ins=["i",(wid',ty,no_annot)]; b_outs=["o",([wid''],ty,no_annot)];
 *          b_typ=ty; b_val=no_bval }
 * 
 * let rec insert_fifo (boxes,wires') (wid,wire) =
 *   match wire with
 *   | ((s,ss),(d,ds)), _, _ when is_bcast_box boxes d  ->
 *       boxes, (wid,wire) :: wires'
 *         (\* Do not insert anything between a box output and a bcaster.
 *            This is useless since buffering will be done in the FIFOs _after_ the splitter. TBC *\)
 *   | ((s,ss),(d,ds)), ty, true ->
 *       boxes, (wid,wire) :: wires'
 *         (\* Do not insert anything on wires representing a parameter dependency *\)
 *   | ((s,ss),(d,ds)), ty, false ->
 *       let wid' = new_wid () in
 *       let wid'' = new_wid () in
 *       let f, fb = new_fifo_box ty wid wid' wid'' in
 *       let w' = ((s,ss),(f,0)), ty, false in
 *       let w'' = ((f,0),(d,ds)), ty, false in
 *       let boxes' = update_bouts wid wid' s boxes in
 *       let boxes'' = update_bins wid wid'' d boxes' in
 *       (f,fb) :: boxes'', (wid',w') :: (wid'',w'') :: wires'
 * 
 * and update_bouts wid wid' s boxes = 
 *   let replace_wire b_outs =
 *     List.map (function
 *         id,([w],ty,b) when w = wid -> id,([wid'],ty,b)   (\* TODO : handle case when an output is bound to several wires ? *\)
 *                                                      (\* Maybe not necessary if splitters have been inserted *\)
 *       | o -> o) b_outs  in
 *   Misc.assoc_replace s (function b -> { b with b_outs = replace_wire b.b_outs }) boxes 
 * 
 * and update_bins wid wid' s boxes = 
 *   let replace_wire b_ins =
 *     List.map (function
 *         id,(w,ty,b) when w = wid -> id,(wid',ty,b)
 *       | i -> i) b_ins  in
 *   Misc.assoc_replace s (function b -> { b with b_ins = replace_wire b.b_ins }) boxes 
 * 
 * let is_fifo_wire boxes (wid,(_,_,is_dep_wire)) = not is_dep_wire
 * 
 * let insert_fifos sp =
 *   let boxes', wires' = List.fold_left insert_fifo (sp.boxes, []) sp.wires in
 *   { sp with boxes = boxes'; wires = wires' } *)

(* RULE NE |- Program => NE,B,W *)

exception BoxWiring of string * bid * wid

let rec update_wids (wires: (wid * (((bid*sel)*(bid*sel)) * typ * bool)) list) (bid,b) =
  try
    bid,
    (match b.b_tag with
     | ActorB | BcastB | GraphB ->
        { b with
          b_ins = List.mapi
                        (fun sel (id,(_,ty,ann)) -> id, (find_src_wire wires bid sel, ty, ann))
                        (List.filter (fun (id,(_,ty,_)) -> not (is_unit_type ty)) b.b_ins);
          b_outs = List.mapi
                      (fun sel (id,(_,ty,ann)) -> id, (find_dst_wire wires bid sel, ty, ann))
                      (List.filter (fun (id,(_,ty,_)) -> not (is_unit_type ty)) b.b_outs) }
     | SourceB -> 
        { b with b_outs = add_source_outputs wires b.b_name bid }
     | SinkB -> 
        { b with b_ins = add_sink_inputs wires b.b_name bid }
     | LocalParamB | InParamB -> 
        { b with
          b_ins = add_param_inputs wires bid;
          b_outs = add_param_outputs wires bid }
     | DummyB ->
        Misc.fatal_error "Static.update_wids" (* should not happen *))
  with
    BoxWiring (what,bid,sel) -> invalid_box_wiring what b.b_name sel

and add_source_outputs wires bname bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, _)) -> s=bid && ss=0)
      wires in
  match ws with
    [] -> []
  | (_,(_,ty,_))::_ -> [bname, (List.map fst ws, ty, no_annot)]

and add_sink_inputs wires bname bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, _)) -> d=bid && ds=0)
      wires in
  match ws with
    [] -> []
  | [(wid,(_,ty,_))] -> [bname, (wid, ty, no_annot)]
  | _ -> List.mapi (fun i (wid,(_,ty,_)) -> bname ^ string_of_int (i+1), (wid, ty, no_annot)) ws

and add_param_inputs wires bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, is_dep_wire)) -> d=bid && ds=0 && is_dep_wire)
      wires in
  List.mapi (fun i (wid,(_,ty,_)) -> "i" ^ string_of_int (i+1), (wid, ty, no_annot)) ws

and add_param_outputs wires bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, is_dep_wire)) -> s=bid && ss=0 && is_dep_wire)
      wires in
  match ws with
    [] -> []
  | (_,(_,ty,_))::_ -> ["o", (List.map fst ws, ty, no_annot)]

and find_src_wire wires bid sel =
  let find wids (wid, ((_,(d,ds)),_,_)) = if d=bid && ds=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] -> raise (BoxWiring ("no wire connected to input",bid,sel))
  | [w] -> w
  | ws -> raise (BoxWiring ("more than one wire connected to input",bid,sel))

and find_dst_wire wires bid sel =
  let find wids (wid, (((s,ss),_),_,_)) = if s=bid && ss=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] ->  raise (BoxWiring ("no wire connected to output",bid,sel))
  | ws -> ws

let collect_actor_insts name boxes =
  List.fold_left
    (fun acc (id,b) -> if b.b_name = name then id::acc else acc)
    []
    boxes
  
let build_static tp senv p =
  let senv_p, bs_p, ws_p = List.fold_left (eval_param_decl tp senv) ([],[],[]) p.params in
  let senv_i, bs_i, ws_i = List.fold_left (eval_io_decl tp senv_p) ([],[],[]) p.ios in
  let actors  = List.map (build_actor_desc tp) p.actors in
  let senv_a = List.map (fun (id,a) -> id, SVAct a) actors in
  let ne', bs', ws' =
    List.fold_left
      (eval_net_decl tp)
      (senv_p @ senv_i @ senv_a @ senv, [], [])
      p.defns in
  let ws'' = ws' @ ws_p @ ws_i in
  let bs'' = List.map (update_wids ws'') (bs' @ bs_p @ bs_i) in
  { nvals = ne';
    boxes = bs'';
    wires = ws'';
    gacts =
      List.map
        (fun (id,a) -> id, { sa_desc=a; sa_insts=collect_actor_insts id bs'})
        actors;
    gparams =
      List.map
        (fun (_,b) -> b.b_name, b)
        (List.filter (fun (_,b) -> b.b_tag=LocalParamB || b.b_tag=InParamB) bs'');
    pragmas = List.map (fun d -> d.Syntax.pr_desc) p.pragmas }
  |> (if cfg.insert_bcasts then insert_bcasters else Misc.id)
  (* |> (if cfg.insert_fifos then insert_fifos else Misc.id) *)
  
let extract_special_boxes name sp =
  List.fold_left
    (fun acc (id,b) -> if b.b_name = name then (id,b)::acc else acc)
    []
    sp.boxes

let extract_bcast_boxes sp = extract_special_boxes cfg.bcast_name sp
let extract_fifo_boxes sp = extract_special_boxes cfg.fifo_name sp

let get_pragma_desc cat name sp =
  let rec find = function
    | [] -> []
    | (cat', name'::args) :: _ when cat'=cat && name'=name -> args
    | _::rest -> find rest in
  find sp.pragmas

(* Printing *)

let string_of_typed_bin (id,(wid,ty,_)) = id ^ ":" ^ string_of_type ty ^ "(<-W" ^ string_of_int wid ^ ")"
let string_of_typed_bout (id,(wids,ty,_)) = 
    id ^ ":" ^ string_of_type ty ^ "(->["
  ^ (Misc.string_of_list (function wid -> "W" ^ string_of_int wid) "," wids) ^ "])"

let string_of_typed_io (id,ty,ann) = id ^ ":" ^ string_of_type ty ^ Syntax.string_of_io_annot ann
let string_of_opt_value = function None -> "?" | Some v -> string_of_ssval v
let string_of_typed_param (id,ty,v) = id ^ ":" ^ string_of_type ty ^ " = " ^ string_of_opt_value v

let print_param (n,b) =
  Printf.printf "%s: val=\"%s\"=%s ins=[%s] outs=[%s]\n"
        n
        (string_of_net_expr (b.b_val.bv_lit))
        (string_of_ssval (b.b_val.bv_val))
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)

let print_actor (id,ac) = 
  Pr_type.reset_type_var_names ();
  let a = ac.sa_desc in 
  Printf.printf "%s: %s : params=[%s] ins=[%s] outs=[%s] (->%s)\n"
    a.sa_id
    (string_of_type_scheme a.sa_typ)
    (Misc.string_of_list string_of_typed_param ","  a.sa_params)
    (Misc.string_of_list string_of_typed_io ","  a.sa_ins)
    (Misc.string_of_list string_of_typed_io ","  a.sa_outs)
    (Misc.string_of_list (function bid -> "B" ^ string_of_int bid) "," ac.sa_insts)

let box_prefix b = match b.b_tag with
    BcastB -> "Y"
  | SourceB -> "I"
  | SinkB -> "O"
  | ActorB -> "B"
  | DummyB -> "D"
  | LocalParamB -> "L"
  | InParamB -> "P"
  | GraphB -> "G"

let print_box (i,b) =
  Pr_type.reset_type_var_names ();
  Printf.printf "%s%d: %s (ins=[%s] outs=[%s] %s)\n"
        (box_prefix b)
        i
        b.b_name
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)
        (match b.b_tag with LocalParamB -> "val=" ^ string_of_net_expr b.b_val.bv_lit | _ -> "")

let print_wire (i,(((s,ss),(d,ds)),ty,b)) =
  Printf.printf "W%d: %s: (B%d,%d) %s (B%d,%d)\n" i (string_of_type ty) s ss (if b then "+>" else "->") d ds

let dump_static sp =
  printf "Static environment ---------------\n";
  printf "- Parameters ----------------------\n";
  List.iter print_param sp.gparams;
  printf "- Actors --------------------------\n";
  List.iter print_actor sp.gacts;
  printf "- Boxes --------------------------\n";
  List.iter print_box sp.boxes;
  printf "- Wires --------------------------\n";
  List.iter print_wire sp.wires;
  printf "----------------------------------\n"

