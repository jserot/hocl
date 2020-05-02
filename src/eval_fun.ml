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

(* Evaluation of functional graph descriptions *)

open Syntax
open Semval
open Error
open Eval

(* E, B |-p pat, v => E', B', W *)

exception Matching_fail

let rec eval_match istop env boxes pat v =
  (* [istop] tells whether we are performing a "toplevel" match [val pat = ...] or 
     a local match [let pat = ...]. *)
  match pat.p_desc, v with
  | Pat_var id, _ ->
     begin match get_located_box env boxes id, v with
     | Some (SVLoc ((d,ds,ty') as l'), dst_box), SVLoc ((s,ss,ty) as l) ->
        if dst_box.b_tag = SinkB && istop || dst_box.b_tag = RecB then
          let k, w = new_wid (), (l,l') in   (* Create new wire ... *)
          let src_box = List.assoc s boxes in  
          (if dst_box.b_tag = RecB then [id, SVLoc (d,0,ty)] else []),
          [d, set_box_input boxes d ds k;    (* Update output box input ... *)
           s, add_box_output boxes s ss k],  (* ... and src box outputs *)
          [k,w]
        else
          [id,v],
          [],
          []
     | Some _, _ ->
          binding_error pat.p_loc
     | None, _ ->
        [id,v], 
        [],
        []
     end
  | Pat_tuple ps, SVTuple vs when List.length ps = List.length vs ->
     let envs, bs, ws = List.map2 (eval_match istop env boxes) ps vs |> Misc.list_split3 in
     Misc.fold_left1 (++) envs,     
     Misc.fold_left1 (+++) bs,
     Misc.fold_left1 (++) ws     
  | Pat_ignore, _ ->
     [],
     [],
     []
  | Pat_unit, SVUnit ->
     [],
     [],
     []
  | Pat_nil, SVNil ->
     [],
     [],
     []
  | Pat_cons(p1, p2), SVCons(v1, v2) ->
      let env1, bs1, ws1  = eval_match istop env boxes p1 v1 in
      let env2, bs2, ws2  = eval_match istop env boxes p2 v2 in
      env1++env2,
      bs1+++bs2,
      ws1++ws2
  | Pat_list ps, SVList vs when List.length ps = List.length vs ->
     let envs, bs, ws = List.map2 (eval_match istop env boxes) ps vs |> Misc.list_split3 in
     Misc.fold_left1 (++) envs,     
     Misc.fold_left1 (+++) bs,
     Misc.fold_left1 (++) ws     
  | Pat_list ps, SVCons _ ->
      eval_match istop env boxes pat (list_of_cons v)
  | Pat_cons _, SVList _ ->
      eval_match istop env boxes pat (cons_of_list v)
  (* | Pat_cons _, SVTuple vs -> (\* Special case *\)
   *    matching pat (cons_of_list (SVList vs)) *)
  | _, _ ->
     raise Matching_fail

(* E, B |- expr => v, B', W *)

let rec eval_expr env boxes expr =
  match expr.e_desc with
  | EVar id ->
     let v = match lookup env expr.e_loc id with
       | SVNode n -> SVNode (copy_node n) (* Required for polymorphic nodes *)
       | v' -> v' in
     v, [], []
  | ETuple es ->
     let vs, bs, ws = List.map (eval_expr env boxes) es |> Misc.list_split3 in
     SVTuple vs, Misc.fold_left1 (+++) bs, Misc.fold_left1 (++) ws
  | EApp (fn, arg) ->
     let val_fn, bs_f, ws_f = eval_expr env boxes fn in
     begin match val_fn with
     | SVNode n ->
        if n.sn_req then (* Node [n] requires actual parameters *)
          let val_params, bs_p, ws_p = eval_param_expr env boxes arg in
          let n' = { n with sn_req=false;
                            sn_params=List.map2 (fun (id,_,ty,anns) v -> (id,v,ty,anns)) n.sn_params val_params } in
          SVNode n',
          bs_f+++bs_p,
          ws_f++ws_p
        else  (* Node [n] does not require parameters, or already got them *)
          let val_arg, bs_a, ws_a = eval_expr env boxes arg in
          let v, bs', ws' = eval_application env (boxes+++bs_a+++bs_f) (ws_a++ws_f) expr.e_loc val_fn val_arg in
          let ty = type_of_node_args n
          and ty' = type_of_semval val_arg in
          Typing.try_unify "application" ty ty' expr.e_loc; (* Late unification for polymorphic node application *)
          v,
          bs_a+++bs_f+++bs',
          ws_a++ws_f++ws'
     | _ -> 
        let val_arg, bs_a, ws_a = eval_expr env boxes arg in
        let v, bs', ws' = eval_application env (boxes+++bs_a+++bs_f) (ws_a++ws_f) expr.e_loc val_fn val_arg in
        v,
        bs_a+++bs_f+++bs',
        ws_a++ws_f++ws'
     end
  | EFun (pat,exp) ->
      SVClos {cl_pat=pat; cl_exp=exp; cl_env=env}, [], []
  | ELet (isrec, defns, body) ->
     let env', boxes', wires' = eval_definitions false expr.e_loc isrec (env,boxes) defns in
     let v, boxes'', wires'' = eval_expr (env++env') (boxes+++boxes') body in
     v, boxes'+++boxes'', wires'++wires''
  | EUnit ->
     SVUnit, [], []
  | EInt n -> SVInt n, [], []
  | EBool v -> SVBool v, [], []
  | EBinop (op,e1,e2) ->
     begin match lookup env expr.e_loc op with
     | SVPrim f ->
        let v1, bs1, ws1 = eval_expr env boxes e1 in
        let v2, bs2, ws2 = eval_expr env boxes e2 in
        f (SVTuple [v1;v2]),
        bs1+++bs2,
        ws1++ws2
     | _ ->
        illegal_application expr.e_loc (* should not happen thx to TC *)
     end
  | EIf (e1,e2,e3) ->
     let v1, bs1, ws1 = eval_expr env boxes e1 in
     let v', bs', ws' =
       begin match v1 with
       | SVBool true -> eval_expr env boxes e2
       | SVBool false -> eval_expr env boxes e3
       | _ -> Misc.fatal_error "Static.eval_expression" (* should not happen thx to TC *)
       end in 
     v', bs1+++bs', ws1++ws'
  | ENil -> SVNil, [], []
  | ECons (e1,e2) ->
      let v1, bs1, ws1 = eval_expr env boxes e1 in
      let v2, bs2, ws2 = eval_expr env boxes e2 in
      SVCons (v1,v2), bs1+++bs2, ws1++ws2
  | EList es ->
     let vs, bs, ws = List.map (eval_expr env boxes) es |> Misc.list_split3 in
     SVList vs, Misc.fold_left1 (+++) bs, Misc.fold_left1 (++) ws
  | EListElem (l,i) ->
      let v1, bs1, ws1 = eval_expr env boxes l in
      let v2, bs2, ws2 = eval_expr env boxes i in
      eval_list_access expr.e_loc v1 v2, bs1@bs2, ws1@ws2
  | EMatch (e, cases) ->
      let v, bs', ws' = eval_expr env boxes e in
      let rec seq_match = function 
        | [] -> matching_failure expr.e_loc
        | {b_desc=(p,e)} :: rest ->
            begin try
              let env_m, bs_m, ws_m = eval_match false env boxes p v in
              eval_expr (env++env_m) (boxes+++bs_m) e
            with Matching_fail ->
              seq_match rest end in
      let v', bs'', ws'' = seq_match cases in
      v', bs'+++bs'', ws'++ws''

and eval_definitions istop loc isrec (env,boxes) defns =
  let is_fun_definition = function {b_desc=({p_desc=Pat_var _}, {e_desc=EFun _})} -> true | _ -> false in
  if not isrec then                                        (* NON RECURSIVE CASE *)
    let envs, bss, wss = List.map (eval_definition istop (env,boxes)) defns |> Misc.list_split3 in
     Misc.fold_left1 (++) envs,     
     Misc.fold_left1 (+++) bss,     
     Misc.fold_left1 (++) wss     
  else                                                     (* RECURSIVE CASE *)
    if List.for_all is_fun_definition defns                (* All recursive _functions_ *)
    then begin
      let rec_cls =
        List.map
          (function
           | {b_desc={p_desc=Pat_var v}, {e_desc=EFun (p,e)}} -> v, {cl_pat=p; cl_exp=e; cl_env=[]}
           | _ -> Misc.fatal_error "Static.eval_net_defns")  (* should not happen *)
          defns in
      let env' = List.map (function (v,cl) -> (v, SVClos cl)) rec_cls in
      List.iter (fun (v,cl) -> cl.cl_env <- env' @ env) rec_cls;
      env',
      [],
      []
    end
    else
      if not (List.exists is_fun_definition defns)   (* No recursive function *)
      then begin
          let renvs, bss =
            List.map (function {b_desc=(pat,exp)} -> create_rec_bindings env boxes pat) defns |> List.split in
          let renv = Misc.fold_left1 (++) renvs in
          let rboxes = Misc.fold_left1 (++) bss in 
          let vs', bss', wss' =
            List.map (function {b_desc=(pat,exp)} -> eval_expr (env++renv) (boxes++rboxes) exp) defns |> Misc.list_split3 in
          let boxes' = Misc.fold_left1 (++) bss' in 
          let wires' = Misc.fold_left1 (++) wss' in 
          let envs', bss'', wss'' =
            try
              List.map2
                (fun {b_desc=(pat,exp)} v -> eval_match istop (env++renv) (boxes+++rboxes+++boxes') pat v)
                defns vs'
              |> Misc.list_split3 
            with
              Matching_fail -> binding_error loc in
          let env'' = Misc.fold_left1 (++) envs' in     
          let boxes'' = boxes' +++ Misc.fold_left1 (+++) bss'' in     
          let wires'' = wires' ++ Misc.fold_left1 (++) wss'' in    
          env'',
          boxes'',
          wires''
        end
      else
        illegal_rec_definition loc

and shorten_rec_paths boxes wires =
  (* For each rec box [b=<Rec,[1->k1],[1->[k2]]>], with [W(k1)=w1=<l1,.>] and [W(k2)=w2=<.,l2>], 
     we create a new wire [w3=W(k3)=<l1,l2>] and two substitutions : [w1->w3] and [w2->w3].
     We then apply the resulting set of substitutions to each non-rec box, add the created wires and
     remove the substituted ones *)
  let rboxes, boxes' = List.partition (fun (i,b) -> b.b_tag = RecB) boxes in
  let mk_subst (new_wires,deleted_wires,substs) (i,b) =
    match b.b_ins, b.b_outs with
    | [|_,k1,ty1,_|], [|_,[k2],ty2,_|] ->
       let l1,_ = List.assoc k1 wires in
       let _,l2 = List.assoc k2 wires in
       let k3 = new_wid () in
       (k3,(l1,l2))::new_wires, k1::k2::deleted_wires, (k1,k3)::(k2,k3)::substs
    | [|_,k1,ty1,_|], [|_,[],ty2,_|] ->
       let l1,_ = List.assoc k1 wires in
       new_wires, k1::deleted_wires, substs
    | _, _ -> Misc.fatal_error "Static.shorten_rec_paths" in
  let apply_substs substs (i,b) =
    let subst k = match List.assoc_opt k substs with Some k' -> k' | None -> k in
    i, { b with b_ins = Array.map (function (i,k,ty,anns) -> i, subst k, ty, anns)  b.b_ins;
                b_outs = Array.map (function (i,ks,ty,anns) -> i, List.map subst ks, ty, anns) b.b_outs } in
  let new_wires, deleted_wires, substs = List.fold_left mk_subst ([],[],[]) rboxes in
  let boxes'' = List.map (apply_substs substs) boxes' in
  let wires'' = List.filter (fun (k,w) -> not (List.mem k deleted_wires)) wires ++ new_wires in
  boxes'', wires''

and eval_list_access loc v1 v2 = 
  match v1, v2 with
  | SVList vs, SVInt k ->
     if k < List.length vs
     then List.nth vs k 
     else invalid_list_index k loc
  | SVCons _, SVInt _ ->
     eval_list_access loc (list_of_cons v1) v2
  | _, _ -> Misc.fatal_error "Static.eval_list_access" (* should not happen thx to TC *)
  
(* |-r Pat => E, B *)

and create_rec_bindings env boxes pat =
  match pat.p_desc with
  | Pat_var id ->
     begin match get_located_box env boxes id with
     | Some (l,b) when b.b_tag=SinkB->
       [id, l],  (* Do not create temporary boxes for vars bound to outputs *)
       []
     | Some _
     | None ->
        let l = new_bid () in
        let ty = pat.p_typ in
        let b = new_box l id RecB ["i",0,ty,[]] ["o",[],ty,[]] no_bval in
        [id, SVLoc(l,0,ty)],
        [(l,b)]
     end
    | Pat_tuple ps ->
       let envs', bs' = List.map (create_rec_bindings env boxes) ps |> List.split in
       Misc.fold_left1 (++) envs',
       Misc.fold_left1 (++) bs'
    | _ ->
       illegal_rec_definition pat.p_loc

 
and eval_definition istop (env,boxes) {b_desc=(pat,exp); b_loc=loc} = 
  let v, boxes', wires' = eval_expr env boxes exp in
  let env', boxes'', wires'' =
    try eval_match istop env (boxes+++boxes') pat v
    with Matching_fail -> binding_error loc in
  env',
  boxes'+++boxes'',
  wires'++wires''

and eval_application env boxes wires loc val_fn val_arg = 
  let mk_wire dst i v =
    match v with
    | SVLoc src -> src, (dst,i)
    | _ -> Misc.fatal_error "Static.eval_application.mk_wire" in
  match val_fn with
  | SVClos {cl_pat=pat; cl_exp=exp; cl_env=env} ->
      let env', _, _ =
        begin try eval_match false [] [] pat val_arg
        with Matching_fail -> binding_error pat.p_loc
        end in
      eval_expr (env++env') boxes exp
  | SVNode n ->
     let args = 
       let get_loc = function SVLoc l -> l | _ -> illegal_application loc in
       match val_arg with
       | SVUnit -> []
       | SVLoc l -> [l]
       | SVTuple vs when List.length vs = List.length n.sn_ins -> List.map get_loc vs
       | _ -> illegal_application loc in
     let params =
       List.map
         (function
          | _, SVLoc l, _, _ -> l
          | _, _, _, _ -> Misc.fatal_error "eval_application")
        n.sn_params in
     begin match eval_node_application boxes n params args with
     | [], boxes', wires' -> SVUnit, boxes', wires'
     | [v], boxes', wires' -> v, boxes', wires'
     | vs, boxes', wires' -> SVTuple vs, boxes', wires'
     end
  | _ ->
     illegal_application loc

and copy_node n =
(* Make a copy a a node description with fresh type variables when present *)
  let open Types in
  let ty =
    TyProduct [
        TyProduct (List.map (fun (_,_,ty,_) -> ty) n.sn_params);
        TyProduct (List.map (fun (_,ty,_) -> ty) n.sn_ins);
        TyProduct (List.map (fun (_,ty,_) -> ty) n.sn_outs) ] in
  match Types.type_copy ty with
    TyProduct [TyProduct ts1; TyProduct ts2; TyProduct ts3] ->
      { n with sn_params = List.map2 (fun (id,v,_,anns) ty -> id,v,ty,anns) n.sn_params ts1;
               sn_ins = List.map2 (fun (id,_,anns) ty -> id,ty,anns) n.sn_ins ts2;
               sn_outs = List.map2 (fun (id,_,anns) ty -> id,ty,anns) n.sn_outs ts3 }
  |  _ 
  | exception Invalid_argument _ ->
      Misc.fatal_error "Eval_fun.instanciate_node"
         

and eval_param_expr env boxes e =
  let get_loc v =
    match lookup env e.e_loc v with
     | SVLoc ((src,_,_) as l) -> 
        let b = List.assoc src boxes in
        if b.b_tag=InParamB then Some l else None
     | _ ->
        None in
  let extract_deps e =
    let module LocSet = Set.Make(struct type t=string*sv_loc let compare=Stdlib.compare end) in
    let rec scan e = match e.e_desc with
    | EVar v ->
       begin
         match get_loc v with
         | Some l -> LocSet.singleton (v,l)
         | None -> LocSet.empty 
       end
    | EBinop (op,e1,e2) ->
       scan e1 |> LocSet.union (scan e2)
    | _ ->
       LocSet.empty in
    scan e |> LocSet.elements in
  let ty = e.e_typ in
  match e.e_desc with
  | EInt n ->
     let bid = new_bid () in
     let bv = { bv_lit = e; bv_sub=e; bv_val = SVInt n } in
     let name = "p" ^ string_of_int bid in
     let b = new_box bid name LocalParamB [] ["o",[],ty,[]] bv in
     [SVLoc (bid,0,ty)],
     [bid,b], 
     []
  | EBool n ->
     let bid = new_bid () in
     let bv = { bv_lit = e; bv_sub=e; bv_val = SVBool n } in
     let name = "p" ^ string_of_int bid in
     let b = new_box bid name LocalParamB [] ["o",[],ty,[]] bv in
     [SVLoc (bid,0,ty)],
     [bid,b], 
     []
  | EVar v -> 
     begin
       match get_loc v with
       | Some l ->
          [SVLoc l],
          [],
          []
       | None ->
          illegal_param_expr e.e_loc
     end
  | ETuple es ->
    let vs, bss, wss = List.map (eval_param_expr env boxes) es |> Misc.list_split3 in
    List.concat vs,
    Misc.fold_left1 (+++) bss,
    Misc.fold_left1 (++) wss
  | EBinop (op,e1,e2) ->
     let slocs = extract_deps e in
     let bid = new_bid () in
     let ws = List.mapi (fun i (_,l) -> new_wid (), (l,(bid,i,ty))) slocs in
     let bins = List.mapi (fun i (wid,(l,((_,_,ty) as l'))) -> "i" ^ string_of_int (i+1), wid, ty, []) ws in
     let name = "p" ^ string_of_int bid in
     let bv = { bv_lit=e; bv_sub=subst_param_deps slocs e; bv_val=static_value env e } in 
     let b = new_box bid name LocalParamB bins ["o",[],ty,[]] bv in
     let bs' = List.map2 (fun (_,(l,s,_)) (wid,_) -> l, add_box_output boxes l s wid) slocs ws in
     [SVLoc (bid,0,ty)],
     bs'++[bid,b], 
     ws
  | _ ->
     illegal_param_expr e.e_loc

and subst_param_deps substs expr =
  let rec subst e = match e.e_desc with
    | EVar v ->
       begin match Misc.assoc_pos v substs with
       | 0 -> e (* Not found *)
       | j -> { e with e_desc = EVar ("i" ^ string_of_int j) }
       end
    | EBinop (op,e1,e2) ->
       { e with e_desc = EBinop (op, subst e1, subst e2) }
    | _ -> e in
  subst expr
       
and eval_node_application boxes n param_locs arg_locs = 
    let bid = new_bid () in
    let ws = List.mapi (fun i ((_,_,ty) as l) -> new_wid (), (l,(bid,i,ty))) (param_locs @ arg_locs) in
    let bins =
      List.map2
        (fun (id,anns) (wid,(_,(_,_,ty))) -> id,wid,ty,anns)
          (List.map (fun (id,_,_,anns) -> id, anns) n.sn_params
         @ List.map (fun (id,_,anns) -> id, anns) n.sn_ins)
        ws in
    let bouts = List.map (fun (id,ty,anns) -> id,[],ty,anns) n.sn_outs in 
    let tag = tag_of_kind n.sn_kind in
    let b = new_box bid n.sn_id tag bins bouts no_bval in
    let bs' = List.map2 (fun (l,s,ty) (wid,_) -> l, add_box_output boxes l s wid) (param_locs @ arg_locs) ws in
    let bs'' = match bs' with
      | [] -> []
      | _ -> bs' |> List.map (fun b -> [b]) |> Misc.fold_left1 (+++) in (* Merge updates to the same box *)
    let v = List.mapi (fun i (_,_,ty,_) -> SVLoc (bid,i,ty)) bouts in
    v,
    bs''++[bid,b], 
    ws

(* E, B, W |- ValDecl => E', B', W *)

let eval_val_decl (env,boxes,wires) {vd_desc=(isrec,defns); vd_loc=loc} =
  let env', boxes', wires' = eval_definitions true loc isrec (env,boxes) defns in
  env ++ env',
  boxes +++ boxes',
  wires ++ wires'

(* E, B |- ValDecls => B', W *)
  
let eval_val_decls (env,boxes) defns =
 let env', boxes', wires =
    List.fold_left
      eval_val_decl
      (env, boxes, [])
      defns in
  let boxes'', wires' = shorten_rec_paths boxes' wires in
  env', boxes'', wires'


