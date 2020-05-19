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

(* Aux *)

let copy_node n =
(* Make a copy a a node description with fresh type variables when present *)
  match n.sn_supplied_ins with
  | [] ->
     let open Types in
     let ty =
       TyProduct [
           TyProduct (List.map (fun (_,ty,_,_) -> ty) n.sn_ins);
           TyProduct (List.map (fun (_,ty,_) -> ty) n.sn_outs) ] in
     begin match Types.type_copy ty with
       TyProduct [TyProduct ts1; TyProduct ts2] ->
        { n with sn_ins = List.map2 (fun (id,ty,e,anns) ty -> id,ty,e,anns) n.sn_ins ts1;
                 sn_supplied_ins = [];
                 sn_outs = List.map2 (fun (id,ty,anns) ty' -> id,ty',anns) n.sn_outs ts2 }
     |  _ 
        | exception Invalid_argument _ ->
         Misc.fatal_error "Eval_fun.instanciate_node"
     end
  | _ -> (* Copy has already been done *)
     n

let remove_label l expr =
    (* Ex: [remove_label "l2" (Fun ~l1:x -> Fun ~l2:y -> (x,y)]
         = [(Fun ~l1:x -> (x,x), "y")] *)
    let rec remove e = match e.e_desc with
    | EFun ({fp_desc=(l',id)} as pat, e') ->
       if l="" || l=l'
       then
         e', id
       else
         let e'', id = remove e' in
         { e_desc=EFun (pat, e''); e_loc=e.e_loc; e_typ=Types.remove_label l expr.e_typ (* TO FIX ? *)  },
         id
    | _ ->
       Misc.fatal_error "Eval_fun.remove_label" (* should not happen *) in
    remove expr

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
  | EApp (fn, (lbl,arg)) ->
     let val_fn, bs_f, ws_f = eval_expr env boxes fn in
     let val_arg, bs_a, ws_a = eval_expr env boxes arg in
     begin match val_fn with
     | SVClos {cl_pat=(lbl',id,_) as pat; cl_exp=exp; cl_env=env'} ->
         if lbl = "" || lbl = lbl' then
           let env'' = (id,val_arg)::env' in
           let v', bs', ws' = eval_expr env'' (boxes+++bs_a+++bs_f) exp in
           v',
           bs_a+++bs_f+++bs',
           ws_a++ws_f++ws'
         else
           let exp', id' = remove_label lbl exp in
           SVClos { cl_pat=pat; cl_exp=exp'; cl_env = (id',val_arg)::env' },
           bs_a+++bs_f,
           ws_a++ws_f
     | SVNode n ->
        (* let m = List.length n.sn_ins in
         * let k = List.length n.sn_supplied_ins in *)
        let n' =
          begin
            match n.sn_ins with
              [] -> n (* input-less node *)
            | _ -> set_node_input lbl val_arg n
          end in
        (* if m = 0 || k = m-1 then                        (\* The supplied arg was the last missing one *\) *)
        if n'.sn_ins = [] then                             (* All arguments have been supplied *)
          let v, bs', ws' = eval_node_application env (boxes+++bs_a+++bs_f) (ws_a++ws_f) expr.e_loc n' in
          v,
          bs_a+++bs_f+++bs',
          ws_a++ws_f++ws'
        else                                               (* Some argument(s) are still missing *)
          SVNode n',
          bs_a+++bs_f,
          ws_a++ws_f
     | _ ->
        illegal_application expr.e_loc
     end
  | EFun ({fp_desc=lbl,id; fp_typ=ty}, exp) ->
      SVClos {cl_pat=lbl,id,ty; cl_exp=exp; cl_env=env}, [], []
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
       | _ -> Misc.fatal_error "Eval_fun.eval_expression" (* should not happen thx to TC *)
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
  | EQuote e ->
     let l, bs_p, ws_p = eval_param_expr env boxes e in
     SVLoc l,
     bs_p,
     ws_p

and set_node_input lbl v n = 
  let supplied, remains =
    match Misc.list_extract (fun (id,_,_,_) -> lbl="" || lbl=id) n.sn_ins with
    | Some (id,ty,e,anns), rest -> [id,ty,v,anns], rest   (* TO FIX : [e] is ignored here ! *)
    | None, _ -> Misc.fatal_error "Eval_fun.set_node_input" in (* should not happen *)
  { n with sn_supplied_ins = n.sn_supplied_ins @ supplied;
           sn_ins = remains }

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
           | {b_desc={p_desc=Pat_var v}, {e_desc=EFun ({fp_desc=lbl,id; fp_typ=ty},e)}} ->
              v, {cl_pat=lbl,id,ty; cl_exp=e; cl_env=[]}
           | _ ->
              Misc.fatal_error "Eval_fun.eval_net_defns")  (* should not happen *)
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
    | _, _ -> Misc.fatal_error "Eval_fun.shorten_rec_paths" in
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
  | _, _ -> Misc.fatal_error "Eval_fun.eval_list_access" (* should not happen thx to TC *)
  
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

and eval_node_application env boxes wires loc n = 
  match instanciate_node loc boxes n with
  | [], boxes', wires' -> SVUnit, boxes', wires'
  | [v], boxes', wires' -> v, boxes', wires'
  | vs, boxes', wires' -> SVTuple vs, boxes', wires'

and instanciate_node loc boxes n = 
  let arg_locs =
    List.map
      (fun (_,ty,v,_) ->
        match v with
        | SVLoc ((src,sel,ty') as l) ->
           Typing.try_unify ~relax:true "application" ty ty' loc;  (* Late unification for polymorphic node application *)
           l
        | _ ->
           Misc.fatal_error "Eval_fun.instanciate_node")
      n.sn_supplied_ins in
    let bid = new_bid () in
    let ws =
      List.mapi
        (fun i ((_,_,ty) as l) -> new_wid (), (l,(bid,i,ty)))
        arg_locs in
    let bins =
      List.map2
        (fun (id,ty,v,anns) (wid,(_,(_,_,ty'))) -> id,wid,ty',anns)
        n.sn_supplied_ins
        ws in
    let bouts = List.map (fun (id,ty,anns) -> id,[],ty,anns) n.sn_outs in 
    let tag = tag_of_kind n.sn_kind in
    let b = new_box bid n.sn_id tag bins bouts no_bval in
    let bs' = List.map2 (fun (l,s,ty) (wid,_) -> l, add_box_output boxes l s wid) arg_locs ws in
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


