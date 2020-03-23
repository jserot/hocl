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
open Semval
open Interm

type cfg = {
  mutable insert_bcasts: bool;
  (* mutable insert_fifos: bool; *)
  bcast_name: string;
  (* fifo_name: string; *)
  }

let cfg = {
  insert_bcasts = false;
  (* insert_fifos = false; *)
  bcast_name = "bcast";
  (* fifo_name = "fifo"; *)
  }

type static_env = (string * sem_val) list

let augment_env env' env = env' @ env

let lookup_env loc senv id = 
      if List.mem_assoc id senv then List.assoc id senv
      else unbound_value_err id loc

(* Core level *)
          
let rec eval_core_expr senv expr =
  let lookup id = 
      if List.mem_assoc id senv then List.assoc id senv
      else unbound_value_err id expr.ce_loc in
  match expr.ce_desc with
  | EVar v -> lookup v
  | EInt n -> SVInt n
  | EBool v -> SVBool v
  | EBinop (op, e1,e2) ->
     begin match lookup op with
     | SVPrim f ->
        let v1 = eval_core_expr senv e1 in
        let v2 = eval_core_expr senv e2 in
        f (SVTuple [v1;v2])
     | _ ->
        illegal_application expr.ce_loc (* should not happen thx to TC *)
     end

let is_valid_param_value = function
  | SVInt _ | SVBool _ -> true  (* MAY BE ADJUSTED  *)
  | _ -> false

(* let box_name sp (i,b) =
 *   match b.b_tag with
 *   (\* | IBcastB ->
 *    *    b.b_name ^ "_" ^ string_of_int i *\)
 *   | ActorB ->
 *   (\* | EBcastB -> *\)
 *      let a =
 *        try List.assoc b.b_name sp.gacts
 *        with Not_found -> Misc.fatal_error ("Static.box_name(" ^ b.b_name ^ ")") (\* should not happen *\) in
 *      if List.length a.sa_insts > 1
 *      then b.b_name ^ "_" ^ string_of_int i
 *      else b.b_name
 *   | _ ->
 *      b.b_name *)

(* Box creation *)

let new_bid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let no_bval =
  let no_expr = {ce_desc=EVar ""; ce_loc=Location.no_location; ce_typ=no_type } in
  { bv_lit=no_expr; bv_sub=no_expr; bv_val=SVUnit }
          
let new_box tag name ty ins outs =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=[]; b_ins=ins; b_outs=outs; b_typ=ty; b_val=no_bval }

let new_io_box ?(bv=no_bval) tag name ty  =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=[]; b_ins=[]; b_outs=[]; b_typ=ty; b_val=bv }

let new_param_box tag ty v =
  let bid = new_bid () in
  let name = "p" ^ string_of_int bid in
  bid, { b_id=bid; b_tag=tag; b_name=name; b_params=[]; b_ins=[]; b_outs=[]; b_typ=ty; b_val=v }

let new_dummy_box name ty =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=DummyB; b_name=name; b_params=[]; b_ins=[]; b_outs=["r",([0],ty,[])]; b_typ=ty; b_val=no_bval }

let boxes_of_wire boxes (((s,ss),(d,ds)),ty,_) = 
  try
    List.assoc s boxes, List.assoc d boxes
  with Not_found -> 
    fatal_error "Static.boxes_of_wire"  (* should not happen *)

let src_box_of_wire boxes (w:sv_wire) = fst (boxes_of_wire boxes w)
let dst_box_of_wire boxes (w:sv_wire) = snd (boxes_of_wire boxes w)

(*** Structural evaluation ***)

exception BoxWiring of string * bid * wid

let rec update_wids (wires: (wid * (((bid*sel)*(bid*sel)) * typ * wire_kind)) list) (bid,b) =
  try
    bid,
    (match b.b_tag with
     | ActorB | IBcastB | EBcastB | GraphB (* | DelayB *) ->
        { b with
          b_ins = List.mapi
                        (fun sel (id,(_,ty,anns)) -> id, (find_src_wire wires bid sel, ty, anns))
                        (List.filter (fun (id,(_,ty,_)) -> not (is_unit_type ty)) b.b_ins);
          b_outs = List.mapi
                      (fun sel (id,(_,ty,anns)) -> id, (find_dst_wire wires bid sel, ty, anns))
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
  | (_,(_,ty,_))::_ -> [bname, (List.map fst ws, ty, [])]

and add_sink_inputs wires bname bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, _)) -> d=bid && ds=0)
      wires in
  match ws with
    [] -> []
  | [(wid,(_,ty,_))] -> [bname, (wid, ty, [])]
  | _ -> List.mapi (fun i (wid,(_,ty,_)) -> bname ^ string_of_int (i+1), (wid, ty, [])) ws

and add_param_inputs wires bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, kind)) -> d=bid && ds=0 && kind=ParamW)
      wires in
  List.mapi (fun i (wid,(_,ty,_)) -> "i" ^ string_of_int (i+1), (wid, ty, [])) ws

and add_param_outputs wires bid =
  let ws =
    List.filter
      (function (wid, (((s,ss),(d,ds)), ty, kind)) -> s=bid && ss=0 && kind=ParamW)
      wires in
  match ws with
    [] -> []
  | (_,(_,ty,_))::_ -> ["o", (List.map fst ws, ty, [])]

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

let rec static_value senv expr =
  match expr.ce_desc with
  | EVar v ->
     let sv = lookup_env expr.ce_loc senv v in
     if is_static_const sv
     then sv
     else SVUnit 
  | EInt n -> SVInt n
  | EBool v -> SVBool v
  | EBinop (op, e1,e2) ->
     begin match lookup_env expr.ce_loc senv op with
     | SVPrim f ->
        let v1 = static_value senv e1 in
        let v2 = static_value senv e2 in
        if is_static_const v1 && is_static_const v2
        then f (SVTuple [v1;v2])
        else SVUnit 
     | _ ->
        SVUnit
     end

let rec eval_param_expr senv expr =
  let eval_int_param n = 
     let l, b = new_param_box LocalParamB type_int { bv_lit = expr; bv_sub=expr; bv_val = SVInt n } in
     SVLoc (l,0,type_int,SV_Param),
     [l,b],
     [] in
  let eval_bool_param n =
     let l, b =  new_param_box LocalParamB type_bool { bv_lit = expr; bv_sub=expr; bv_val = SVBool n } in
     SVLoc (l,0,type_bool,SV_Param),
     [l,b],
     [] in
  match expr.ce_desc with
  | EVar v ->
     begin
       match lookup_env expr.ce_loc senv v with
       | SVInt n -> eval_int_param n
       | SVBool n -> eval_bool_param n
       | v' -> v', [], []
     end
  | EInt n -> eval_int_param n
  | EBool n -> eval_bool_param n
  | EBinop (op, e1,e2) ->
     let v =  static_value senv expr in 
     let ty = expr.ce_typ in
     let l, b =  new_param_box LocalParamB ty { bv_lit = expr; bv_sub=expr; bv_val = static_value senv expr } in 
     let bindings = extract_param_deps senv (l,0) expr in (* TO FIX !!! Will not be 0 for multi-deps !! *)
     if List.length bindings > 1 then
       Misc.not_implemented "Static.eval_param_expression: multi-dependent parameter expression";
     let _ = b.b_val <- { b.b_val with bv_sub=subst_param_deps bindings b.b_val.bv_lit } in
     SVLoc (l,0,ty,SV_Param),
     [l,b],
     List.map snd bindings

and extract_param_deps senv dst expr =
  let rec extract acc e = match e.ce_desc with
    | EInt _ -> acc
    | EBool _ -> acc
    | EVar v ->
       if List.mem_assoc v acc then
         acc (* Already bound *)
       else
         begin match lookup_env expr.ce_loc senv v with
         | SVLoc(i,j,ty,SV_Param) -> (* [v] refers to an input parameter *)
            let src = i,j in
            let w = new_wid(), ((src,dst),ty,ParamW) in
            (v,w)::acc               (* Bind [v] to the refering wire *)
         | _ -> acc
         end
    | EBinop (_,e1,e2) ->
       extract (extract acc e1) e2 in
  extract [] expr

and subst_param_deps bindings expr =
  let rec subst e = match e.ce_desc with
    | EInt _ -> e
    | EBool _ -> e
    | EVar v ->
       begin match Misc.assoc_pos v bindings with
       | 0 -> e (* Not found *)
       | j -> { e with ce_desc = EVar ("i" ^ string_of_int j) }
       end
    | EBinop (op,e1,e2) ->
       { e with ce_desc = EBinop (op, subst e1, subst e2) } in
  subst expr
  
let string_of_box_tag t = match t with
  | EBcastB -> "EBcast"
  | _ -> "other"

let tag_of_kind = function
  | SV_Actor -> ActorB
  | SV_Graph -> GraphB

let eval_gnode_decl gid tp (senv,bs,ws) { gn_desc = nid, n; gn_loc = loc } =
  let lookup id =
    try List.assoc id senv
    with Not_found -> Misc.fatal_error "Static.eval_gnode_decl.lookup" in
  let update senv k v = Misc.assoc_update k v senv in
  let ni, tag, ty =
    match lookup n.gn_name with
    | SVNode (n,_) -> n, tag_of_kind n.sn_kind, type_instance n.sn_typ
    | _ -> illegal_node_instanciation loc in
  let is_real_io (id,ty,_) = not (is_unit_type ty) in
  let bins =
      List.map
      (fun (id,ty) -> (id,(0,ty,[])))
      ni.sn_params
    @ List.map
      (fun (id,ty,anns) -> (id,(0,ty,anns)))
      (List.filter is_real_io  ni.sn_ins) in
  let bouts =
    List.map
      (fun (id,ty,anns) -> (id,([0],ty,anns)))
      (List.filter is_real_io  ni.sn_outs) in
  let l, b = new_box tag ni.sn_id ty (*[]*) bins bouts in
  let mk_param_wire dst (senv,ws,bs) expr =
    match eval_param_expr senv expr with
    | SVLoc(i,j,ty,_), bs', ws' ->
       let src = i,j in
       senv,
       (new_wid(), ((src,dst),ty,ParamW)) :: ws' @ ws,
       bs @ bs'
    | _, _, _ -> Misc.fatal_error "Static.eval_gnode_decl" in
  let mk_inp_wire dst (senv,ws) id =
    match lookup id with
    | SVLoc(i,j,ty,_) ->
       (* The corresponding box input is connected to another box output *)
       let src = i,j in
       senv,
       (new_wid(), ((src,dst),ty,DataW)) :: ws
    | SVWire (wid, ((l,l'),ty,_)) when l=sv_no_loc && l'=sv_no_loc ->
       (* The corresponding box input is connected to a yet unconnected wire *)
       update senv id (SVWire (wid, ((sv_no_loc,dst),ty,DataW))),
       ws
    | SVWire (wid, ((src,l'),ty,_)) when l'=sv_no_loc ->
       (* The corresponding box input is connected to src-connected wire *)
       update senv id (SVWire (wid, ((src,dst),ty,DataW))),
       (wid, ((src,dst),ty,DataW)) :: ws
    | SVWire (wid, ((_,dst),_,_)) ->
       (* The corresponding box input is connected to an already-connected wire *)
       multiply_connected_wire gid id
    | _ -> Misc.fatal_error "Static.eval_gnode_decl" in
  let mk_outp_wire src (senv,ws) id =
    match lookup id with
    | SVLoc(i,j,ty,_) ->
       (* The corresponding box output is connected to another box input *)
       let dst = i,j in
       senv,
       (new_wid(), ((src,dst),ty,DataW)) :: ws
    | SVWire (wid, ((l,l'),ty,_)) when l=sv_no_loc && l'=sv_no_loc ->
       (* The corresponding box output is connected to a yet unconnected wire *)
       update senv id (SVWire (wid, ((src,sv_no_loc),ty,DataW))),
       ws
    | SVWire (wid, ((l,dst),ty,_)) when l=sv_no_loc ->
       (* The corresponding box output is connected to a dst-connected wire *)
       update senv id (SVWire (wid, ((src,dst),ty,DataW))),
       (wid, ((src,dst),ty,DataW)) :: ws
    | SVWire (wid, ((_,dst),_,_)) ->
       (* The corresponding box output is connected to an already-connected wire *)
       multiply_connected_wire gid id
    | _ -> Misc.fatal_error "Static.eval_gnode_decl" in
  let np = List.length ni.sn_params in
  let senv_p, ws_p, bs_p = (* connect param wires *)
    Misc.foldl_index
      (fun k (se,ws,bs) id -> mk_param_wire (l,k) (se,ws,bs) id)
      (senv,[],[])
      n.gn_params in
  let senv_i, ws_i = (* connect input wires *)
    Misc.foldl_index
      (fun k (se,ws) id -> mk_inp_wire (l,np+k) (se,ws) id)
      (senv_p,[])
      n.gn_ins in
  let senv_o, ws_o = (* connect output wires *)
    Misc.foldl_index
      (fun k (se,ws) id -> mk_outp_wire (l,k) (se,ws) id)
      (senv_i,[])
      n.gn_outs in
  senv_o, bs @ bs_p @ [l,b], ws @ ws_p @ ws_i @ ws_o

let eval_param_value = function
  | None -> no_bval
  | Some v -> { bv_lit=v; bv_sub=v; bv_val=eval_core_expr [] v }
       
let rec eval_param_decl (env,boxes) (id,ty) v =
  let v' = eval_param_value v in
  let l, b = new_io_box InParamB id ty ~bv:v' in
  (id,SVLoc (l,0,ty,SV_Param)) :: env,
  (l,b) :: boxes

let eval_io_decl tag (env,boxes) (id,(ty,_)) =
    let l, b = new_io_box tag id ty in
    let tag' = match tag with SourceB -> SV_Source | SinkB -> SV_Sink | _ -> SV_None in
    (id,SVLoc (l,0,ty,tag')) :: env,
    (l,b) :: boxes

let eval_wire_decl { gw_desc = id, {te_typ=ty} } =
  id, SVWire (new_wid(), new_wire ty DataW)
  
let check_wire gid (id,sv) = match sv with
  | SVWire (wid,((src,dst),_,_)) when src=sv_no_loc || dst=sv_no_loc -> incomplete_wire gid id 
  | _ -> ()

let is_real_io (id,(ty,_)) = not (is_unit_type ty)

let eval_struct_graph_desc gid tp senv intf params g =
  let env_p, bs_p =
    try List.fold_left2 eval_param_decl ([],[]) intf.t_params params
    with Invalid_argument _ -> Misc.fatal_error "Static.eval_struct_graph_desc" in
 let env_i, bs_i = List.fold_left (eval_io_decl SourceB) ([],[]) (List.filter is_real_io intf.t_ins) in
 let env_o, bs_o = List.fold_left (eval_io_decl SinkB) ([],[]) (List.filter is_real_io intf.t_outs) in
 let env_w = List.map eval_wire_decl g.gs_wires in
 let senv' = senv |> augment_env env_p |> augment_env (env_i @ env_o @ env_w) in
 let senv'', boxes, wires =
    List.fold_left
      (eval_gnode_decl gid tp)
      (senv', bs_p @ bs_i @ bs_o, [])
      g.gs_nodes in
 let _ = List.iter (check_wire gid) senv'' in
 let boxes' = List.map (update_wids wires) boxes in
 boxes', wires
           
(*** Functional evaluation ***)

let is_source_loc sv = match sv with
  | SVLoc (_,_,_,SV_Source) -> true
  | _ -> false
       
let is_sink_loc sv = match sv with
  | SVLoc (_,_,_,SV_Sink) -> true
  | _ -> false

let is_param_loc sv = match sv with
  | SVLoc (_,_,_,SV_Param) -> true
  | _ -> false
       
exception Matching_fail

let rec net_matching nenv npat r =
  let mk_wire l l' = match l, l' with
    | SVLoc (i,s,ty,_), SVLoc (i',s',_,_) -> new_wid(), (((i,s),(i',s')),ty,DataW)
    | _, _ -> Misc.fatal_error "Static.net_matching" in
  let oenv = List.filter (fun (id,sv) -> is_sink_loc sv) nenv in 
  match npat.np_desc, r with
  | NPat_var id, _ ->
     (* If the bound var designates an output, create a wire to the corresponding box *)
     let ws =
       begin match r, List.assoc id oenv with
              | (SVLoc _ as l), (SVLoc _ as l') -> [mk_wire l l']
              | _, _ -> []
              | exception Not_found -> []
       end in
     [id, r], ws
  | NPat_tuple ps, SVTuple rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching (*oenv*) nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_tuple ps, SVList rs when List.length ps = List.length rs -> (* implicit bundle to tuple conversion *)
      let nenvs, ws = List.split (List.map2 (net_matching (*oenv*) nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_tuple ps, SVCons _ ->
      net_matching (*oenv*) nenv npat (list_of_cons r)
  | NPat_bundle ps, SVList rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching (*oenv*) nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_bundle ps, SVCons _ ->
      net_matching (*oenv*) nenv npat (list_of_cons r)
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

let rec eval_net_expr senv expr =
  let lookup v =
    if List.mem_assoc v senv then List.assoc v senv
    else unbound_value_err v expr.ne_loc in
  match expr.ne_desc with
  | NInt n -> SVInt n, [], []
  | NBool b -> SVBool b, [], []
  | NUnit -> SVUnit, [], []
  | NVar v -> lookup v, [], []
  | NPVar (v, params) ->
     let val_params, bs_p, ws_p = List.fold_left (eval_net_param senv) ([],[],[]) params in
     let r = match lookup v with
        | SVNode (n,[]) -> SVNode (n, val_params) 
        | _ -> illegal_application expr.ne_loc in (* Only actors and graphs can take parameters *)
     r, bs_p, ws_p
  | NNil -> SVNil, [], []
  | NTuple es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr senv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVTuple rs, bs, ws
  | NCons (e1,e2) ->
      let v1, bs1, ws1 = eval_net_expr senv e1 in
      let v2, bs2, ws2 = eval_net_expr senv e2 in
      SVCons (v1,v2), bs1@bs2, ws1@ws2
  | NBundle es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr senv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVList rs, bs, ws
  | NBundleElem (l,i) ->
      let v1, bs1, ws1 = eval_net_expr senv l in
      let v2, bs2, ws2 = eval_net_expr senv i in
      eval_list_access expr.ne_loc v1 v2, bs1@bs2, ws1@ws2
  | NIf (e1,e2,e3) ->
     let v1, bs1, ws1 = eval_net_expr senv e1 in
     let v', bs', ws' =
       begin match v1 with
       | SVBool true -> eval_net_expr senv e2
       | SVBool false -> eval_net_expr senv e3
       | _ -> illegal_expression expr (* should not happen *)
       end in 
     v', bs1@bs', ws1@ws'
  | NApp (fn, arg) ->
      let val_fn, bs_f, ws_f = eval_net_expr senv fn in
      let val_arg, bs_a, ws_a = eval_net_expr senv arg in
      let r, bs'', ws'' =
        eval_net_application (bs_a@bs_f,ws_a@ws_f) senv expr.ne_loc val_fn val_arg in
      r, bs'' @ bs_a @ bs_f, ws'' @ ws_a @ ws_f
  | NFun (npat,nexp) ->
      SVClos {cl_pat=npat; cl_exp=nexp; cl_env=senv}, [], []
  | NLet (isrec, defns, body) ->
     let nenv',boxes,wires = eval_net_defns expr.ne_loc isrec senv defns in (* no output to bind here *)
     let r',boxes',wires' = eval_net_expr (augment_env nenv' senv) body in
     r', boxes' @ boxes, wires' @ wires
  | NMatch (e, bs) ->
      let v, bs', ws' = eval_net_expr senv e in
      let rec seq_match = function 
        | [] -> matching_failure expr.ne_loc
        | { nb_desc=(p,e) } :: bs ->
            begin try
              let nenv' = matching p v in
              eval_net_expr (augment_env nenv' senv) e
            with Matching_fail ->
              seq_match bs end in
      let v', bs'', ws'' = seq_match bs in
      v', bs'@bs'', ws'@ws''

and eval_net_application (bs,ws) senv loc val_fn val_arg = 
  let lookup id =
    try List.assoc id senv
    with Not_found -> Misc.fatal_error "Static.eval_net_application.lookup" in
  match val_fn, val_arg with
  | SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv'}, _ ->
      let nenv'', _ =
        begin try net_matching [] npat val_arg
        with Matching_fail -> binding_error npat.np_loc
        end in
      eval_net_expr (nenv'' @ nenv') nexp
      (* TO FIX: adding [senv] here creates duplicates since all the scoped symbols are already in [cl_env] (?) *)
      (* eval_net_expr nodes (augment_env (nenv'' @ nenv') senv) nexp *)
  | SVNode (n, val_params), _ -> 
     let tag = tag_of_kind n.sn_kind in
     instanciate_actor_or_graph tag senv loc n val_params val_arg
  | SVPrim f, _ -> f val_arg, [], [] 
  | _, _ ->
     illegal_application loc

and eval_net_param senv (acc,bs,ws) expr =
    match eval_param_expr senv expr with
    | (SVLoc(_,_,_,_) as l), bs', ws' ->
       acc @ [l],
       bs @ bs',
       ws' @ ws
    | _ -> Misc.fatal_error "Static.eval_net_param"

(* val npat1=nexp1 ... and npatn=nexpn => NE', B, W *)

and eval_net_defns loc isrec senv bindings =
  if not isrec then                                        (* NON RECURSIVE CASE *)
    let eval_net_binding {nb_desc=npat,nexp} =
      let v, bs', ws' = eval_net_expr senv nexp in
      let nenv', ws'' =
        try net_matching (*oenv*) senv npat v
        with Matching_fail -> binding_error npat.np_loc in
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
      List.iter (function (v,cl) -> cl.cl_env <- nenv' @ senv) rec_cls;
      nenv', [], []
    end
    else if not (List.exists is_fun_definition bindings)   (* No recursive function *)
    then begin
      let rec_env, bs =
        List.fold_left
          (fun (env,bs) {nb_desc=npat,nexp} ->
            let env',bs' = create_rec_bindings (*oenv*) senv npat in
            env @ env', bs @ bs')
          ([],[])
          bindings in
      let vs', bs', ws' =
        List.fold_left
          (fun (vs,bs,ws) {nb_desc=npat,nexp} ->
             let v',bs',ws' = eval_net_expr (augment_env rec_env senv) nexp in
             vs @ [v'], bs @ bs', ws @ ws')
          ([],[],[])
          bindings in
      let nenv', ws'' =
        List.fold_left2
          (fun (env,ws) {nb_desc=npat,nexp} v ->
            let env',ws' =
              try net_matching (*oenv*) senv npat v
              with Matching_fail -> binding_error npat.np_loc in
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
  | SVList vs, SVInt k ->
     if k < List.length vs
     then List.nth vs k 
     else invalid_list_index k loc
  | SVCons _, SVInt _ ->
     eval_list_access loc (list_of_cons v1) v2
  | _, _ -> fatal_error "Static.eval_list_access" (* should not happen thx to TC *)

(* RULES REC PATTERN 1, 2 :  |-r NPat => NE, B *)

and create_rec_bindings nenv npat =
  (* TODO: take [oenv] into account to create wire to outputs *)
  match npat.np_desc with
    | NPat_var id ->
       let ty = type_copy npat.np_typ in
       let l, b = new_dummy_box id ty in
       [id, SVLoc(l,0,ty,SV_None)], [(l,b)]
    | NPat_tuple ps ->
        List.fold_left
          (fun (ne,bs) p ->
            let ne', bs' = create_rec_bindings (*oenv*) nenv p in
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

and instanciate_actor_or_graph tag nenv loc n params args =
  let is_real_io (id,ty,_) = not (is_unit_type ty) in
  let bparams =
    List.map
      (fun (id,ty) -> (id,(0,ty,[])))
      n.sn_params in
  let bins =
    List.map
      (fun (id,ty,anns) -> (id,(0,ty,anns)))
      (List.filter is_real_io n.sn_ins) in
  let bouts =
    List.map
      (fun (id,ty,anns) -> (id,([0],ty,anns)))
      (List.filter is_real_io n.sn_outs) in
  (* let tag' = match n.sn_kind with BRegular -> tag | BBcast -> EBcastB in *)
  let tag' = tag in 
  let l, b = new_box tag' n.sn_id (type_instance n.sn_typ) (bparams @ bins) bouts in
  let mk_wire kind dst v = match v with
    SVLoc(i,j,ty,_) -> new_wid(), (((i,j),dst),ty,kind)
  | _ -> illegal_application loc in
  let tyins' = List.map Misc.snd3 n.sn_ins in
  let tyouts' = List.map Misc.snd3 n.sn_outs in
  let wps = 
    List.mapi
      (fun i v -> mk_wire ParamW (l,i) v)
      params in
  let np = List.length n.sn_params in
  let args' = match tyins', args with
    | ts, SVCons _ when List.length ts > 1 ->  (* Bundle to tuple conversion *)
       begin match list_of_cons args with
       | SVList vs when List.length vs = List.length ts -> SVTuple vs
       | _ -> illegal_application loc
       end
    | _, _ -> args in
  let type_of (_,(_,ty,_)) = ty in
  match bins, bouts, args' with
  | [], [], SVUnit ->                                                 (* APP_0_0 *)
     SVUnit,
     [l,b],
     wps
  | [bi], [], SVLoc(l1,s1,ty,_) ->                                     (* APP_1_0 *)
     let w = ((l1,s1),(l,np)), type_of bi, DataW in
     SVUnit,
     [l,b],
     wps @ [new_wid(),w]
  |  _, [], SVTuple vs ->                                             (* APP_m_0 *)
     let ws'' = List.mapi (fun i v -> mk_wire DataW (l,np+i) v) vs in
     SVUnit,
     [l,b],
     wps @ ws''
  | [], [bo], SVUnit ->                                                (* APP_0_1 *)
      SVLoc (l,0,type_of bo,SV_None),
      [l,b],
      wps
  | [], bos, SVUnit ->                                                  (* APP_0_n *)
      SVTuple (List.mapi (fun i bo -> SVLoc(l,i,type_of bo,SV_None)) bos),
      [l,b],
      wps
  | [bi], [bo], SVLoc(l1,s1,ty,_) ->                                    (* APP_1_1 *)
      let w = ((l1,s1),(l,np)), type_of bi, DataW in
      SVLoc (l,0,type_of bo,SV_None),
      [l,b],
      wps @ [new_wid(),w]
  | [bi], bos, SVLoc(l1,s1,ty,_) ->                                      (* APP_1_n *)
      let w = ((l1,s1),(l,np)), type_of bi, DataW in
      SVTuple (List.mapi (fun i bo -> SVLoc(l,i,type_of bo,SV_None)) bos),
      [l,b],
      wps @ [new_wid(),w]
  | _, [bo], SVTuple vs ->                                             (* APP_m_1 *)
      let ws'' = List.mapi (fun i v -> mk_wire DataW (l,np+i) v) vs in
      SVLoc (l,0,type_of bo,SV_None),
      [l,b],
      wps @ ws''
  | bis, bos, SVTuple vs ->                                               (* APP_m_n *)
      let ws'' = List.mapi (fun i v -> mk_wire DataW (l,np+i) v) vs in
      SVTuple (List.mapi (fun i bo -> SVLoc(l,i,type_of bo,SV_None)) bos),
      [l,b],
      wps @ ws''
  | _ ->
      illegal_application loc


let eval_net_decl (senv,boxes,wires) { nd_desc = isrec, defns; nd_loc=loc } = 
  let nenv', boxes', wires' = eval_net_defns loc isrec senv defns in 
  augment_env nenv' senv,
  boxes @ boxes',
  wires @ wires'

let subst_deps names e =
  let rec subst e = match e.ce_desc with
  | EVar v ->
     begin match Misc.list_pos v names with
     | None -> e
     | Some k -> { e with ce_desc = EVar ("i" ^ string_of_int (k+1)) }
     end
  | EInt _ | EBool _ -> e
  | EBinop (op, e1, e2) -> { e with ce_desc = EBinop (op, subst e1, subst e2) }
  in
  subst e
  

let eval_fun_graph_desc tp senv intf params defns =
 let senv_p, bs_p =
    try List.fold_left2 eval_param_decl ([],[]) intf.t_params params
    with Invalid_argument _ -> Misc.fatal_error "Static.eval_fun_graph_desc" in
 let senv_i, bs_i = List.fold_left (eval_io_decl SourceB) ([],[]) (List.filter is_real_io intf.t_ins) in
 let senv_o, bs_o = List.fold_left (eval_io_decl SinkB) ([],[]) (List.filter is_real_io intf.t_outs) in
 let senv' = senv |> augment_env senv_p |> augment_env (senv_i @ senv_o) in
 let senv'', bs', ws' =
    List.fold_left
      eval_net_decl
      (senv', [], [])
      defns in
  (* let wires = ws' @ ws_p @ add_source_wires @ add_dest_wires in *)
  let boxes = List.map (update_wids ws') (bs' @ bs_p @ bs_i @ bs_o) in
  boxes, ws'

(* Network transformations *)

(* Insert (implicit) BCASTs 
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
  let bos = List.mapi (fun i wid -> "o_" ^ string_of_int (i+1), ([wid],ty,[])) wids in 
  bid, { b_id=bid; b_tag=IBcastB; b_name=cfg.bcast_name; b_params=[];
         b_ins=["i",(wid,ty,[])]; b_outs=bos; b_typ=ty; b_val=no_bval }

let is_bcast_box boxes bid = (find_box boxes bid).b_name = cfg.bcast_name

let rec insert_bcast bid oidx (boxes,wires,box) bout = 
  match bout with
  | (id, ([],_,_)) -> boxes, wires, box       (* should not happen ? *)
  | (id, ([wid],_,_)) -> boxes, wires, box    (* no need to insert here *)
  | (id, (wids,ty,anns)) ->                 (* the relevant case : a box output connected to several wires *)
      let wid' = new_wid () in
      let m, mb = new_bcast_box ty wid' wids in
      let box' = { box with b_outs = Misc.assoc_replace id (function _ -> [wid'],ty,anns) box.b_outs } in
      let wires' = Misc.foldl_index (update_wires m) wires wids in
      let boxes' = Misc.assoc_replace bid (function b -> box') boxes in
      (m,mb) :: boxes', (wid',(((bid,oidx),(m,0)),ty,get_wire_kind wires (List.hd wids))) :: wires', box'

and get_wire_kind wires wid =
  try
    match List.assoc wid wires with
    | _,_,k -> k
  with
  | Not_found -> 
     Misc.fatal_error "Static.get_wire_kind"

and update_wires s' j wires wid = 
  Misc.assoc_replace wid (function ((s,ss),(d,ds)),ty,kind -> ((s',j),(d,ds)),ty,kind) wires 

let insert_bcast_after after_boxes (boxes,wires) (bid,box) = 
  if List.mem box.b_tag after_boxes then 
    let boxes', wires', box' = Misc.foldl_index (insert_bcast bid) (boxes,wires,box) box.b_outs in
    boxes', wires'
  else
    boxes, wires

let insert_bcasters after_boxes ir = 
  let insert g =
    let boxes', wires' = List.fold_left (insert_bcast_after after_boxes) (g.sg_boxes,g.sg_wires) g.sg_boxes in
    { sg_boxes = boxes'; sg_wires = wires' } in
  let insert_g (id,g) = id, insert g in
  let insert_n (id,n) = match n.sn_impl with
    | NI_Graph g -> id, {n with sn_impl = NI_Graph (insert g)}
    | _ -> id,n in
  let insert_t (id,g) = id, { g with tg_impl = insert g.tg_impl } in
  { ir with
    ir_nodes = List.map insert_n ir.ir_nodes; 
    ir_graphs = List.map insert_t ir.ir_graphs }

(* Insert FIFOs - not used here 
 * 
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *        |       |    w    |       |          |       |    w'   |        |    w''   |        |
 *        |  A1  k|-------->|k' A2  |    ===>  |  A1  k|-------->|0 FIFO 0|--------->|k' A2   |
 *        |       |         |       |          |       |         |        |          |        |
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *)

(* let new_fifo_box ty w wid' wid'' =
 *   let bid = new_bid () in
 *   bid, { b_id=bid; b_tag=ActorB; b_name=cfg.fifo_name; b_ins=["i",(wid',ty,no_annot)]; b_outs=["o",([wid''],ty,no_annot)];
 *          b_typ=ty (* b_val=no_bval *) }
 * 
 * let rec insert_fifo (boxes,wires') (wid,wire) =
 *   match wire with
 *   | ((s,ss),(d,ds)), _, _ when is_bcast_box boxes d  ->
 *       boxes, (wid,wire) :: wires'
 *         (* Do not insert anything between a box output and a bcaster.
 *            This is useless since buffering will be done in the FIFOs _after_ the splitter. TBC *)
 *   | ((s,ss),(d,ds)), ty, true ->
 *       boxes, (wid,wire) :: wires'
 *         (* Do not insert anything on wires representing a parameter dependency *)
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
 *         id,([w],ty,b) when w = wid -> id,([wid'],ty,b)   (* TODO : handle case when an output is bound to several wires ? *)
 *                                                      (* Maybe not necessary if splitters have been inserted *)
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

(* Graph evaluator *)

let build_static_node id kind intf = {
      sn_id = id;
      sn_kind = kind;
      sn_params = List.map (fun (id,ty) -> id, ty) intf.t_params;
      sn_ins = List.map (fun (id,(ty,anns)) -> id,ty,anns) intf.t_ins;
      sn_outs = List.map (fun (id,(ty,anns)) -> id,ty,anns) intf.t_outs;
      sn_typ = intf.t_sig }

let eval_graph_decl tp nodes (acc,senv) { g_desc=g } =
  let intf =
    try fst (List.assoc g.g_id tp.tp_graphs)
    with Not_found -> Misc.fatal_error "Static.eval_graph_decl" in
  let params = List.map (function { pm_desc=_,_,v } -> v) g.g_params in
  let boxes, wires = 
    begin match g.g_defn.gd_desc with
    | GD_Struct desc -> eval_struct_graph_desc g.g_id tp senv intf params desc
    | GD_Fun desc -> eval_fun_graph_desc tp senv intf params desc
    end in
  let g' = { tg_intf=intf; tg_impl={sg_boxes=boxes; sg_wires=wires} } in
  let acc' = (g.g_id, g') :: acc in
  let senv' = senv |> augment_env [g.g_id, SVNode (build_static_node g.g_id SV_Graph intf, [])] in
  acc', senv'
  
let eval_gval_decl senv { nd_desc=d; nd_loc=loc } = match d with
  | isrec, [b] ->  
     begin match eval_net_defns loc isrec senv [b] with
     | [(id,SVClos c) as v], [], [] -> v :: senv
     | _, _, _ -> illegal_global_value loc 
     end
  | _, _ -> illegal_global_value loc 
    
let eval_node_decl tp (senv,nodes) n =
  let id = n.n_intf.ni_desc.n_id in
  let intf =
    try List.assoc id tp.tp_nodes
    with Not_found -> Misc.fatal_error "Static.eval_impl_decl" in
  let params = List.map (function _ -> None) intf.t_params in
  let impl, kind = match n.n_impl.nm_desc with
    | NM_None -> NI_Actor [], SV_Actor
    | NM_Actor im -> NI_Actor im, SV_Actor
    | NM_Struct desc ->
       let boxes, wires = eval_struct_graph_desc id tp senv intf params desc in
       NI_Graph { sg_boxes=boxes; sg_wires=wires }, SV_Graph
    | NM_Fun desc ->
       let boxes, wires = eval_fun_graph_desc tp senv intf params desc in
       NI_Graph { sg_boxes=boxes; sg_wires=wires }, SV_Graph in
  let node = {
    sn_intf = intf;
    sn_impl = impl; } in
  (id, SVNode (build_static_node id kind intf, [])) :: senv,
  (id,node)::nodes

(* let extract_subgraph acc (id,n) = 
 *   match n.sn_impl with
 *     | NI_Actor _ -> acc
 *     | NI_Graph g -> (id,g)::acc *)
  
let build_static tp senv p =
  let senv_v = List.fold_left eval_gval_decl senv p.gvals in
  let senv_n, nodes = List.fold_left (eval_node_decl tp) (senv_v,[]) p.nodes in
  (* let subgraphs = List.fold_left extract_subgraph [] nodes in *)
  let topgraphs, senv_g =
    List.fold_left 
      (eval_graph_decl tp nodes)
      ([], senv_n)
      p.graphs in
  { ir_values = senv_v;
    ir_nodes = nodes;
    ir_graphs = (*subgraphs @*) topgraphs }

(* let extract_special_boxes name sp =
 *   List.fold_left
 *     (fun acc (id,b) -> if b.b_name = name then (id,b)::acc else acc)
 *     []
 *     sp.boxes
 * 
 * let extract_bcast_boxes sp = extract_special_boxes cfg.bcast_name sp
 * let extract_fifo_boxes sp = extract_special_boxes cfg.fifo_name sp *)

let dump_static_environment senv = 
  let dump_static_value (id,v) =
    Printf.printf "value %s = %s\n" id (string_of_semval v) in 
  Printf.printf "Static environment ---------------\n";
  List.iter dump_static_value senv;
  Printf.printf "----------------------------------\n"

