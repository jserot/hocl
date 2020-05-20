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

(* Definitions shared by the functional and structural evaluators *)

open Syntax
open Semval
open Error

(* Environment manipulation *)
   
let lookup env loc id =
  try List.assoc id env
  with Not_found -> unbound_value id loc

let (++) l1 l2 = l2 @ l1  (* \oplus operator on environments *)

let (+++) bb1 bb2 = (* Specialized box merging operation *)
  let merge_inp (id1,wid1,ty1,ann1) (id2,wid2,ty2,ann2) = (id1, max wid1 wid2, ty1, ann1) in (* Replace undefined wids (0) *)
  let merge_outp (id1,wids1,ty1,ann1) (id2,wids2,ty2,ann2) = (id1, Misc.list_merge wids1 wids2, ty1, ann1) in
  let merge_bval v1 v2 = match v1.bv_val, v2.bv_val with
    | SVUnit, SVUnit -> v1
    | _, SVUnit -> v1
    | SVUnit, _ -> v2
    | bv1, bv2 when bv1=bv2 -> v1
    | _, _ -> Misc.fatal_error "Eval.merge_box_env" in
  Misc.assoc_merge
    (fun b1 b2 ->
      { b_id = b1.b_id;
        b_tag = b1.b_tag;
        b_model = b1.b_model;
        b_ins = Array.map2 merge_inp b1.b_ins b2.b_ins;
        b_outs = Array.map2 merge_outp b1.b_outs b2.b_outs;
        b_val = merge_bval b1.b_val b2.b_val })
    bb1 bb2
        
(* Aux *)

let new_bid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wire ty = (-1,-1,ty), (-1,-1,ty)

let new_box bid model tag ins outs v =
  { b_id=bid; b_tag=tag; b_model=model; b_ins=Array.of_list ins; b_outs=Array.of_list outs; b_val=v }

let lookup_box bid boxes =
  try List.assoc bid boxes
  with Not_found -> Misc.fatal_error "Eval.lookup_box"

let lookup_wire wid wires =
  try List.assoc wid wires
  with Not_found -> Misc.fatal_error "Eval.lookup_wire"
  
let add_box_output boxes bid sel wid = 
    (* Add wire [wid] to the list of wires connected to the [sel+1]th output of the input box *)
    let b = lookup_box bid boxes in 
    { b with b_outs = b.b_outs |> Misc.array_replace (fun (id,wids,ty,anns) -> id,wid::wids,ty,anns) sel }

let set_box_input boxes bid sel wid = 
    (* Set wire connected to the [sel+1]th input of the output box to [wid] *)
    let b = lookup_box bid boxes in 
    { b with b_ins = b.b_ins |> Misc.array_replace (fun (id,_,ty,anns) -> id,wid,ty,anns) sel }

let tag_of_kind = function
  | ActorN -> ActorB
  | GraphN -> GraphB

let get_located_box env boxes id = 
  match List.assoc_opt id env with
  | Some (SVLoc (d,_,_) as l) ->
     Some (l, List.assoc d boxes)
  | _ -> None

let get_node_desc env id = 
  match List.assoc_opt id env with
  | Some (SVNode n) -> n
  | _ -> Misc.fatal_error "staticBase: get_node_desc"

let list_of_cons v =
  let rec h = function
    | SVCons (v1,v2) -> v1 :: h v2
    | SVNil -> []
    | _ -> Misc.fatal_error "Eval.list_of_cons" in
  SVList (h v)

let cons_of_list v =
  let rec h = function
  | [] -> SVNil
  | v::vs -> SVCons (v, h vs) in
  match v with
  | SVList l -> h l
  | _ -> Misc.fatal_error "Eval.cons_of_list"

let rec static_value env expr =
  let is_static_const = function SVInt _ | SVBool _ -> true | _ -> false in
  match expr.e_desc with
  | EVar v ->
     let sv = lookup env expr.e_loc v in
     if is_static_const sv
     then sv
     else SVUnit 
  | EInt n -> SVInt n
  | EBool v -> SVBool v
  | EBinop (op, e1,e2) ->
     begin match lookup env expr.e_loc op with
     | SVPrim f ->
        let v1 = static_value env e1 in
        let v2 = static_value env e2 in
        if is_static_const v1 && is_static_const v2
        then f (SVTuple [v1;v2])
        else SVUnit 
     | _ ->
        SVUnit
     end
  | _ ->
     SVUnit

exception Type_of_semval
        
let type_of_semval v =
  let open Types in
  let rec type_of v = match v with
  | SVInt v -> type_int
  | SVBool v -> type_bool
  | SVUnit -> type_unit
  | SVNil -> type_list (new_type_var ())
  | SVCons (v1,v2) -> type_list (type_of v1)
  | SVLoc (l,s,ty) -> real_type ty
  | SVTuple vs -> type_product (List.map type_of vs)
  | SVList [] -> type_list (new_type_var ())
  | SVList (v::_) -> type_list (type_of v)
  | SVClos cl -> let l,p,ty = cl.cl_pat in type_arrow ~lbl:l ty cl.cl_exp.e_typ
  | SVQuote e -> type_param e.e_typ
  | _ -> raise Type_of_semval in
  type_of v

let type_of_node n = 
  let open Types in
  let type_of = function [] -> type_unit | [t] -> t | ts -> TyProduct ts in 
  let ty_ins = n.sn_ins |> List.map (fun (_,ty,_,_) -> ty) |> type_of in
  let ty_outs = n.sn_outs |> List.map (fun (_,ty,_) -> ty) |> type_of in
  type_arrow ty_ins ty_outs

let type_of_node_args n = 
  let open Types in
  let type_of = function [] -> type_unit | [t] -> t | ts -> TyProduct ts in 
  let ty_ins = n.sn_ins |> List.map (fun (_,ty,_,_) -> ty) |> type_of in
  ty_ins

let eval_param_const ty e v = 
    let bid = new_bid () in
    let ty' = Types.type_param ty in
    let e = let open Syntax in { e_desc = e; e_loc = no_loc; e_typ = ty' } in
    let bv = { bv_lit = e; bv_sub=e; bv_val = v } in
    let name = "p" ^ string_of_int bid in
    let b = new_box bid name LocalParamB [] ["o",[],ty',[]] bv in
    (bid,0,ty'), (* src location *)
    [bid,b],     (* created box env *)
    []           (* created wire env *)

let rec eval_param_expr env boxes e =
  let get_loc v =
    match lookup env e.e_loc v with
     | SVLoc ((src,_,_) as l) -> 
        let b = List.assoc src boxes in
        if b.b_tag=InParamB then Some l else None  (* TO BE RELAXED ? *)
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
  match e.e_desc with
  | EInt n -> eval_param_const e.e_typ (EInt n) (SVInt n)
  | EBool n -> eval_param_const e.e_typ (EBool n) (SVBool n)
  | EVar v -> 
     begin
       match get_loc v with
       | Some l -> l, [], []
       | None -> illegal_param_expr e.e_loc
     end
  | EBinop (op,e1,e2) ->
     let slocs = extract_deps e in
     let bid = new_bid () in
     let ws = List.mapi (fun i (_,((_,_,ty) as l)) -> new_wid (), (l,(bid,i,ty))) slocs in
     let bins = List.mapi (fun i (wid,(l,((_,_,ty) as l'))) -> "i" ^ string_of_int (i+1), wid, ty, []) ws in
     let name = "p" ^ string_of_int bid in
     let bv = { bv_lit=e; bv_sub=subst_param_deps slocs e; bv_val=static_value env e } in 
     let ty' = Types.type_param e.e_typ in
     let b = new_box bid name LocalParamB bins ["o",[],ty',[]] bv in
     let bs' = List.map2 (fun (_,(l,s,_)) (wid,_) -> l, add_box_output boxes l s wid) slocs ws in
     (bid,0,ty'),
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

(* |- NodeParam => E', B *)

let rec eval_static_const e = match e.e_desc with
  | EInt n -> SVInt n
  | EBool n -> SVBool n
  | EQuote e -> eval_static_const e
  | _ -> Misc.fatal_error "Eval.eval_static_const"

let no_bval =
  let no_expr = {e_desc=EVar ""; e_loc=Location.no_location; e_typ=Types.no_type } in
  { bv_lit=no_expr; bv_sub=no_expr; bv_val=SVUnit }
       
(* |- NodeInput => E', B *)

let eval_node_input {io_desc=(id,t,e,anns)} =
  (* Note: attached annots are systematically ignored here *)
  let l = new_bid () in
  (* let ty = Types.type_wire t.te_typ in  *)
  let ty = t.te_typ in 
  let b = 
    if Types.is_param_type ty then
      let bv = match e with
        | None -> no_bval
        | Some e' -> { bv_lit=e'; bv_sub=e'; bv_val=eval_static_const e' } in
      new_box l id InParamB [] ["o",[],ty,[]] bv
  else
    new_box l id SourceB [] ["o",[],ty,[]] no_bval in
  (id, SVLoc (l,0,ty)),
  (l, b)

(* |- NodeInputs => E', B *)

let eval_node_inputs inps =
  inps |> List.map eval_node_input |> List.split

(* |- NodeOutput => E', B *)

let eval_node_output {io_desc=(id,t,e,anns)} =
  (* Note: attached expression and annots are systematically ignored here *)
  let l = new_bid () in
  (* let ty = Types.type_wire t.te_typ in *)
  let ty = t.te_typ in
  let b = new_box l id SinkB ["i",-1,ty,[]] [] no_bval in
  (id, SVLoc (l,0,ty)),
  (l, b)

(* |- NodeOutputs => E', B *)

let eval_node_outputs outps =
  outps |> List.map eval_node_output |> List.split
           
(* |- NodeIntf => E', B' *)

let eval_node_intf intf =
  let env_i, boxes_i = eval_node_inputs intf.n_ins in
  let env_o, boxes_o = eval_node_outputs intf.n_outs in
  env_i ++ env_o,
  boxes_i ++ boxes_o
