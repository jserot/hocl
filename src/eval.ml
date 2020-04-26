open Syntax
open Semval
open Error

(* Definitions shared by the functional and structural evaluators *)

(* Environment manipulation *)
   
let lookup env loc id =
  try List.assoc id env
  with Not_found -> unbound_value id loc

let (++) l1 l2 = l2 @ l1  (* \oplus operator on environments *)

let (+++) bb1 bb2 = (* Specialized box merging operation *)
  let merge_inp (id1,wid1,ty1,ann1) (id2,wid2,ty2,ann2) = (id1, max wid1 wid2, ty1, ann1) in (* Replace undefined wids (0) *)
  let merge_outp (id1,wids1,ty1,ann1) (id2,wids2,ty2,ann2) = (id1, Misc.list_merge wids1 wids2, ty1, ann1) in
  let merge_bval v1 v2 = match v1, v2 with
    | SVUnit, SVUnit -> SVUnit
    | v1, SVUnit -> v1
    | SVUnit, v2 -> v2
    | v1, v2 when v1=v2 -> v1
    | v1, v2 -> Misc.fatal_error "Static.merge_box_env" in
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
  with Not_found -> Misc.fatal_error "Static.lookup_box"

let lookup_wire wid wires =
  try List.assoc wid wires
  with Not_found -> Misc.fatal_error "Static.lookup_wire"
  
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
    | _ -> Misc.fatal_error "Static.list_of_cons" in
  SVList (h v)

let cons_of_list v =
  let rec h = function
  | [] -> SVNil
  | v::vs -> SVCons (v, h vs) in
  match v with
  | SVList l -> h l
  | _ -> Misc.fatal_error "Static.cons_of_list"

(* |- NodeParam => E', B *)

let eval_static_const e = match e.e_desc with
  | EInt n -> SVInt n
  | EBool n -> SVBool n
  | _ -> Misc.fatal_error "Static.eval_static_const"
       
let eval_param_value e =  match e with
    | None -> SVUnit
    | Some e' -> eval_static_const e'

let eval_node_param {pm_desc=(id,t,e,_)} =
  let l = new_bid () in
  let ty = t.te_typ in
  let b = new_box l id InParamB [] ["o",[],ty,[]] (eval_param_value e) in
  (id, SVLoc (l,0,ty)),
  (l, b)

(* |- NodeParams => E', B *)

let eval_node_params params =
  params |> List.map eval_node_param |> List.split

(* |- NodeInput => E', B *)

let eval_node_input {io_desc=(id,t,_)} =
  let l = new_bid () in
  let ty = Types.type_wire t.te_typ in 
  let b = new_box l id SourceB [] ["o",[],ty,[]] SVUnit in
  (id, SVLoc (l,0,ty)),
  (l, b)

(* |- NodeInputs => E', B *)

let eval_node_inputs inps =
  inps |> List.map eval_node_input |> List.split
(* |- NodeOutput => E', B *)

let eval_node_output {io_desc=(id,t,_)} =
  let l = new_bid () in
  let ty = Types.type_wire t.te_typ in
  let b = new_box l id SinkB ["i",-1,ty,[]] [] SVUnit in
  (id, SVLoc (l,0,ty)),
  (l, b)

(* |- NodeOutputs => E', B *)

let eval_node_outputs outps =
  outps |> List.map eval_node_output |> List.split
           
(* |- NodeIntf => E', B' *)

let eval_node_intf intf =
  let env_p, boxes_p = eval_node_params intf.n_params in
  let env_i, boxes_i = eval_node_inputs intf.n_ins in
  let env_o, boxes_o = eval_node_outputs intf.n_outs in
  env_p ++ env_i ++ env_o,
  boxes_p ++ boxes_i ++ boxes_o
