open Syntax
open Semval
open Error
open Eval

(* Evaluation of structural graph descriptions *)

(* Aux *)

type sbox_desc = 
  (* This is a variant of [Syntax.box_desc] in which parameters and IOs have been named 
     according to the refered node declaration *)
  { sb_node: string; (* Name of the instanciated model *)
    sb_params: (string * expr * Types.typ * Syntax.io_annot list) list;
    sb_ins: (string * string * Types.typ * Syntax.io_annot list) list;
    sb_outs: (string * string * Types.typ * Syntax.io_annot list) list }
   
let name_box_ios env b =
  let name f ns vs =
    try List.map2 f ns vs
    with Invalid_argument _ -> Misc.fatal_error "StaticStruct.name_box_ios: node mismatch" in
  let n = get_node_desc env b.bx_node in
  { sb_node = b.bx_node;
    sb_params = name (fun (n,_,ty,anns) e -> (n,e,ty,anns)) n.sn_params b.bx_params;
    sb_ins = name (fun (n,ty,anns) v -> n,v,ty,anns) n.sn_ins b.bx_ins;
    sb_outs = name (fun (n,ty,anns) v -> n,v,ty,anns) n.sn_outs b.bx_outs; }

(* |- WireDecl => E, W *)

let eval_wire_decl { wr_desc=(id,t) } =
  let wid = new_wid () in
  let ty = Types.type_wire t.te_typ in
  let w = new_wire ty in
  (id,SVWire wid),
  (wid,w)
  
(* E, B, W |-> BoxParam => B', W', M *)

let eval_box_param loc env dst sel' (boxes,wires,bindings) (slot,expr,ty,anns) =
  let rec extract_param_deps expr = match expr.e_desc with
    (* Scan a parameter expression and returns a list of set of boxes pointing to the
       input parameters occuring in it *)
    (* TO FIX ? Mutualize this with [StaticFun.eval_param_expr] *)
    | EInt _ ->
       []
    | EBool _ ->
       []
    | EVar v -> 
         begin match lookup env expr.e_loc v with
         | SVLoc l -> [l] (* [v] denotes an input of the enclosing graph *)
         | _ -> illegal_param_expr expr.e_loc
         end
    | EBinop (_,e1,e2) ->
       extract_param_deps e1 @ extract_param_deps e2
    | _ ->
       illegal_param_expr expr.e_loc in
  let mk_wire dst sel' (boxes,wires) (src,sel,ty) =
     let k = new_wid () in
     let w = ((src,sel,ty),(dst,sel',ty)) in (* Make a new wire starting at the src box ... *)
     let b' = add_box_output boxes src sel k in      
     Misc.replace_assoc boxes src b',          (* ... and add it to its outputs *)
     (k,w)::wires in
  match expr.e_desc, extract_param_deps expr with
  | EVar v, [src,sel,ty] ->
     let boxes', wires' = mk_wire dst sel' (boxes,wires) (src,sel,ty) in
     let (k,w) = List.hd wires' in
     boxes',
     wires',
     bindings@[slot,k,ty,anns]
  | EInt _, deps 
  | EBool _, deps 
  | EBinop _, deps ->
     let k = new_wid () in
     let l = new_bid () in
     let ty = expr.e_typ in
     let w = ((l,0,ty),(dst,sel',ty)) in
     let boxes', wires' = Misc.fold_lefti (mk_wire l) (boxes,wires) deps in
     let bins = List.mapi (fun i (k,(l,((d,ds,ty) as l'))) -> "i" ^ string_of_int i, k, ty, []) wires' in
     let b = new_box l (string_of_expr expr) LocalParamB bins ["o",[k],expr.e_typ,[]] no_bval in
     (l,b)::boxes',
     (k,w)::wires',
     bindings@[slot,k,ty,anns]
  | _, _ ->
     Misc.fatal_error "StaticStruct.eval_box_param"

(* E, W |-> BoxParams => B, M *)

let eval_box_params loc env boxes wires dst params =
  Misc.fold_lefti (eval_box_param loc env dst) (boxes,wires,[]) params

(* E, B, W |-> BoxInp => B', W', M *)

let eval_box_input loc env dst offset sel (boxes,wires,bindings) (slot,id,ty,anns) =
  let sel = sel + offset in  (* If the box has [p] parameters, data inputs starts at index [p] *)
  match lookup env loc id with
  | SVLoc (src,sel',_) ->
     let k = new_wid () in
     let w = ((src,sel',ty),(dst,sel,ty)) in
     let b' = add_box_output boxes src sel' k in
     Misc.replace_assoc boxes src b',
     (k,w)::wires,
     bindings@[slot,k,ty,anns]
  | SVWire k ->
     let w =
       begin
         match lookup_wire k wires with
         | (src,(-1,-1,ty)) -> (src, (dst,sel,ty))
         | _ -> Misc.fatal_error "StaticStruct.eval_box_input"
       end  in
     boxes,
     Misc.replace_assoc wires k w,
     bindings@[slot,k,ty,anns]
  | _ ->
     Misc.fatal_error "StaticStruct.eval_box_input"

(* E, W |-> BoxInps => B, M *)

let eval_box_inputs loc env boxes wires dst offset inps =
  Misc.fold_lefti (eval_box_input loc env dst offset) (boxes,wires,[]) inps

(* E, W |-> BoxOutp => B, M *)

let eval_box_output loc env src sel (boxes,wires,bindings) (slot,id,ty,anns) =
  match lookup env loc id with
  | SVLoc (dst,sel',_) ->
     let k = new_wid () in
     let w = ((src,sel,ty),(dst,sel',ty)) in
     let b' = set_box_input boxes dst sel' k in
     Misc.replace_assoc boxes dst b',
     (k,w)::wires,
     (id,[k],ty,anns)::bindings
  | SVWire k ->
     let w =
       begin
         match lookup_wire k wires with
         | ((-1,-1,ty),dst) -> ((src,sel,ty),dst)
         | _ -> Misc.fatal_error "StaticStruct.eval_box_output"
       end  in
     boxes,
     Misc.replace_assoc wires k w,
     bindings@[slot,[k],ty,anns] (* Keep ordering ! *)
  | _ ->
     Misc.fatal_error "StaticStruct.eval_box_output"

(* E, W |-> BoxOutps => B, M *)

let eval_box_outputs loc env boxes wires src outps =
  Misc.fold_lefti (eval_box_output loc env src) (boxes,wires,[]) outps
  
(* E, W |-> BoxDecl => B, W' *)

let eval_box_decl env (boxes,wires) {bx_desc=(id,b'); bx_loc=loc} =
  let b = name_box_ios env b' in
  match lookup env loc b.sb_node with
  | SVNode n ->
     let l = new_bid () in
     let boxes_p, wires_p, bparams = eval_box_params loc env boxes wires l b.sb_params in
     let offset = List.length b.sb_params in
     let boxes_i, wires_i, bins = eval_box_inputs loc env boxes_p wires_p l offset b.sb_ins in
     let boxes_o, wires_o, bouts = eval_box_outputs loc env boxes_i wires_i l b.sb_outs in
     let tag = tag_of_kind n.sn_kind in
     let b = new_box l n.sn_id tag (bparams@bins) bouts no_bval in
     (l,b)::boxes_o,
     wires_o
  | _ ->
     Misc.fatal_error "Eval_struct.eval_box_decl"

(* E, W |-> BoxDecls => B, W' *)

let eval_box_decls env boxes wires box_decls =
  List.fold_left (eval_box_decl env) (boxes,wires) box_decls
  
(* E, B |- StructGraphDefn => B', W *)

let eval_struct_graph_desc (env,boxes) g =
  let env_w, wires_w = g.gs_wires |> List.map eval_wire_decl |> List.split in
  let boxes', wires = g.gs_boxes |> eval_box_decls (env_w @ env) boxes wires_w in
  boxes', wires
