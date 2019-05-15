(* Static semantic domain *)

open Types
open Location

type ss_val =
  | SVNat of int
  | SVBool of bool
  | SVUnit
  | SVTuple of ss_val list
  | SVClos of sv_clos
  | SVPrim of (ss_val -> ss_val)
  | SVCons of ss_val * ss_val
  | SVList of ss_val list
  | SVNil
  | SVAct of sv_act
  | SVLoc of idx * sel * typ * bool (* node index, output selector, type, is_output *)

and sv_clos =
  { cl_pat: Syntax.net_pattern;
    cl_exp: Syntax.net_expr;
    mutable cl_env: (string * ss_val) list }

and idx = int
and sel = int

and sv_loc = idx * sel

and sv_act = {
    sa_id: string;
    sa_params: (string * typ * ss_val option) list;
    sa_ins: (string * typ) list;
    sa_outs: (string * typ) list;
    sa_typ: typ_scheme;
    (* sa_states: (string * typ) list; *)
}

let is_static_const = function
    SVNat _ | SVBool _ | SVUnit | SVNil -> true
  | _ -> false

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

let rec size_of_ssval v = match v with
  | SVNil -> 0
  | SVCons (_,v) -> 1 + size_of_ssval v
  | SVTuple vs -> List.length vs
  | _ -> 1

(* Printing *)

let rec output_ss_value oc v = output_string oc (string_of_ssval v)

and  string_of_ssval v = match v with
  | SVNat v -> string_of_int v
  | SVBool v -> string_of_bool v
  | SVUnit -> "()"
  | SVNil -> "[]"
  | SVCons (v1,v2) -> string_of_ssval v1 ^ "::" ^ string_of_ssval v2
  | SVLoc (l,s,ty,_) -> "Loc(" ^ (string_of_int l) ^ "," ^ (string_of_int s) ^ ")"
  | SVPrim p -> "Prim(...)"
  | SVAct a -> "Act(...)"
  | SVClos _ -> "Clos(...)"
  | SVTuple vs -> "(" ^ Misc.string_of_list string_of_ssval "," vs ^ ")"
  | SVList vs -> "[" ^ Misc.string_of_list string_of_ssval "," vs ^ "]"

and output_ss_val_list oc sep l = Misc.output_list output_value oc sep l
