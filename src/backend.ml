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

(* Several utility fns common to all backends *)

open Interm

let get_impl_fns target name attrs =
  match List.assoc_opt "incl_file" attrs,
        List.assoc_opt "loop_fn" attrs,
        List.assoc_opt "init_fn" attrs
  with
  | Some f, Some f', Some f'' -> f, f', f'', List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | Some f, Some f', None -> f, f', "", List.mem_assoc "is_delay" attrs, List.mem_assoc "pexec" attrs
  | _, _, _ -> Error.incomplete_actor_impl target name

let get_node_desc ir id =
    try List.assoc id ir.ir_nodes
    with Not_found -> Misc.fatal_error "Backend.get_node_desc"

let get_actor_desc ir target id = 
  match get_node_desc ir id with
  | { sn_impl=NI_Actor impls } -> 
     begin
       try List.assoc target impls
       with Not_found -> Error.missing_actor_impl target id
     end
  | _ ->
     Misc.fatal_error "Backend.get_actor_desc"

let string_of_io_rate rate = match rate with
  | None -> "1"
  (* | Some e -> Syntax.string_of_rate_expr e *)
  | Some e -> e

let get_rate_annot anns = 
  List.assoc_opt "rate" anns
  (* List.find_opt (function Syntax.IA_Rate _ -> true | _ -> false) anns *)

let is_constant_rate_expr s =
  match int_of_string_opt s with
  | Some _ -> true
  | None -> false
