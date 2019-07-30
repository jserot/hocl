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

open Types
open Misc
open Typing
open Ssval

let type_arithm = trivial_scheme
  (type_arrow (type_pair type_nat type_nat) type_nat)
and type_compar = trivial_scheme
  (type_arrow (type_pair type_nat type_nat) type_bool)

let encode_nat n =
    SVNat n
let rec decode_nat = function
  | SVNat n -> n
  | SVLoc (_,_,_,SVNat n) -> n (* Special case for parameters *)
  | _ -> fatal_error "Builtins.decode_nat" (* should not happen *)
let encode_bool b =
    SVBool b
let rec decode_bool = function
  | SVBool b -> b
  | SVLoc (_,_,_,SVBool b) -> b (* Special case for parameters *)
  | _ -> fatal_error "Builtins.decode bool" (* should not happen *)

let prim1 encode op decode =
  SVPrim (function v -> encode (op (decode v)))
let prim2 encode op decode1 decode2 =
  SVPrim (function
   | SVTuple [v1;v2] ->
       encode (op (decode1 v1) (decode2 v2))
   | _ -> fatal_error "Builtins.prim2")

let builtin_primitives = [
    (* Id, type, static value *)
    "+",  (type_arithm, prim2 encode_nat  ( + ) decode_nat decode_nat);
    "-",  (type_arithm, prim2 encode_nat  ( - ) decode_nat decode_nat);
    "*",  (type_arithm, prim2 encode_nat  ( * ) decode_nat decode_nat);
    "/",  (type_arithm, prim2 encode_nat  ( / ) decode_nat decode_nat);
    "%",  (type_arithm, prim2 encode_nat  ( mod ) decode_nat decode_nat);
    "=",  (type_compar, prim2 encode_bool ( = ) decode_nat decode_nat);
    "!=",  (type_compar, prim2 encode_bool ( <> ) decode_nat decode_nat);
    "<",  (type_compar, prim2 encode_bool ( < ) decode_nat decode_nat);
    ">",  (type_compar, prim2 encode_bool ( > ) decode_nat decode_nat);
    "not",  (trivial_scheme (type_arrow type_bool type_bool), prim1 encode_bool ( not ) decode_bool);
  ]

(* Initial (builtin) typing environment *)

let builtin_typing_env = {
  te_types = [  (* type constructors (name, arity) *)
      "nat", 0;
      "bool", 0;
      "unit", 0;
      "bundle", 1
      ];
  te_values =
    List.map (fun (id,(ty,_)) -> id, ty) builtin_primitives
  }

let dump_type (name, arity) =
  Printf.printf "type %s (arity=%d)\n" name arity

and dump_value tag (name, ty_sch) =
  Pr_type.reset_type_var_names ();
  Printf.printf "%s %s : %s\n" tag name (Pr_type.string_of_type_scheme ty_sch);
  flush stdout
  
let dump_typing_environment () = 
  Printf.printf "Typing environment ---------------\n";
  List.iter dump_type builtin_typing_env.te_types;
  List.iter (dump_value "val") builtin_typing_env.te_values;
  Printf.printf "----------------------------------\n"

(* Initial (builtin) static environment *)

let builtin_static_env =List.map (function (n,(t,v)) -> n,v) builtin_primitives

and dump_static_value (name, ty_sch) =
  dump_value "prim" (name, List.assoc name builtin_typing_env.te_values)

let dump_static_environment () = 
  Printf.printf "Static environment ---------------\n";
  List.iter dump_static_value builtin_static_env;
  Printf.printf "----------------------------------\n"
