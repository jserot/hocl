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
open Semval

let type_arithm = trivial_scheme
  (type_arrow (type_pair type_int type_int) type_int)
and type_compar = trivial_scheme
  (type_arrow (type_pair type_int type_int) type_bool)

let encode_int n =
    SVInt n
let rec decode_int = function
  | SVInt n -> n
  | _ -> fatal_error "Builtins.decode_int" (* should not happen *)
let encode_bool b =
    SVBool b
let rec decode_bool = function
  | SVBool b -> b
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
    "+",  (type_arithm, prim2 encode_int  ( + ) decode_int decode_int);
    "-",  (type_arithm, prim2 encode_int  ( - ) decode_int decode_int);
    "*",  (type_arithm, prim2 encode_int  ( * ) decode_int decode_int);
    "/",  (type_arithm, prim2 encode_int  ( / ) decode_int decode_int);
    "%",  (type_arithm, prim2 encode_int  ( mod ) decode_int decode_int);
    "=",  (type_compar, prim2 encode_bool ( = ) decode_int decode_int);
    "!=",  (type_compar, prim2 encode_bool ( <> ) decode_int decode_int);
    "<",  (type_compar, prim2 encode_bool ( < ) decode_int decode_int);
    ">",  (type_compar, prim2 encode_bool ( > ) decode_int decode_int);
    "not",  (trivial_scheme (type_arrow type_bool type_bool), prim1 encode_bool ( not ) decode_bool);
  ]

(* Initial typing environment *)

let builtin_typing_env = {
    te_types = [  (* type constructors (name, arity) *)
      "int", 0;
      "bool", 0;
      "unit", 0;
      "bundle", 1
    ];
    te_values =
      List.map (fun (id,(ty,_)) -> id, ty) builtin_primitives
  }

(* Initial static environment *)

let builtin_static_env =
  List.map (function (n,(t,v)) -> n,v) builtin_primitives
  (* Values such as [map], [pipe], ... will be defined explicitely in the prelude file *)
