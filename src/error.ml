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

(* Printing of error messages and warnings *)

open Misc
open Location
open Syntax
open Printf
open Pr_type

let unbound_value_err name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let unbound_type_err name loc =
  eprintf "%aThe type identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let application_of_non_function_err exp =
  eprintf "%aThis expression is not a function, it cannot be applied.\n"
      output_location exp.ne_loc;
  raise Error

let illegal_global_value loc =
  eprintf "%aThis global value definition is illegal. Only functions can be declared.\n"
      output_location loc;
  raise Error

let illegal_application loc =
  eprintf "%aThis application is not a valid application.\n"
      output_location loc;
  raise Error

let illegal_node_instanciation loc =
  eprintf "%aThis node instanciation is not valid.\n"
      output_location loc;
  raise Error

let illegal_expression exp =
  eprintf "%aThis expression is not valid in this context.\n"
      output_location exp.ne_loc;
  raise Error

let illegal_letrec_expr loc =
  eprintf "%aThis kind of expression is not allowed in \
           right-hand sides of \"let rec\".\n"
    output_location loc;
  raise Error

let wrong_type_err site ty1 ty2 loc =
  eprintf "%aAn error occured when typing this %s : types %s and %s cannot be unified.\n"
    output_location loc
    site
    (string_of_type ty1)
    (string_of_type ty2);
  raise Error

let output_type_mismatch id ty1 ty2 =
  eprintf "An error occured when typing output %s : types %s and %s cannot be unified.\n"
    id
    (string_of_type ty1)
    (string_of_type ty2);
  raise Error

let circular_type_err site ty1 ty2 loc =
  eprintf "%aAn error occured when typing this %s : a cycle was detected between types %s and %s.\n"
    output_location loc
    site
    (string_of_type ty1)
    (string_of_type ty2);
  raise Error

let illegal_definition loc =
  eprintf "%aThis style of definition is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_rec_definition loc =
  eprintf "%aThis kind of pattern is not accepted as left-hand side of recursive definitions.\n"
    output_location loc;
  raise Error

let binding_error loc =
  eprintf "%aThis kind of pattern binding is not accepted.\n"
    output_location loc;
  raise Error

let matching_failure loc =
  eprintf "%aMatch failure\n"
    output_location loc;
  raise Error

let size_mismatch_error sz loc =
  eprintf "%aThis list expression should have size %d\n"
    output_location loc sz;
  raise Error

let unbound_output_warning id =
  eprintf "Warning: the output identifier %s is unbound\n" id

let unrecognized_output_fmt id =
  eprintf "Error: unrecognized output format : %s\n" id;
  raise Error

let unrecognized_edge_label_fmt id =
  eprintf "Error: unrecognized edge_label format : %s\n" id;
  raise Error

let undeclared_type_ctor tid loc =
  eprintf "%a** Error: undeclared type %s.\n." output_location loc tid;
  raise Error

let warning_undeclared_type tid loc =
  eprintf "%a** Warning: type %s has not been declared. Doing it for you...\n." 
    output_location loc tid

let type_ctor_mismatch tid loc =
  eprintf "%a** Error: the type constructor is used with a wrong number of arguments %s.\n." output_location loc tid;
  raise Error

let invalid_list_index idx loc =
  eprintf "%a** Error: invalid list index (%d).\n." output_location loc idx;
  raise Error

let invalid_param_value id loc =
  eprintf "%a** Error: invalid value for parameter %s.\n." output_location loc id;
  raise Error

let invalid_actor_param id loc =
  eprintf "%a** Error: invalid parameter value for actor %s.\n." output_location loc id;
  raise Error

let invalid_param_expr loc =
  eprintf "%a** Error: invalid expression for parameter value.\n." output_location loc;
  raise Error

let initial_value_mismatch v v' =
  eprintf "Error: initial values %s and %s mismatch\n" v v';
  raise Error

let invalid_box_wiring what name sel =
  eprintf "Error: %s %d for box %s.\n" what sel name;
  raise Error

let multiply_connected_wire gid wid  =
  eprintf "Error: the wire %s in graph %s is multiply connected.\n" wid gid;
  raise Error

let incomplete_wire gid wid  =
  eprintf "Error: the wire %s in graph %s is not fully connected.\n" wid gid;
  raise Error

let no_pragma_desc id =
  eprintf "Warning: no #pragma description found for actor %s\n" id

let missing_param what name msg =
  eprintf "Error: no parameter for %s %s%s.\n" what name msg;
  raise Error

let missing_ival_param id =
  eprintf "Warning: no parameter named \"ival\" for actor %s; assuming last one\n" id

let illegal_interface what name msg =
  eprintf "Error: invalid interface for %s %s%s.\n" what name msg;
  raise Error

