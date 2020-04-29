open Misc
open Printf
open Pr_type
open Location

let no_loc = no_location
           
let illegal_application loc =
  eprintf "%aThis application is not a valid application.\n"
    output_location loc;
  raise Error

let unbound_value name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let binding_error loc =
  eprintf "%aThis kind of pattern binding is not accepted.\n"
    output_location loc;
  raise Error

let unbound_type name loc =
  eprintf "%aThe type identifier %a is unbound.\n" 
    output_location loc
    output_string name;
  raise Error

let wrong_type_err site ty1 ty2 loc =
  eprintf "%aAn error occured when typing this %s : types %s and %s cannot be unified.\n"
    output_location loc
    site
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

let illegal_rec_definition loc =
  eprintf "%aThis kind of pattern is not accepted as left-hand side of recursive definitions.\n"
    output_location loc;
  raise Error

let illegal_param_expr loc =
  eprintf "%a** Error: invalid expression for parameter value.\n." output_location loc;
  raise Error

let invalid_list_index idx loc =
  eprintf "%a** Error: invalid list index (%d).\n." output_location loc idx;
  raise Error

let matching_failure loc =
  eprintf "Match failure\n";
  raise Error

let not_implemented what = 
  eprintf "Not implemented: %s.\n" what;
  raise Error

let missing_actor_impl target id =
  eprintf "Error: no %s implementation found for actor %s\n" target id;
  raise Error

let incomplete_actor_impl target id =
  eprintf "Error: incomplete %s implementation given for actor %s (should at least give incl_file and loop_fn)\n" target id;
  raise Error

let missing_ival_param id =
  eprintf "Error: no parameter named \"ival\" for actor %s; assuming last one\n" id;
  raise Error

let illegal_interface what name msg =
  eprintf "Error: invalid interface for %s %s%s.\n" what name msg;
  raise Error
