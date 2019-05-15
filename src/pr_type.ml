(* Printing a type expression *)

open Types
open Buffer

let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (typ * string) list);;

let reset_type_var_names () =
  type_vars_counter := 0; type_vars_names := [];;

let name_of_type_var sch var =
  try
    List.assq var !type_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    incr type_vars_counter;
    type_vars_names := (var, name) :: !type_vars_names;
    name

let rec output_typ b sch priority ty =
  match real_type ty with
    (TyVar {stamp=s}) as t ->
      add_string b "'";
      add_string b ((name_of_type_var sch t)) (* ^"_"^(string_of_int s)) *)
  | TyArrow(ty1, ty2) ->
      if priority >= 1 then add_string b "(";
      output_typ b sch 1 ty1;
      add_string b " -> ";
      output_typ b sch 0 ty2;
      if priority >= 1 then add_string b ")"
  | TyProduct ts ->
      if priority >= 2 then add_string b "(";
      output_typ_list b sch 2 "*" ts;
      if priority >= 2 then add_string b ")"
  | TyConstr (c, args) ->
      begin match args with
        []    -> ()
      | [t] ->
          output_typ b sch 2 t; add_string b " "
      | ts ->
          add_string b "(";
          output_typ_list b sch 0 ", " ts;
          add_string b ") "
      end;
      add_string b c

and output_typ_list b sch priority sep = function
    [] ->
      ()
  | [ty] ->
      output_typ b sch priority ty
  | ty::rest ->
      output_typ b sch priority ty;
      add_string b sep;
      output_typ_list b sch priority sep rest

let string_of_type ty =
  let b = Buffer.create 16 in
  output_typ b false 0 ty;
  Buffer.contents b

(* let string_of_one_type ty = *)
(*   reset_type_var_name(); *)
(*   let b = Buffer.create 16 in *)
(*   output_typ b false 0 ty; *)
(*   Buffer.contents b *)

let string_of_type_scheme sch =
  reset_type_var_names ();
  let b = Buffer.create 16 in
  output_typ b true 0 sch.ts_body;
  Buffer.contents b

