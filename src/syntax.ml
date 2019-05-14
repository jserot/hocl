(* Abstract syntax *)

open Location

(* Type expressions *)

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: location;
    mutable te_typ: Types.typ }

and type_expression_desc =
  | Typetuple of type_expression list
  | Typeconstr of string * type_expression list

(* Programs *)
                
type program =
  { types: type_decl list;
    actors: actor_decl list ;
    defns: net_defn list }

and type_decl =
  { td_desc: tdecl_desc;
    td_loc: location }
and tdecl_desc =
  | Opaque_type_decl of string                                   (* name *)

and actor_decl = 
  { ad_desc: actor_desc;
    ad_loc: location }

and actor_desc = {
    a_id: string;
    a_ins: type_expression list;
    a_outs: type_expression list;
  }

and net_defn =
  { nd_desc: net_defn_desc;
    nd_loc: location }

and net_defn_desc = bool * net_binding list

and net_binding =
  { nb_desc: net_binding_desc;
    nb_loc: location }

and net_binding_desc = net_pattern * net_expr 

and net_pattern =
  { np_desc: net_pattern_desc;
    np_loc: location;
    mutable np_typ: Types.typ }

and net_pattern_desc =
  | NPat_var of string (* string option *) (* name, initial value *)
  | NPat_tuple of net_pattern list
  | NPat_nil
  | NPat_cons of net_pattern * net_pattern
  | NPat_list of net_pattern list
  | NPat_unit
  | NPat_ignore

and net_expr =
  { ne_desc: net_expr_desc;
    ne_loc: location;
    mutable ne_typ: Types.typ }

and net_expr_desc =
   | NVar of string
   | NApp of net_expr * net_expr
   | NTuple of net_expr list
   | NLet of bool * net_binding list * net_expr (* rec / non rec*)
   | NFun of net_pattern * net_expr (* single match here ! *)
   | NNil
   | NCons of net_expr * net_expr
   | NList of net_expr list
   | NListElem of net_expr * net_expr
   | NIf of net_expr * net_expr * net_expr
   | NMatch of net_expr * net_binding list
   | NBool of bool
   | NNat of int
   | NUnit

(* Aux fns *)

(* let is_op p id = List.exists (function {op_desc=id',_,_,_,_} when id=id' -> true | _ -> false) p.ops *)
let is_fun_definition = function

  { nb_desc={np_desc=NPat_var _}, {ne_desc=NFun (_,_)} } -> true
| _ -> false

(* Program manipulation *)

let empty_program = { types=[]; actors=[]; defns=[]; }

let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
    types= p1.types @ p2.types;
    actors= p1.actors @ p2.actors;
    defns= p1.defns @ p2.defns;
  }

(* Printing *)

let is_unop op = List.mem op [ "not" ]      (* TO ADJUST WITH PARSER/LEXER *)

let is_binop op = List.mem op [             (* TO ADJUST WITH PARSER/LEXER *)
  "+"; "-"; "*"; "/"; "%";
  "<"; ">"; "="; "!="; "<>"
  ]
   
let rec string_of_ty_expr te = string_of_ty_exp te.te_desc

and string_of_ty_exp = function
  | Typeconstr (c, []) -> c
  | Typeconstr (c, [t]) -> string_of_ty_expr t ^ " " ^ c
  | Typeconstr (c, ts) -> "(" ^ Misc.string_of_list string_of_ty_expr "," ts ^ ") " ^ c
  | Typetuple ts -> "(" ^ Misc.string_of_list string_of_ty_expr "*" ts ^ ")"

let rec string_of_net_expr ne = string_of_net_exp ne.ne_desc

and string_of_net_exp = function
   | NVar v -> v
   | NApp (e1,e2) -> string_of_net_expr e1 ^ " " ^ string_of_net_expr e2
   | NTuple es -> "(" ^ Misc.string_of_list string_of_net_expr "," es ^ ")"
   | NLet (isrec,nbs,e) ->
         "let " ^ if isrec then "rec " else " " ^  Misc.string_of_list string_of_net_binding "and" nbs
       ^ " in " ^ string_of_net_expr e
   | NFun (p,e) -> "<fun>"
   | NNil -> "[]"
   | NCons (e1,e2) -> string_of_net_expr e1 ^ "::" ^ string_of_net_expr e2
   | NList es -> "[" ^ Misc.string_of_list string_of_net_expr "," es ^ "]"
   | NListElem (e1,e2) -> string_of_net_expr e1 ^ "[" ^ string_of_net_expr e2 ^ "]"
   | NIf (e1,e2,e3) -> "if " ^ string_of_net_expr e1 ^ " then " ^ string_of_net_expr e2 ^ " else " ^ string_of_net_expr e3
   | NMatch (e1,bs) -> "match " ^ string_of_net_expr e1 ^ " with " ^ Misc.string_of_list string_of_net_binding " | " bs
   | NBool b -> string_of_bool b
   | NNat n -> string_of_int n
   | NUnit -> "()"

and string_of_net_binding nb = string_of_net_bind nb.nb_desc

and string_of_net_bind (np,ne) = string_of_net_pattern np ^ "=" ^ string_of_net_expr ne

and string_of_net_pattern np = string_of_net_pat np.np_desc

and string_of_net_pat = function
    NPat_var v -> v
  | NPat_tuple ps -> "(" ^ Misc.string_of_list string_of_net_pattern "," ps ^ ")"
  | NPat_nil -> "[]"
  | NPat_cons (p1,p2) -> string_of_net_pattern p1 ^ "::" ^ string_of_net_pattern p2
  | NPat_list ps -> "[" ^ Misc.string_of_list string_of_net_pattern "," ps ^ "]"
  | NPat_unit -> "()"
  | NPat_ignore -> "_"

let string_of_actor_decl d =
  let a = d.ad_desc in
  a.a_id ^ ": "
  ^ Misc.string_of_list string_of_ty_expr " * " a.a_ins ^ " -> "
  ^ Misc.string_of_list string_of_ty_expr " * " a.a_outs 

let string_of_type_decl d = match d.td_desc with
  | Opaque_type_decl id -> id

let rec string_of_net_defn d = match d.nd_desc with
    r, bs -> string_of_rec r ^ Misc.string_of_list string_of_net_binding " and " bs

and string_of_rec = function true -> " rec " | false -> ""

let dump_type d = Printf.printf "type %s\n" (string_of_type_decl d)
let dump_actor d = Printf.printf "actor %s\n" (string_of_actor_decl d)
let dump_defn d = Printf.printf "net %s\n" (string_of_net_defn d)

let rec dump_program p =
  Printf.printf "Types ---------------\n";
  List.iter dump_type p.types;
  Printf.printf "Actors ---------------\n";
  List.iter dump_actor p.actors;
  Printf.printf "Actors ---------------\n";
  List.iter dump_defn p.defns
