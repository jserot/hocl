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

(* The [Menhir] parser definition *)

%token <string> IDENT
(* %token <string> PREFIX *)
%token <string> INFIX0
(* %token <string> INFIX1 *)
%token <string> INFIX2
%token <string> INFIX3
%token <int> INT
(* %token <string> STRING *)
%token EOF
%token EQUAL          (* "="*)
%token NOTEQUAL       (* "!="*)
%token GREATER        (* ">" *)
%token LESS           (* "<" *)
%token LPAREN         (* "("*)
%token RPAREN         (* ")"*)
(* %token LBRACE         (\* "{"*\)
 * %token RBRACE         (\* "}"*\) *)
%token STAR           (* "*"*)
%token COMMA          (* ","*)
%token ARROW   (* "->"*)
%token COLONCOLON     (* "::"*)
%token SEMI           (* ";"*)
%token COLON          (* ":"*)
%token LBRACKET       (* "["*)
%token RBRACKET       (* "]"*)
%token BAR            (* "|"*)
%token UNDERSCORE     (* "_"*)
%token END            (* "end"*)
%token TYPE           (* "type"*)
%token TY_NAT         (* "nat"*)
%token TY_BOOL        (* "bool"*)
%token TY_UNIT        (* "unit"*)
(* %token BCAST          (\* "bcast"*\)
 * %token DELAY          (\* "delay"*\) *)
%token GRAPH          (* "graph"*)
%token ACTOR          (* "actor"*)
%token PARAM          (* "param"*)
%token IN             (* "in"*)
%token OUT            (* "out"*)
%token FUN            (* "fun" *)
%token STRUCT         (* "struct" *)
%token NODE           (* "node" *)
%token WIRE           (* "wire" *)
%token MATCH          (* "match"*)
%token WITH           (* "with"*)
%token VAL            (* "val"*)
%token LET            (* "let"*)
%token AND            (* "and"*)
%token REC            (* "rec"*)
(* %token LIST           (\* "list"*\) *)
%token IF             (* "if"*)
%token THEN           (* "then"*)
%token ELSE           (* "else"*)
%token TRUE           (* "true"*)
%token FALSE          (* "false"*)
(* %token INITIALLY      (\* "initially"*\) *)
(* %token PRAGMA         /* "#pragma" */ *)

(* Precedences and associativities. Lower precedences first.*)

(* %nonassoc IN *)
%right prec_let
(* %right prec_define *)
%right ARROW
(* %left  BAR *)
%left  COMMA
%left  INFIX0                           (* rev app operators (|>, >>) *)
%left  (*INFIX1*) GREATER LESS EQUAL NOTEQUAL    (* comparisons*)
%left  INFIX2                           (* additives, subtractives*)
%right COLONCOLON
%left  STAR INFIX3                      (* multiplicatives*)
(* %right prec_app *)
(* %right PREFIX                           (\* prefix operators, e.g. !*\) *)
(* %nonassoc prec_uminus  *)

(* Entry points*)

%start program
%type <Syntax.program> program

%{
open Location
open Syntax

let mk_location (p1,p2) =
  let open Lexing in
  Loc (!input_name, p1.pos_bol+p1.pos_cnum, p2.pos_bol+p2.pos_cnum)

let mk_type_expr l desc = { te_desc = desc; te_loc = mk_location l; te_typ = Types.no_type}
let mk_type_decl l desc = { td_desc = desc; td_loc = mk_location l }
let mk_param_decl l desc = { pm_desc = desc; pm_loc = mk_location l }
let mk_io_decl l desc = { io_desc = desc; io_loc = mk_location l }
let mk_actor_decl l desc = { ad_desc = desc; ad_loc = mk_location l }
let mk_graph_decl l desc = { g_desc = desc; g_loc = mk_location l }
let mk_graph_defn l desc = { gd_desc = desc; gd_loc = mk_location l }
let mk_wire_decl l desc = { gw_desc = desc; gw_loc = mk_location l }
let mk_node_decl l desc = { gn_desc = desc; gn_loc = mk_location l }
let mk_net_defn l desc = { nd_desc = desc; nd_loc = mk_location l }
let mk_net_expr l desc = { ne_desc = desc; ne_loc = mk_location l; ne_typ = Types.no_type }
let mk_net_pat l desc = { np_desc = desc; np_loc = mk_location l; np_typ = Types.no_type }
let mk_net_binding l desc = { nb_desc = desc; nb_loc = mk_location l}
let mk_core_expr l desc = { ce_desc = desc; ce_loc = mk_location l; ce_typ = Types.no_type }
let rec mk_apply l f es = match es with
  | [] -> f
  | e2::e2s -> mk_apply l (mk_net_expr l (NApp(f, e2))) e2s (* TO FIX: location *)
let rec mk_apply2 l f ps es = match es with
  | [] -> f
  | e2::e2s -> mk_apply l (mk_net_expr l (NApp2(f, ps, e2))) e2s (* TO FIX: location *)
let rec mk_fun l pats e = match pats with
  | [] -> Misc.fatal_error "mk_fun" (* should not happen *)
  | [p] -> mk_net_expr l (NFun (p,e))
  | p::ps -> mk_net_expr l (NFun (p, mk_fun l ps e)) (* TO FIX: location *)
let mk_binop l (op,l') e1 e2 = mk_net_expr l (NApp (mk_net_expr l' (NVar op), mk_net_expr l (NTuple [e1;e2])))
(* let mk_unop l (op,l') e = mk_net_expr l (NApp (mk_net_expr l' (NVar op),e)) *)
(* let mk_uminus l l' e = match e.ne_desc with
 *   | NInt n -> { e with ne_desc = NInt (-n) }
 *   | _ -> mk_unop l ("~-",l') e *)
let mk_infix l (op,l') e1 e2 = mk_apply l (mk_net_expr l' (NVar op)) [e1; e2]
let mk_cbinop l (op,l') e1 e2 = mk_core_expr l (EBinop (op,  e1, e2))
(* let mk_unop l (op,l') e = mk_net_expr l (NApp (mk_net_expr l' (NVar op),e)) *)
(* let mk_uminus l l' e = match e.ne_desc with
 *   | NInt n -> { e with ne_desc = NInt (-n) }
 *   | _ -> mk_unop l ("~-",l') e *)

type top_decl =
  | TyDecl of Syntax.type_decl
  | ActorDecl of Syntax.actor_decl
  | GraphDecl of Syntax.graph_decl

let is_type_decl = function TyDecl _ -> true | _ -> false             
let is_actor_decl = function ActorDecl _ -> true | _ -> false             
let is_graph_decl = function GraphDecl _ -> true | _ -> false             

let type_decl_of = function TyDecl d -> d | _ -> Misc.fatal_error "Parser.type_decl_of"             
let actor_decl_of = function ActorDecl d -> d | _ -> Misc.fatal_error "Parser.actor_decl_of"             
let graph_decl_of = function GraphDecl d -> d | _ -> Misc.fatal_error "Parser.graph_decl_of"             

type struct_decl =
  | WireDecl of Syntax.wire_decl
  | NodeDecl of Syntax.node_decl

let is_wire_decl = function WireDecl _ -> true | _ -> false             
let is_node_decl = function NodeDecl _ -> true | _ -> false             

let wire_decl_of = function WireDecl d -> d | _ -> Misc.fatal_error "Parser.wire_decl_of"             
let node_decl_of = function NodeDecl d -> d | _ -> Misc.fatal_error "Parser.node_decl_of"             
%}

%%

%public optional(X):
   (* Nothing *) { false }
  | X { true }

%public my_list(X):
  (* nothing *)
    { [] }
| x = X; xs = my_list(X)
    { x :: xs }

%public my_nonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = my_nonempty_list(X)
    { x :: xs }

%public my_separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = my_separated_nonempty_list(separator, X)
    { x :: xs }

%public my_separated_list(separator, X):
  (* nothing *)
    { [] }
| x = my_separated_nonempty_list(separator, X)
    { x }

(* PROGRAM *)

program:
  | decls = my_list(decl) EOF
      { { Syntax.types = decls |> List.filter is_type_decl |> List.map type_decl_of;
          Syntax.actors = decls |> List.filter is_actor_decl |> List.map actor_decl_of;
          Syntax.graphs = decls |> List.filter is_graph_decl |> List.map graph_decl_of } }

(* DECLARATIONS *)

decl:
    d = type_decl SEMI { TyDecl d }
  | d = actor_decl SEMI { ActorDecl d }
  | d = graph_decl SEMI { GraphDecl d }
          
(* TYPE DECLARATION *)

type_decl:
  | TYPE id=IDENT 
      { mk_type_decl $loc (Syntax.Opaque_type_decl id) }

(* ACTOR DECLARATION *)

actor_decl:
   ACTOR id=IDENT params=opt_params IN inps=io_decls OUT outps=io_decls 
    { mk_actor_decl $loc { a_id=id; a_params=params; a_ins=inps; a_outs=outps } }

opt_params:
  | (* Nothing *) { [] }
  | PARAM LPAREN ps=my_separated_list(COMMA, param_decl) RPAREN { ps }

param_decl:
   id=IDENT COLON t=simple_type_expr
    { mk_param_decl $loc (id, t) }

io_decls:
  | LPAREN ios=my_separated_list(COMMA, io_decl) RPAREN { ios }

io_decl:
  id=IDENT COLON t=simple_type_expr (* rate=io_rate ann=io_annot *)
    { mk_io_decl $loc (id, t, None, None) }
     
(* CORE EXPRESSIONS *)

core_expr:
        e=simple_core_expr
          { e }
      | e1=core_expr op=INFIX3 e2=core_expr
          { mk_cbinop $loc (op,$loc(op)) e1 e2 }
      | e1=core_expr op=INFIX2 e2=core_expr
          { mk_cbinop $loc (op,$loc(op)) e1 e2 }
      | e1=core_expr op=STAR e2=core_expr
          { mk_cbinop $loc ("*",$loc(op)) e1 e2 }
      (* | e1=core_expr op=GREATER e2=ore_expr
       *     { mk_cbinop $loc (">",$loc(op)) e1 e2 }
       * | e1=core_expr op=LESS e2=core_expr
       *     { mk_cbinop $loc ("<",$loc(op)) e1 e2 } *)

simple_core_expr:
      | id=IDENT
          { mk_core_expr $loc (EVar id) }
      | n=INT
          { mk_core_expr $loc (EInt n) }
      | TRUE
          { mk_core_expr $loc (EBool true) }
      | FALSE
          { mk_core_expr $loc (EBool false) }
      | LPAREN e=core_expr RPAREN
          { e }
    
(* TYPE EXPRESSIONS *)

(* type_expr:
 *   | ts = my_separated_nonempty_list(STAR, simple_type_expr)
 *            { match ts.te_des with
 *              | [] -> Misc.fatal_error "Parser.type_expr" (\* should not happen *\)
 *              | [t] -> mk_type_expr t
 *              | _ -> mk_type_expr (Typetuple (List.map mk_type_expr ts)) } *)

simple_type_expr:
      | id=IDENT 
          { mk_type_expr $loc (Typeconstr(id, [])) }
      | TY_UNIT 
          { mk_type_expr $loc (Typeconstr("unit", [])) }
      | TY_NAT 
          { mk_type_expr $loc (Typeconstr("nat", [])) }
      | TY_BOOL 
          { mk_type_expr $loc (Typeconstr("bool", [])) }
      | t=simple_type_expr id=IDENT
          { mk_type_expr $loc (Typeconstr(id, [t])) }
      (* | IDENT opt_size_exprs
       *     { mk_type_expr $loc (Typeconstr($1, [], $2)) } *)
      (* | simple_type_expr TY_ARRAY LBRACKET size_expr RBRACKET
       *     { mk_type_expr $loc (Typeconstr("array", [$1], [$4])) } *)
      (* | simple_type_expr IDENT opt_size_exprs
       *     { mk_type_expr $loc (Typeconstr($2, [$1], $3)) } *)
      (* | LPAREN type_expr COMMA type_expr_comma_list RPAREN IDENT opt_size_exprs
       *     { mk_type_expr $loc (Typeconstr($6, $2::$4, $7)) } *)
      (* | LPAREN t=type_expr RPAREN
       *     { t } *)

(* GRAPH DECLARATION *)

graph_decl:
  | GRAPH id=IDENT params=opt_params IN inps=io_decls OUT outps=io_decls d=graph_defn
    { mk_graph_decl $loc {g_id=id; g_params=params; g_ins=inps; g_outs=outps; g_defn=d } }

graph_defn:
  | STRUCT d=struct_graph_desc END { mk_graph_defn $loc (GD_Struct d) }
  | FUN d=fun_graph_desc END { mk_graph_defn $loc (GD_Fun d) }

(* STRUCTURAL GRAPH DESCRIPTION *)

struct_graph_desc:
  | ds = my_list(struct_defn)
           { { gs_wires = ds |> List.filter is_wire_decl |> List.map wire_decl_of;
               gs_nodes = ds |> List.filter is_node_decl |> List.map node_decl_of } }

struct_defn:
  | d=wire_defn { WireDecl d }
  | d=node_defn { NodeDecl d }

wire_defn:
  | WIRE id=IDENT COLON t=simple_type_expr
     { mk_wire_decl $loc (id,t) }

node_defn:
  | NODE id=IDENT COLON name=IDENT params=opt_node_params inps=node_ios outps=node_ios
     { mk_node_decl $loc (id, { gn_name=name; gn_params=params; gn_ins=inps; gn_outs=outps }) }

opt_node_params:
  | (* Nothing *) { [] }
  | LESS vs=my_separated_list(COMMA, core_expr) GREATER { vs }

node_ios:
  | LPAREN ios=my_separated_list(COMMA, node_io) RPAREN { ios }

node_io:
  | id=IDENT { id }

(* FUNCTIONAL GRAPH DESCRIPTION *)

fun_graph_desc:
  | ds = my_list(net_defn) { ds }  
            
net_defn:
  | VAL r=optional(REC) bs=my_separated_nonempty_list(AND, net_binding)
      { mk_net_defn $loc (r, bs) }

net_binding:
  | p=net_pattern EQUAL e=net_expr  (*%prec prec_define*)
      { mk_net_binding $loc (p, e) }
  | id=net_binding_name ps=my_nonempty_list(simple_net_pattern) EQUAL e=net_expr  (*%prec prec_define*)
          { mk_net_binding $loc (mk_net_pat $loc(id) (NPat_var id), mk_fun $loc ps e) }

net_binding_name:
      id=IDENT { id }
    | LPAREN op=INFIX0 RPAREN { op }

param_values:
    | LESS vs=my_separated_nonempty_list(COMMA,core_expr) GREATER { vs }
                  
net_expr:
        e=simple_net_expr
          { e }
      | f=simple_net_expr args=my_nonempty_list(simple_net_expr)   (*%prec prec_app*)
          { mk_apply $loc f args }
      | f=simple_net_expr params=param_values args=my_nonempty_list(simple_net_expr)   (*%prec prec_app*)
          { mk_apply2 $loc f params args }
      | es=net_expr_comma_list
          { mk_net_expr $loc (NTuple(List.rev es)) }
      | e1=net_expr COLONCOLON e2=net_expr 
          { mk_net_expr $loc (NCons(e1,e2)) }
      (* | e=simple_net_expr LBRACKET i=simple_net_expr RBRACKET
       *     { mk_net_expr $loc (NBundleElem (e,i)) }
       * | LBRACKET es=my_separated_nonempty_list(COMMA,net_expr) RBRACKET
       *     { mk_net_expr $loc (NBundle es) } *)
      | LET r=optional(REC) bs=my_separated_nonempty_list(AND,net_binding) IN e=net_expr  %prec prec_let
          { mk_net_expr $loc (NLet(r, bs, e)) }
      | FUN p=net_pattern ARROW e=net_expr
          { mk_net_expr $loc (NFun(p,e)) }
      | MATCH e=net_expr WITH cs=my_separated_nonempty_list(BAR,net_case) 
          { mk_net_expr $loc (NMatch(e,cs)) }
      | IF e1=net_expr THEN e2=net_expr ELSE e3=net_expr
          { mk_net_expr $loc (NIf(e1,e2,e3)) }
      | e1=net_expr op=INFIX3 e2=net_expr
          { mk_binop $loc (op,$loc(op)) e1 e2 }
      | e1=net_expr op=INFIX2 e2=net_expr
          { mk_binop $loc (op,$loc(op)) e1 e2 }
      | e1=net_expr op=INFIX0 e2=net_expr
          { mk_infix $loc (op,$loc(op)) e1 e2 }
      | e1=net_expr op=GREATER e2=net_expr
          { mk_binop $loc (">",$loc(op)) e1 e2 }
      | e1=net_expr op=LESS e2=net_expr
          { mk_binop $loc (">",$loc(op)) e1 e2 }
      | e1=net_expr op=STAR e2=net_expr
          { mk_binop $loc ("*",$loc(op)) e1 e2 }
      | e1=net_expr op=EQUAL e2=net_expr
          { mk_binop $loc ("=",$loc(op)) e1 e2 }
      | e1=net_expr op=NOTEQUAL e2=net_expr
          { mk_binop $loc ("!=",$loc(op)) e1 e2 }
      (* | u=MINUS e=net_expr %prec prec_uminus
       *     { mk_uminus $loc $loc(u) e } *)

simple_net_expr:
      | id=IDENT
          { mk_net_expr $loc (NVar id) }
      | LPAREN RPAREN
          { mk_net_expr $loc (NUnit) }
      | LBRACKET es=net_expr_comma_list RBRACKET
          { mk_net_expr $loc (NBundle (List.rev es)) }
      | LBRACKET RBRACKET
          { mk_net_expr $loc (NNil) }
      | n=INT
          { mk_net_expr $loc (NInt n) }
      | TRUE
          { mk_net_expr $loc (NBool true) }
      | FALSE
          { mk_net_expr $loc (NBool false) }
      | LPAREN e=net_expr RPAREN
          { e }

net_expr_comma_list:
        net_expr_comma_list COMMA net_expr
          { $3 :: $1 }
      | net_expr COMMA net_expr
          { [$3; $1] }

net_case: 
      p=net_pattern ARROW e=net_expr
          { mk_net_binding $loc (p,e) }

net_pattern:
        p=simple_net_pattern
          { p }
      | ps=net_pattern_comma_list
          { mk_net_pat $loc (NPat_tuple(List.rev ps)) }
      | p1=net_pattern COLONCOLON p2=net_pattern
          { mk_net_pat $loc (NPat_cons(p1,p2)) }
      | LBRACKET ps=net_pattern_comma_list RBRACKET
          { mk_net_pat $loc (NPat_bundle(List.rev ps)) }

simple_net_pattern:
      | id=IDENT
          { mk_net_pat $loc (NPat_var (id)) }
      | UNDERSCORE
          { mk_net_pat $loc (NPat_ignore) }
      | LPAREN net_pattern RPAREN
          { $2 }
      | LBRACKET RBRACKET
          { mk_net_pat $loc (NPat_nil) }
      | LPAREN RPAREN
          { mk_net_pat $loc (NPat_unit) }

net_pattern_comma_list:
        ps=net_pattern_comma_list COMMA p=net_pattern
          { p :: ps }
      | p1=net_pattern COMMA p2=net_pattern
          { [p2; p1] }

(* (\* PRAGMAs *\)
 * 
 * pragma:
 *   | PRAGMA id=IDENT LPAREN ps=my_separated_list(COMMA,pragma_param) RPAREN
 *      { mk_pragma_decl $loc (id,ps) } 
 * ;
 * 
 * pragma_param:
 *   | s=STRING 
 *       { s }
 * ; *)
%%
