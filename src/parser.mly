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
%token <string> INFIX1
%token <string> INFIX2
%token <string> INFIX3
%token <int> INT
%token <string> STRING
%token EOF
%token EQUAL          (* "="*)
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
%token TYPE           (* "type"*)
%token TY_NAT         (* "nat"*)
%token TY_BOOL        (* "bool"*)
%token TY_UNIT        (* "unit"*)
%token BCAST          (* "bcast"*)
%token GRAPH          (* "graph"*)
%token ACTOR          (* "actor"*)
%token PARAMETER      (* "parameter"*)
%token PARAM          (* "param"*)
%token SOURCE         (* "source"*)
%token SINK           (* "sink"*)
%token FUN            (* "function"*)
%token MATCH          (* "match"*)
%token WITH           (* "with"*)
%token LET            (* "let"*)
%token IN             (* "in"*)
%token OUT            (* "out"*)
%token AND            (* "and"*)
%token REC            (* "rec"*)
(* %token LIST           (\* "list"*\) *)
%token IF             (* "if"*)
%token THEN           (* "then"*)
%token ELSE           (* "else"*)
%token TRUE           (* "true"*)
%token FALSE          (* "false"*)
(* %token INITIALLY      (\* "initially"*\) *)
%token PRAGMA         /* "#pragma" */

(* Precedences and associativities. Lower precedences first.*)

(* %nonassoc IN *)
%right prec_let
(* %right prec_define *)
%right ARROW
(* %left  BAR *)
%left  COMMA
%left  INFIX0                           (* rev app operators (|>, >>) *)
%left  INFIX1 EQUAL                     (* comparisons*)
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

let mk_type_decl l desc = { td_desc = desc; td_loc = mk_location l }
let mk_pragma_decl l desc = { pr_desc = desc; pr_loc = mk_location l }
let mk_param_decl l desc = { pd_desc = desc; pd_loc = mk_location l }
let mk_io_decl l desc = { io_desc = desc; io_loc = mk_location l }
let mk_actor_decl l desc = { ad_desc = desc; ad_loc = mk_location l }
let mk_type_expr l desc = { te_desc = desc; te_loc = mk_location l; te_typ = Types.no_type}
let mk_net_defn l desc = { nd_desc = desc; nd_loc = mk_location l }
let mk_net_expr l desc = { ne_desc = desc; ne_loc = mk_location l; ne_typ = Types.no_type }
let mk_net_pat l desc = { np_desc = desc; np_loc = mk_location l; np_typ = Types.no_type }
let mk_net_binding l desc = { nb_desc = desc; nb_loc = mk_location l}
let rec mk_apply l e es = match (e,es) with
  | e1, [] -> e1
  | e1, e2::e2s -> mk_apply l (mk_net_expr l (NApp(e1, e2))) e2s (* TO FIX: location *)
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

type decl =
  | TyDecl of Syntax.type_decl
  | ParamDecl of Syntax.param_decl
  | IoDecl of Syntax.io_decl
  | ActorDecl of Syntax.actor_decl
  | NetDefn of Syntax.net_defn
  | PragmaDecl of Syntax.pragma_decl

let is_type_decl = function TyDecl _ -> true | _ -> false             
let is_param_decl = function ParamDecl _ -> true | _ -> false             
let is_io_decl = function IoDecl _ -> true | _ -> false             
let is_actor_decl = function ActorDecl _ -> true | _ -> false             
let is_net_defn  = function NetDefn _ -> true | _ -> false             
let is_pragma_decl = function PragmaDecl _ -> true | _ -> false             

let type_decl_of = function TyDecl d -> d | _ -> Misc.fatal_error "Parser.type_decl_of"             
let param_decl_of = function ParamDecl d -> d | _ -> Misc.fatal_error "Parser.param_decl_of"             
let io_decl_of = function IoDecl d -> d | _ -> Misc.fatal_error "Parser.io_decl_of"             
let actor_decl_of = function ActorDecl d -> d | _ -> Misc.fatal_error "Parser.actor_decl_of"             
let net_defn_of  = function NetDefn d -> d | _ -> Misc.fatal_error "Parser.net_defn_of"             
let pragma_decl_of = function PragmaDecl d -> d | _ -> Misc.fatal_error "Parser.pragma_decl_of"             
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
          Syntax.params = decls |> List.filter is_param_decl |> List.map param_decl_of;
          Syntax.ios = decls |> List.filter is_io_decl |> List.map io_decl_of;
          Syntax.actors = decls |> List.filter is_actor_decl |> List.map actor_decl_of;
          Syntax.defns = decls |> List.filter is_net_defn |> List.map net_defn_of;
          Syntax.pragmas = decls |> List.filter is_pragma_decl |> List.map pragma_decl_of } 
      }

(* DECLARATIONS *)

decl:
    d = type_decl SEMI { TyDecl d }
  | d = param_decl SEMI { ParamDecl d }
  | d = actor_decl SEMI { ActorDecl d }
  | d = io_decl SEMI { IoDecl d }
  | d = net_defn SEMI { NetDefn d }
  | d = pragma { PragmaDecl d }
          
(* TYPE DECLARATION *)

type_decl:
  | TYPE id=IDENT 
      { mk_type_decl $loc (Syntax.Opaque_type_decl id) }

(* PARAMETER DECLARATION *)

param_decl:
  | PARAMETER id=IDENT COLON ty=simple_type_expr  (* Input parameter *)
      { mk_param_decl $loc (id,P_Input,ty,mk_net_expr $loc (NUnit)) }
  | PARAMETER id=IDENT COLON ty=simple_type_expr EQUAL e=net_expr (* Locally static parameter *)
      { mk_param_decl $loc (id,P_Local,ty,e) }

(* SOURCE/SINK DECLARATION *)

io_decl:
  | kind=io_kind id=IDENT params=io_params COLON ty=simple_type_expr
      { mk_io_decl $loc (kind,id,params,ty) }

io_params:
  | (* Nothing *) { [] }
  | LPAREN ps=my_separated_list(COMMA, actor_param) RPAREN { ps }

io_kind:
  | SOURCE { Io_src }
  | SINK { Io_snk }

(* ACTOR DECLARATION *)

actor_decl:
   kind=actor_kind id=IDENT params=actor_params IN LPAREN inps=my_separated_list(COMMA, actor_io) RPAREN
                 OUT LPAREN outps=my_separated_list(COMMA, actor_io) RPAREN
    { mk_actor_decl $loc {a_kind=kind; a_id=id; a_params=params; a_ins=inps; a_outs=outps} }

actor_kind:
  | ACTOR { A_Regular }
  | BCAST { A_Bcast }
  | GRAPH { A_Graph }
    
actor_params:
  | (* Nothing *) { [] }
  | PARAM LPAREN ps=my_separated_list(COMMA, actor_param) RPAREN { ps }

actor_param:
   id=IDENT COLON t=simple_type_expr
    { (id, t) }

actor_io:
   id=IDENT COLON t=simple_type_expr ann=io_annot
    { (id, t, ann) }
     
io_annot:
  | (* Nothing *)
      { "" }
  | s=STRING
      { s }
  (* | LBRACE e=net_expr RBRACE
   *     { e } *)
    
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

(* NET DEFNS*)

net_defn:
        LET r=optional(REC) bs=my_separated_nonempty_list(AND, net_binding)
          { mk_net_defn $loc (r, bs) }

net_binding:
      p=net_pattern EQUAL e=net_expr  (*%prec prec_define*)
          { mk_net_binding $loc (p, e) }
    | id=net_binding_name ps=my_nonempty_list(simple_net_pattern) EQUAL e=net_expr  (*%prec prec_define*)
          { mk_net_binding $loc (mk_net_pat $loc(id) (NPat_var id), mk_fun $loc ps e) }

net_binding_name:
      id=IDENT { id }
    | LPAREN op=INFIX0 RPAREN { op }

net_expr:
        e=simple_net_expr
          { e }
      | e=simple_net_expr es=my_nonempty_list(simple_net_expr)   (*%prec prec_app*)
          { mk_apply $loc e es }
      | es=net_expr_comma_list
          { mk_net_expr $loc (NTuple(List.rev es)) }
      | e1=net_expr COLONCOLON e2=net_expr 
          { mk_net_expr $loc (NCons(e1,e2)) }
      | e=simple_net_expr LBRACKET i=simple_net_expr RBRACKET
          { mk_net_expr $loc (NListElem (e,i)) }
      (* | LBRACKET es=my_separated_nonempty_list(COMMA,net_expr) RBRACKET
       *     { mk_net_expr $loc (NList es) } *)
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
      | e1=net_expr op=INFIX1 e2=net_expr
          { mk_binop $loc (op,$loc(op)) e1 e2 }
      | e1=net_expr op=STAR e2=net_expr
          { mk_binop $loc ("*",$loc(op)) e1 e2 }
      | e1=net_expr op=EQUAL e2=net_expr
          { mk_binop $loc ("=",$loc(op)) e1 e2 }
      (* | u=MINUS e=net_expr %prec prec_uminus
       *     { mk_uminus $loc $loc(u) e } *)

simple_net_expr:
      | id=IDENT
          { mk_net_expr $loc (NVar id) }
      | LPAREN RPAREN
          { mk_net_expr $loc (NUnit) }
      | LBRACKET es=net_expr_comma_list RBRACKET
          { mk_net_expr $loc (NList (List.rev es)) }
      | LBRACKET RBRACKET
          { mk_net_expr $loc (NNil) }
      | n=INT
          { mk_net_expr $loc (NNat n) }
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

(* Patterns*)

net_pattern:
        p=simple_net_pattern
          { p }
      | ps=net_pattern_comma_list
          { mk_net_pat $loc (NPat_tuple(List.rev ps)) }
      | p1=net_pattern COLONCOLON p2=net_pattern
          { mk_net_pat $loc (NPat_cons(p1,p2)) }
      | LBRACKET ps=net_pattern_comma_list RBRACKET
          { mk_net_pat $loc (NPat_list(List.rev ps)) }

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

(* PRAGMAs *)

pragma:
  | PRAGMA id=IDENT LPAREN ps=my_separated_list(COMMA,pragma_param) RPAREN
     { mk_pragma_decl $loc (id,ps) } 
;

pragma_param:
  | s=STRING 
      { s }
;
%%
