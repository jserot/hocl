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
%token EOF
%token EQUAL          (* "="*)
%token LPAREN         (* "("*)
%token RPAREN         (* ")"*)
%token COMMA          (* ","*)
%token ARROW          (* "->"*)
%token SEMI           (* ";"*)
%token UNDERSCORE     (* "_"*)
%token COLON          (* ":"*)
%token END           
%token IN          
%token OUT        
%token FUN       
%token NODE    
%token VAL 
%token LET 
%token AND 
%token REC
%token <int> INT
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> INFIX3
%token TYPE
%token GRAPH
%token LBRACKET
%token RBRACKET
%token COLONCOLON
%token MATCH
%token WITH
%token BAR
%token STRUCT   
%token BOX    
%token WIRE   
%token <string> STRING
%token ACTOR
%token LBRACE
%token RBRACE
%token <string> TYVAR
%token QUOTE

(* Precedences and associativities. Lower precedences first.*)

%left  COMMA
%left  INFIX0 (* rev app operators (|>, >>) *)
%left  INFIX1 (* comparisons*)
%left  INFIX2 (* additives, subtractives*)
%left  INFIX3 (* multiplicatives *)
%right COLONCOLON

(* Entry points*)

%start program
%type <Syntax.program> program

%{
open Syntax
open Location

type top_decl =
  | TypeDecl of Syntax.type_decl
  | ValueDecl of Syntax.val_decl
  | NodeDecl of Syntax.node_decl

let get_type_decl acc d = match d with TypeDecl d -> d::acc | _ -> acc               
let get_value_decl acc d = match d with ValueDecl d -> d::acc | _ -> acc               
let get_node_decl acc d = match d with NodeDecl d -> d::acc | _ -> acc               

type struct_decl =
  | WireDecl of Syntax.wire_decl list
  | BoxDecl of Syntax.box_decl

let get_wire_decls acc ds = match ds with WireDecl ds -> acc @ ds | _ -> acc               
let get_box_decl acc d = match d with BoxDecl d -> d::acc | _ -> acc               

let mk_location (p1,p2) =
  let open Lexing in
  Loc (!input_name, p1.pos_bol+p1.pos_cnum, p2.pos_bol+p2.pos_cnum)

let mk_type_expr l desc = { te_desc = desc; te_loc = mk_location l; te_typ = Types.no_type }
let mk_type_decl l desc = { td_desc = desc; td_loc = mk_location l }
let mk_io_decl l desc = { io_desc = desc; io_loc = mk_location l; io_typ = Types.no_type }
let mk_val_decl l desc = { vd_desc = desc; vd_loc = mk_location l }
let mk_node_decl l desc = { nd_desc = desc; nd_loc = mk_location l }
let mk_expr l desc = { e_desc = desc; e_loc = mk_location l; e_typ = Types.no_type }
let mk_pat l desc = { p_desc = desc; p_loc = mk_location l; p_typ = Types.no_type }
let mk_binding l desc = { b_desc = desc; b_loc = mk_location l}
let mk_wire_decl l desc = { wr_desc = desc; wr_loc = mk_location l }
let mk_box_decl l desc = { bx_desc = desc; bx_loc = mk_location l }

let rec mk_apply l f es = match es with
  | [] -> f
  | e2::e2s -> mk_apply l (mk_expr l (EApp (f, e2))) e2s (* TO FIX: location ! *)
let rec mk_fun l pats e = match pats with
  | [] -> Misc.fatal_error "Parser.mk_fun" (* should not happen *)
  | [p] -> mk_expr l (EFun (p,e))
  | p::ps -> mk_expr l (EFun (p, mk_fun l ps e)) (* TO FIX: location *)
let mk_binop l op e1 e2 = mk_expr l (EBinop (op,e1,e2))
let mk_infix l (op,l') e1 e2 = mk_apply l (mk_expr l' (EVar op)) [e1; e2]

%}

%%

(* PROGRAM *)

program:
  | decls = list(top_decl) EOF
              { { types = decls |> List.fold_left get_type_decl [] |> List.rev;
                  values = decls |> List.fold_left get_value_decl [] |> List.rev; 
                  nodes = decls |> List.fold_left get_node_decl [] |> List.rev; } }

(* TOP DECLARATIONS *)

top_decl:
    d = type_decl SEMI { TypeDecl d }
  | d = val_decl SEMI { ValueDecl d }
  | d = node_decl SEMI { NodeDecl d }

(* TYPE DECLARATIONS *)

type_decl:
  | TYPE id=IDENT 
      { mk_type_decl $sloc (id, TD_Abstract) }

(* NODE and GRAPH DECLARATIONS *)

node_decl:
   NODE id=IDENT IN inps=io_decls OUT outps=io_decls impl=option(node_impl)
     { mk_node_decl
       $sloc
       (id, { n_intf={n_id=id; n_isgraph=false; n_ins=inps; n_outs=outps };
            n_impl=match impl with None -> NM_Actor [] | Some im -> im }) }
  | GRAPH id=IDENT IN inps=io_decls OUT outps=io_decls impl=node_impl
     { mk_node_decl
       $sloc
       (id,{ n_intf={n_id=id; n_isgraph=true; n_ins=inps; n_outs=outps };
            n_impl=impl })}

node_impl:
  | FUN f=list(val_decl) END { NM_Fun f }
  | STRUCT s=struct_graph_desc END { NM_Struct s }
  | ACTOR d=list(actor_desc) END { NM_Actor d }

actor_desc:
  | t=IDENT LPAREN attrs=separated_list(COMMA,impl_attr) RPAREN { (t,attrs) }

impl_attr:
  | name=IDENT EQUAL v=STRING { name, v }
  | name=IDENT { name, "" }

io_decls:
  | LPAREN ios=separated_list(COMMA,io_decl) RPAREN { ios }

io_decl:
  | id=IDENT COLON t=type_expr e=option(io_expr) anns=io_annots { mk_io_decl $sloc (id,t,e,anns) }

io_expr:
  | EQUAL e=simple_expr { mk_expr $sloc (EQuote e) }

io_annots:
  | (* Nothing *) { [] }
  | LBRACKET e=basic_expr RBRACKET { ["rate", AN_Expr e] }
  | LBRACE anns=separated_list(COMMA,io_annot) RBRACE { anns }

io_annot:
  | name=IDENT EQUAL value=STRING { name, AN_String value }

simple_type_expr:
      | c=IDENT { mk_type_expr $sloc (Typeconstr (c,[])) }
      | tv=TYVAR { mk_type_expr $sloc (Typevar tv) }

type_expr:
      | t=simple_type_expr { t }
      | t=simple_type_expr c=IDENT { mk_type_expr $sloc (Typeconstr (c,[t])) }
                       
(* VAL DECLARATIONS *)

val_decl:
  | VAL r=boption(REC) bs=separated_nonempty_list(AND,binding)
      { mk_val_decl $sloc (r, bs) }

binding:
  | p=pattern EQUAL e=expr  (*%prec prec_define*)
      { mk_binding $sloc (p,e) }
  | id=binding_name ps=nonempty_list(fun_pattern) EQUAL e=expr
      { mk_binding $sloc (mk_pat $sloc (Pat_var id), mk_fun $sloc ps e) }

binding_name:
      id=IDENT { id }
    | LPAREN op=INFIX0 RPAREN { op }

expr:
        e=simple_expr
          { e }
      | f=simple_expr args=nonempty_list(simple_expr)   (*%prec prec_app*)
          { mk_apply $sloc f args }
      | es=expr_comma_list
          { mk_expr $sloc (ETuple (List.rev es)) }
      | FUN ps=nonempty_list(fun_pattern) ARROW e=expr
          { mk_fun $sloc ps e }
      | LET r=boption(REC) bs=separated_nonempty_list(AND,binding) IN e=expr
          { mk_expr $sloc (ELet (r,bs,e)) }
      | IF e1=expr THEN e2=expr ELSE e3=expr
          { mk_expr $sloc (EIf (e1,e2,e3)) }
      | e1=expr op=INFIX0 e2=expr
          { mk_infix $sloc (op,$loc(op)) e1 e2 }
      | e1=expr op=INFIX1 e2=expr
          { mk_binop $sloc op e1 e2 }
      | e1=expr op=INFIX2 e2=expr
          { mk_binop $sloc op e1 e2 }
      | e1=expr op=INFIX3 e2=expr
          { mk_binop $sloc op e1 e2 }
      | e1=expr op=EQUAL e2=expr
          { mk_binop $sloc "=" e1 e2 }
      | e1=expr COLONCOLON e2=expr 
          { mk_expr $sloc (ECons(e1,e2)) }
      | e=simple_expr LBRACKET i=simple_expr RBRACKET
          { mk_expr $sloc (EListElem (e,i)) }
      | MATCH e=expr WITH cs=separated_nonempty_list(BAR,match_case) 
          { mk_expr $sloc (EMatch (e,cs)) }

match_case: 
      p=pattern ARROW e=expr
          { mk_binding $sloc (p,e) }

simple_expr:
      | id=IDENT
          { mk_expr $sloc (EVar id) }
      | LPAREN e=expr RPAREN
          { e }
      | LPAREN RPAREN
          { mk_expr $sloc EUnit }
      | e=const_expr
          { e }
      | LBRACKET es=separated_nonempty_list(SEMI,expr) RBRACKET
          { mk_expr $sloc (EList es) }
      | LBRACKET RBRACKET
          { mk_expr $sloc ENil }
      | QUOTE e=basic_expr QUOTE
          { mk_expr $sloc (EQuote e) }

const_expr:      
      | n=INT
          { mk_expr $sloc (EInt n) }
      | TRUE
          { mk_expr $sloc (EBool true) }
      | FALSE
          { mk_expr $sloc (EBool false) }

expr_comma_list:
        expr_comma_list COMMA expr
          { $3 :: $1 }
      | expr COMMA expr
          { [$3; $1] }

pattern:
        p=simple_pattern
          { p }
      | ps=pattern_comma_list
          { mk_pat $sloc (Pat_tuple (List.rev ps)) }
      | p1=pattern COLONCOLON p2=pattern
          { mk_pat $sloc (Pat_cons(p1,p2)) }
      | LBRACKET ps=separated_nonempty_list(SEMI,simple_pattern) RBRACKET
          { mk_pat $sloc (Pat_list ps) }

fun_pattern:
      | id=IDENT
          { mk_pat $sloc (Pat_var id) }

simple_pattern:
      | id=IDENT
          { mk_pat $sloc (Pat_var id) }
      | UNDERSCORE
          { mk_pat $sloc Pat_ignore }
      | LPAREN pattern RPAREN
          { $2 }
      | LPAREN RPAREN
          { mk_pat $sloc Pat_unit }
      | LBRACKET RBRACKET
          { mk_pat $sloc Pat_nil }

pattern_comma_list:
        ps=pattern_comma_list COMMA p=pattern
          { p :: ps }
      | p1=pattern COMMA p2=pattern
          { [p2; p1] }

(* STRUCTURAL GRAPH DESCRIPTIONS *)

struct_graph_desc:
  | decls = list(struct_decl)
              { { gs_wires = decls |> List.fold_left get_wire_decls [];
                  gs_boxes = decls |> List.fold_left get_box_decl [] |> List.rev; } }

struct_decl:
  | wire_decl { WireDecl $1 }
  | box_decl { BoxDecl $1 }

wire_decl:
  | WIRE ids=separated_list(COMMA,IDENT) COLON t=type_expr
     { List.map (fun id -> mk_wire_decl $sloc (id,t)) ids }

box_decl:
  | BOX id=IDENT COLON node=IDENT inps=box_inps outps=box_outps
     { mk_box_decl $sloc (id, { bx_node=node; bx_ins=inps; bx_outs=outps }) }

box_inps:
  | LPAREN ios=separated_list(COMMA,box_inp) RPAREN { ios }

box_outps:
  | LPAREN ios=separated_list(COMMA,box_outp) RPAREN { ios }

box_inp:
  | id=IDENT { mk_expr $sloc (EVar id) }
  | QUOTE e=basic_expr QUOTE { e }

box_outp:
  | id=IDENT { mk_expr $sloc (EVar id) }
                 
(* box_decl:
 *   | BOX id=IDENT COLON node=IDENT params=loption(box_params) inps=box_ios outps=box_ios
 *      { mk_box_decl $sloc (id, { bx_node=node; bx_params=params; bx_ins=inps; bx_outs=outps }) }
 * 
 * box_params:
 *   | LBRACKET vs=separated_list(COMMA,basic_expr) RBRACKET { vs } *)

(* quoted_expr:
 *       | LBRACE e=basic_expr RBRACE
 *           { mk_expr $sloc (EQuote e) } *)

basic_expr:
      | id=IDENT
          { mk_expr $sloc (EVar id) }
      | e=const_expr
          { e }
      | e1=basic_expr op=INFIX1 e2=basic_expr
          { mk_binop $sloc op e1 e2 }
      | e1=basic_expr op=INFIX2 e2=basic_expr
          { mk_binop $sloc op e1 e2 }
      | e1=basic_expr op=INFIX3 e2=basic_expr
          { mk_binop $sloc op e1 e2 }
      | LPAREN e=basic_expr RPAREN
          { e }

%%
