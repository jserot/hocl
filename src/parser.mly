%token <string> IDENT
(* %token <string> PREFIX *)
%token <string> INFIX0
%token <string> INFIX2
%token <string> INFIX3
%token <int> INT
(* %token <string> STRING *)
%token EOF
%token EQUAL          (* "="*)
%token LPAREN         (* "("*)
%token RPAREN         (* ")"*)
%token STAR           (* "*"*)
%token COMMA          (* ","*)
%token ARROW   (* "->"*)
%token COLONCOLON     (* "::"*)
%token SEMI           (* ";"*)
%token COLON          (* ":"*)
%token LBRACKET       (* "["*)
%token RBRACKET       (* "]"*)
(* %token LBRACK         (\* "<"*\)
 * %token RBRACK         (\* ">"*\) *)
%token BAR            (* "|"*)
(* %token UNDERSCORE     (\* "_"*\) *)
%token TYPE           (* "type"*)
%token TY_NAT         (* "nat"*)
%token TY_BOOL        (* "bool"*)
%token TY_UNIT        (* "unit"*)
%token ACTOR          (* "actor"*)
%token FUN            (* "function"*)
%token MATCH          (* "match"*)
%token WITH           (* "with"*)
%token NET            (* "net"*)
%token LET            (* "let"*)
%token IN             (* "in"*)
%token AND            (* "and"*)
%token REC            (* "rec"*)
(* %token LIST           (\* "list"*\) *)
%token IF             (* "if"*)
%token THEN           (* "then"*)
%token ELSE           (* "else"*)
%token TRUE           (* "true"*)
%token FALSE          (* "false"*)
(* %token INITIALLY      (\* "initially"*\) *)

(* Precedences and associativities. Lower precedences first.*)

%right prec_let
(* %right prec_define *)
%right ARROW
(* %left  BAR *)
%left  COMMA
%left  INFIX0 EQUAL                     (* comparisons*)
%left  INFIX2                           (* additives, subtractives*)
%left  STAR INFIX3                      (* multiplicatives*)
%right COLONCOLON
(* %right prec_app *)
(* %right PREFIX                           (\* prefix operators, e.g. !*\) *)

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
let mk_unop l (op,l') e = mk_net_expr l (NApp (mk_net_expr l' (NVar op),e))

(* let negate_expr l s e = match s, e with
 *   | "-", {e_desc = EConst(Const.CInt n)} -> mk_expr l (EConst(Const.CInt(-n)))
 *   | s, e -> mk_unop l ("~" ^ s) e *)

type decl =
  | TyDecl of Syntax.type_decl
  | ActorDecl of Syntax.actor_decl
  | NetDefn of Syntax.net_defn

let is_type_decl = function TyDecl _ -> true | _ -> false             
let is_actor_decl = function ActorDecl _ -> true | _ -> false             
let is_net_defn  = function NetDefn _ -> true | _ -> false             

let type_decl_of = function TyDecl d -> d | _ -> Misc.fatal_error "Parser.type_decl_of"             
let actor_decl_of = function ActorDecl d -> d | _ -> Misc.fatal_error "Parser.actor_decl_of"             
let net_defn_of  = function NetDefn d -> d | _ -> Misc.fatal_error "Parser.net_defn_of"             
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
          Syntax.defns = decls |> List.filter is_net_defn |> List.map net_defn_of } 
      }

(* DECLARATIONS *)

decl:
    d = type_decl SEMI { TyDecl d }
  | d = actor_decl SEMI { ActorDecl d }
  | d = net_defn SEMI { NetDefn d }
          
          (* TYPE DECLARATION *)

type_decl:
  | TYPE id=IDENT 
      { mk_type_decl $loc (Syntax.Opaque_type_decl id) }

(* ACTOR DECLARATION *)

actor_decl:
   ACTOR id=IDENT COLON t1=my_separated_nonempty_list(STAR, simple_type_expr)
         ARROW t2=my_separated_nonempty_list(STAR, simple_type_expr)
    { mk_actor_decl $loc {a_id=id; a_ins=t1; a_outs=t2} }

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
        NET r=optional(REC) bs=my_separated_nonempty_list(AND, net_binding)
          { mk_net_defn $loc (r, bs) }

net_binding:
      p=net_pattern EQUAL e=net_expr  (*%prec prec_define*)
          { mk_net_binding $loc (p, e) }
    | id=IDENT ps=my_nonempty_list(simple_net_pattern) EQUAL e=net_expr  (*%prec prec_define*)
          { mk_net_binding $loc (mk_net_pat $loc(id) (NPat_var id), mk_fun $loc ps e) }

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
          { mk_binop $loc (op,$loc(op)) e1 e2 }
      | e1=net_expr op=STAR e2=net_expr
          { mk_binop $loc ("*",$loc(op)) e1 e2 }
      | e1=net_expr op=EQUAL e2=net_expr
          { mk_binop $loc ("=",$loc(op)) e1 e2 }

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
      (* | UNDERSCORE
       *     { mk_net_pat $loc (NPat_ignore) } *)
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
%%
