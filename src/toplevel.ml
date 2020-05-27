open Typing 
open Semval
open Syntax

exception Toplevel
exception Invalid_directive of string

type cfg = {
  dot_file: string;
  }

let cfg = {
  dot_file = "/tmp/hocl_top.dot"
  }

type ctx = {
  mutable tenv: Typing.tenv;
  mutable venv: Typing.venv;
  mutable senv: Semval.sem_env;
  mutable boxes: Semval.box_env;
  mutable wires: Semval.wire_env;
  mutable dot_output: bool;
  }

let print_banner () = 
  Printf.printf "Welcome to the HoCL interpreter version %s\n" Version.version;
  Printf.printf "Type \"#help\" to get help\n";
  flush stdout

let help () = 
  Printf.printf "\"> type <name>;\" to define a type\n";
  Printf.printf "\"> node <name> in (<id>:<type>, ...) out (<id>:<type>,...);\" to define an actor\n";
  Printf.printf "\"> input <name> : <type;\" to define a graph input\n";
  Printf.printf "\"> val <name> = <expr>;\" to define a value\n";
  Printf.printf "\"> #print_tenv;\" to print the typing environment\n";
  Printf.printf "\"> #print_senv;\" to print the static (eval) environment\n";
  Printf.printf "\"> #display;\" to write the current graph in file for display with Graphviz\n";
  Printf.printf "\"> #clear_graph;\" to clear the current graph (deleting all nodes and wires)\n";
  Printf.printf "\"> #clear_all;\" to clear current graph and all environments\n";
  Printf.printf "\"> #help;\" to get this help\n"

let parse () = 
  let lexbuf = !Location.input_lexbuf in
  Parser.phrase Lexer.main lexbuf

(* let print_phrase p =
 *   Printf.printf "> %s\n" (Syntax.string_of_phrase p);
 *   flush stdout *)

let eval_node_decl (id,d) = 
  let n = {
      sn_id = id;
      sn_kind = ActorN;
      sn_ins = List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, e, anns) d.n_intf.n_ins;
      sn_supplied_ins = [];
      sn_outs = List.map (fun {io_desc=id,_,e,anns; io_typ=ty} -> id, ty, anns) d.n_intf.n_outs
      } in
  SVNode n

let print_type (id,arity) =
  Printf.printf "> type %s\n" id (* Arity is always 0 for user-defined types *)
  
let print_node (id,t,v) =
  Printf.printf "> node %s: %s = %s\n" id (Pr_type.string_of_type_scheme t) (string_of_semval v)

let print_box (id,b) = 
  Printf.printf "> box B%d: %s\n" id (string_of_box ~typed:true b)
  
let print_wire (id,w) = 
  Printf.printf "> wire W%d: %s\n" id (string_of_wire ~typed:true w)

let print_val venv (id,v) =
  let ty =
    try List.assoc id venv
    with Not_found -> Misc.fatal_error "Toplevel.print_val" in
  Printf.printf "> val %s: %s = %s\n" id (Pr_type.string_of_type_scheme ty) (string_of_semval v)
  
let dump_graph ctx =
  let oc = open_out cfg.dot_file  in
  Dot.output_graph oc ctx.boxes ctx.wires;
  close_out oc

let eval_io_decl eval_fn ctx ({ io_desc = id,t,_,_ } as d)  = 
       let ty = type_of_type_expression ctx.tenv t in
       let venv' = [id,Types.generalize [] ty] in
       let senv', boxes' = eval_fn d in
       List.iter (print_val venv') [senv'];
       List.iter print_box [boxes'];
       ctx.venv <- venv' @ ctx.venv;
       ctx.senv <- [senv'] @ ctx.senv;
       ctx.boxes <- ctx.boxes @ [boxes']
  
let compile_phrase ctx =
    let p = parse () in
    (* print_phrase p; *)
    match p with
    | TypeDecl d ->
       let tycon = type_type_decl ctx.tenv d in
       ctx.tenv <- { ctx.tenv with te_cons = tycon :: ctx.tenv.te_cons };
       print_type tycon   
    | NodeDecl { nd_desc = id, d } ->
       let t, ve_i, ve_o = type_node_intf ctx.tenv d.n_intf in
       let v = eval_node_decl (id,d) in
       ctx.venv <- (id,t)::ctx.venv;
       ctx.senv <- (id,v)::ctx.senv;
       print_node (id,t,v)
    | InpDecl d ->
       eval_io_decl Eval.eval_node_input ctx d
    | OutpDecl d ->
       eval_io_decl Eval.eval_node_output ctx d
    | ValDecl { vd_loc = loc; vd_desc = isrec, defns } ->
       let venv' = type_definitions loc isrec ctx.tenv ctx.venv defns in
       ctx.venv <- venv' @ ctx.venv;
       let senv', boxes', wires' = Eval_fun.eval_definitions true loc isrec (ctx.senv,ctx.boxes) defns in
       let boxes'', wires'' = Eval_fun.shorten_rec_paths boxes' wires' in
       List.iter (print_val venv') senv';
       List.iter print_box boxes'';
       List.iter print_wire wires'';
       ctx.senv <- senv' @ ctx.senv;
       ctx.boxes <- ctx.boxes @ boxes'';
       ctx.wires <- ctx.wires @ wires''
    | Directive ("print_tenv", _) -> Typing.dump_typing_environment "Typing environment" (ctx.tenv.te_cons,[])
    | Directive ("print_senv", _) -> Static.dump_static_env "Static environment" ctx.venv ctx.senv
    | Directive ("display", _) ->
       ctx.dot_output <- true;
       Printf.printf "> Writing file %s...\n" cfg.dot_file
    | Directive ("clear_graph", _) ->
       ctx.boxes <- [];
       ctx.wires <- []
    | Directive ("clear_all", _) ->
       if Options.cfg.Options.builtins then begin
         ctx.tenv <- { te_cons = fst Builtins.typing_env; te_vars = []};
         ctx.venv <- snd Builtins.typing_env;
         ctx.senv <- Builtins.static_env
         end
       else begin
         ctx.tenv <- { te_cons = []; te_vars = [] };
         ctx.venv <- [];
         ctx.senv <- []
         end;
       ctx.boxes <- [];
       ctx.wires <- []
    | Directive ("help", _) -> help ()
    | Directive ("use", fname) -> Error.not_implemented "Toplevel #use directive"
    | Directive (s,_) -> raise (Invalid_directive s)
    | EoF -> raise End_of_file

let run () =
  let lexbuf = Lexing.from_channel !Location.input_chan in
  let ctx = 
    if Options.cfg.Options.builtins then
      { tenv = { te_cons = fst Builtins.typing_env; te_vars = []};
        venv = snd Builtins.typing_env;
        senv = Builtins.static_env;
        boxes = [];
        wires = [];
        dot_output = false }
    else
      { tenv = { te_cons = []; te_vars = [] };
        venv = [];
        senv = [];
        boxes = [];
        wires = [];
        dot_output = false } in
  Location.input_lexbuf := lexbuf;
  print_banner ();
  while true do
    try
      print_string "# "; flush stdout;
      compile_phrase ctx;
      if ctx.dot_output then dump_graph ctx
    with
    | Parser.Error -> Compiler.syntax_error ()
    | Lexer.Lexical_error (errcode,pos1,pos2) -> Compiler.lexical_error (errcode,pos1,pos2)
    | Misc.Error -> flush stderr
    | Invalid_directive s -> Printf.printf "Unknown directive: #%s\n" s; flush stdout
    | e -> raise e
  done
  
