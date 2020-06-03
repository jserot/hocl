open Typing 
open Semval
open Syntax

exception Toplevel
exception Invalid_directive of string
exception NestedUse

type cfg = {
  mutable dot_output: bool;
  mutable dot_file: string;
  mutable verbose_level: int
  }

let cfg = {
  dot_output = false;
  dot_file = "/tmp/hocl_top.dot";
  verbose_level = 1;
  }

type ctx = {
  mutable tenv: Typing.tenv;
  mutable venv: Typing.venv;
  mutable senv: Semval.sem_env;
  mutable boxes: Semval.box_env;
  mutable wires: Semval.wire_env;
  mutable lexbuf: Lexing.lexbuf;
  mutable in_use: bool
  }

let print_banner () = 
  Printf.printf "Welcome to the HoCL interpreter version %s\n" Version.version;
  Printf.printf "Type \"#help;\" to get help\n";
  flush stdout

let help () = 
  Printf.printf "\"> type <name>;\" to define a type\n";
  Printf.printf "\"> node <name> in (<id>:<type>, ...) out (<id>:<type>,...);\" to define an actor\n";
  Printf.printf "\"> input <name> : <type;\" to define a graph input\n";
  Printf.printf "\"> output <name> : <type;\" to define a graph output\n";
  Printf.printf "\"> val <name> = <expr>;\" to define a value\n";
  Printf.printf "\"> #print_tenv;\" to print the typing environment\n";
  Printf.printf "\"> #print_senv;\" to print the static (eval) environment\n";
  Printf.printf "\"> #dump_dot;\" to toggle dumping of current graph in DOT format in file\n";
  Printf.printf "\"> #dot_file <fname>;\" to set the DIT dump file (default: /tmp/hocl_top.dot)\n";
  Printf.printf "\"> #clear_graph;\" to clear the current graph (deleting all nodes and wires)\n";
  Printf.printf "\"> #clear_all;\" to clear current graph and all environments\n";
  Printf.printf "\"> #use <fname>;\" to load phrases from a file\n";
  Printf.printf "\"> #verbose <level>;\" to set verbosity level (0,1,2)\n";
  Printf.printf "\"> #help;\" to get this help\n"

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
       if cfg.verbose_level > 0 then List.iter (print_val venv') [senv'];
       if cfg.verbose_level > 1 then List.iter print_box [boxes'];
       ctx.venv <- venv' @ ctx.venv;
       ctx.senv <- [senv'] @ ctx.senv;
       ctx.boxes <- ctx.boxes @ [boxes']
  
let compile_phrase ctx =
    let p = Parser.toplevel_phrase Lexer.main ctx.lexbuf in
    (* print_phrase p; *)
    match p with
    | TypeDecl d ->
       let tycon = type_type_decl ctx.tenv d in
       ctx.tenv <- { ctx.tenv with te_cons = tycon :: ctx.tenv.te_cons };
       if cfg.verbose_level > 0 then print_type tycon   
    | NodeDecl { nd_desc = id, d } ->
       let t, ve_i, ve_o = type_node_intf ctx.tenv d.n_intf in
       let v = eval_node_decl (id,d) in
       ctx.venv <- (id,t)::ctx.venv;
       ctx.senv <- (id,v)::ctx.senv;
       if cfg.verbose_level > 0 then print_node (id,t,v)
    | InpDecl d ->
       eval_io_decl Eval.eval_node_input ctx d
    | OutpDecl d ->
       eval_io_decl Eval.eval_node_output ctx d
    | ValDecl { vd_loc = loc; vd_desc = isrec, defns } ->
       let venv' = type_definitions loc isrec ctx.tenv ctx.venv defns in
       ctx.venv <- venv' @ ctx.venv;
       let senv', boxes', wires' = Eval_fun.eval_definitions true loc isrec (ctx.senv,ctx.boxes) defns in
       let boxes'', wires'' = Eval_fun.shorten_rec_paths boxes' wires' in
       if cfg.verbose_level > 0 then List.iter (print_val venv') senv';
       if cfg.verbose_level > 1 then begin
         List.iter print_box boxes'';
         List.iter print_wire wires''
         end;
       ctx.senv <- senv' @ ctx.senv;
       ctx.boxes <- ctx.boxes @ boxes'';
       ctx.wires <- ctx.wires @ wires''
    | Directive ("print_tenv", _) -> Typing.dump_typing_environment "Typing environment" (ctx.tenv.te_cons,[])
    | Directive ("print_senv", _) -> Static.dump_static_env "Static environment" ctx.venv ctx.senv
    | Directive ("dump_dot", _) ->
       cfg.dot_output <- not cfg.dot_output;
       if cfg.dot_output && cfg.verbose_level > 0 then Printf.printf "> Now dumping to file %s...\n" cfg.dot_file
    | Directive ("dot_file", DA_String fname) ->
       cfg.dot_file <- fname
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
    | Directive ("verbose", DA_Int l) -> 
       if l >= 0 && l <= 2 then
         cfg.verbose_level <- l
       else
         Printf.printf "* Invalid verbose level (should be 0..2)\n"
    | Directive ("use", DA_String fname) ->
       if ctx.in_use then
         raise NestedUse
       else begin
           if cfg.verbose_level > 0 then Printf.printf "> Reading file %s\n" fname;
           let ic = open_in fname in
           let lexbuf = Lexing.from_channel ic in
           Location.input_chan := ic;
           Location.input_lexbuf := lexbuf;
           ctx.lexbuf <- lexbuf;
           ctx.in_use <- true
         end
    | Directive (s,_) -> raise (Invalid_directive s)
    | EoF ->
       if ctx.in_use then begin
         ctx.lexbuf <- Lexing.from_channel stdin;
         close_in !Location.input_chan;
         Location.input_chan := stdin;
         Location.input_lexbuf := ctx.lexbuf;
         ctx.in_use <- false
         end
       else
         raise End_of_file

let run () =
  let lexbuf = Lexing.from_channel stdin in
  let ctx = 
    if Options.cfg.Options.builtins then
      { tenv = { te_cons = fst Builtins.typing_env; te_vars = []};
        venv = snd Builtins.typing_env;
        senv = Builtins.static_env;
        boxes = [];
        wires = [];
        in_use = false;
        lexbuf = lexbuf }
    else
      { tenv = { te_cons = []; te_vars = [] };
        venv = [];
        senv = [];
        boxes = [];
        wires = [];
        in_use = false;
        lexbuf = lexbuf  } in
  Location.input_lexbuf := ctx.lexbuf;
  print_banner ();
  while true do
    try
      print_string "# "; flush stdout;
      compile_phrase ctx;
      if cfg.dot_output then dump_graph ctx
    with
    | Parser.Error -> Compiler.syntax_error ()
    | Lexer.Lexical_error (errcode,pos1,pos2) -> Compiler.lexical_error (errcode,pos1,pos2)
    | Misc.Error -> flush stderr
    | Invalid_directive s -> Printf.printf "Unknown directive: #%s\n" s; flush stdout
    | NestedUse -> Printf.printf "Not implemented: nested #use directive\n"; flush stdout
    | e -> raise e
  done
  
