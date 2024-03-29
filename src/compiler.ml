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

(* Batch compiler *)
   
open Printf
open Misc
open Location
open Typing

let print_banner () = 
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  Printf.printf "This is the HoCL compiler, version %s\n" Version.version;
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  flush stdout

let parse fname = 
  let ic = open_in_bin fname in
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  Parser.program Lexer.main !Location.input_lexbuf

let compile p = 
  let tenv, senv =
    if Options.cfg.Options.builtins then Builtins.typing_env, Builtins.static_env
    else ([],[]),[] in
  let tp = type_program tenv p in
  if Options.cfg.dump_typed then dump_typed_program tp;
  Static.build_static senv p

let mk_fname pfx sfx = Options.cfg.target_dir ^ "/" ^ pfx ^ sfx

let output pfx ir = 
  let path = Options.cfg.target_dir ^ "/"  in
  match Options.cfg.output_fmt with
  | NoOutput -> ()
  | Dot ->
     Dot.dump path pfx ir
  | Systemc ->
      Systemc.dump path pfx ir
     (* if has_splitters ir then Systemc.dump_split_actors ir; *)
  | Preesm ->
     Preesm.dump path pfx ir
  | Xdf ->
     Xdf.dump path pfx ir
  | Dif ->
     Dif.dump path pfx ir

let insert_bcasts ir = 
  if Interm.cfg.insert_bcasts then
    let open Semval in
    let after_boxes = match Options.cfg.output_fmt with
      | Dot -> [LocalParamB; InParamB; SourceB; ActorB (*; DelayB*)]
      | Systemc -> [LocalParamB; InParamB; SourceB; ActorB (*; DelayB*)]
      | Preesm -> [SourceB; ActorB (*; DelayB*)]
      | _ -> [] in
    Interm.insert_bcasts after_boxes ir
  else
    ir

let process_file p f =
  Printf.printf "Parsing file %s\n" f; flush stdout;
  let p' = parse f in
  Syntax.add_program p p'
  
let process_files fs =
  let p = List.fold_left process_file Syntax.empty_program fs in
  (* Syntax.dump_program p; *)
  let ir = p |> compile |> insert_bcasts in
  if Options.cfg.dump_ir then Interm.dump_ir ~typed:true ir;
  if Options.cfg.output_fmt <> NoOutput then begin
      check_dir Options.cfg.target_dir;
      let pfx = Misc.file_prefix @@ List.hd @@ List.rev fs in
      output pfx ir
      end

let syntax_error () = 
  let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
  let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
  eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
  flush stderr

let lexical_error (errcode,pos1,pos2) = 
  let l = Loc(!input_name,pos1, pos2) in
  begin match errcode with
    Lexer.Illegal_character ->
     eprintf "%aIllegal character.\n" output_location l
  | Lexer.Unterminated_string ->
     eprintf "%aUnterminated string.\n" output_location l
  | Lexer.Unterminated_comment ->
     eprintf "%aUnterminated comment.\n" output_location l
  end;
  flush stderr

let run () =
  try
    print_banner ();
    if Options.cfg.builtins && Options.cfg.dump_tenv then
      Typing.dump_typing_environment "Builtin typing environment" Builtins.typing_env;
    Logfile.start ();
    let sfs = match Options.cfg.stdlib with
      | "none" -> !source_files
      | "" -> Version.stdlib :: !source_files
      | f -> f :: !source_files in
    process_files sfs;
    Logfile.stop ()
  with
    Parser.Error -> syntax_error (); exit 1
  | Lexer.Lexical_error (errcode,pos1,pos2) -> lexical_error (errcode,pos1,pos2); exit 2
  | e -> raise e
