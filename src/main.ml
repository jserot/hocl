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

open Printf
open Misc
open Location
open Builtins
open Typing
open Static

exception Toplevel
 
let usage = "usage: hoclc [options...] files"

let anonymous fname = source_files := !source_files @ [fname]

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
  let tp = type_program builtin_typing_env p in
  if Options.cfg.dump_typed then dump_typed_program tp;
  (* if Options.cfg.output_fmt = Systemc then begin
   *     Static.cfg.Static.insert_bcasts <- true;
   *     (\* Static.cfg.Static.insert_fifos <- true *\)
   *     end; *)
  let sp = build_static tp builtin_static_env p in
  if Options.cfg.dump_static then dump_static sp;
  sp

let mk_fname pfx sfx = Options.cfg.target_dir ^ "/" ^ pfx ^ sfx

let output pfx sp = 
  match Options.cfg.output_fmt with
  | NoOutput -> ()
  | Dot ->
     let fname = mk_fname pfx ".dot" in
     Dot.dump pfx sp
  (* | Preesm ->
   *    let fname = mk_fname pfx ".pi" in
   *    Preesm.dump fname sp
   * | Xdf ->
   *    let fname = mk_fname pfx ".xdf" in
   *    Xdf.dump fname sp
   * | Systemc ->
   *    let top_fname = mk_fname pfx "_top.cpp" in
   *    Systemc.dump_top_module Options.cfg.output_prefix top_fname sp;
   *    List.iter (Systemc.dump_actor Options.cfg.target_dir Options.cfg.output_prefix sp) sp.gacts;
   *    List.iter (Systemc.dump_param Options.cfg.target_dir Options.cfg.output_prefix sp) sp.gparams
   *    (\* if has_splitters sp then 
   *     *   Systemc.dump_split_actors sp; *\)
   *    (\* if !Misc.generate_makefiles then Genmake.dump_systemc_makefile () *\) *)

(* let insert_bcasts sp = 
 *   if cfg.insert_bcasts then
 *     let after_boxes = match Options.cfg.output_fmt with
 *       | Dot -> [LocalParamB; InParamB; SourceB; ActorB; DelayB]
 *       | Systemc -> [LocalParamB; InParamB; SourceB; ActorB; DelayB]
 *       | Preesm -> [SourceB; ActorB; DelayB]
 *       | _ -> [] in
 *     Static.insert_bcasters after_boxes sp
 *   else
 *     sp *)

let process_file p f =
  Printf.printf "Parsing file %s\n" f; flush stdout;
  let p' = parse f in
  Syntax.add_program p p'

  
let process_files fs =
  let p = List.fold_left process_file Syntax.empty_program fs in
  (* Syntax.dump_program p; *)
  let sp = p |> compile (* |> insert_bcasts *) in
  if Options.cfg.output_fmt <> NoOutput then begin
      check_dir Options.cfg.target_dir;
      let pfx = Misc.file_prefix @@ List.hd @@ List.rev fs in
      output pfx sp
      end

let main () =
try
  Sys.catch_break true;
  Arg.parse Options.options_spec anonymous usage;
  print_banner ();
  if Options.cfg.dump_tenv then
    dump_global_typing_environment "Initial typing environment" Builtins.builtin_typing_env;
  if Options.cfg.dump_senv then dump_static_environment Builtins.builtin_static_env;
  Logfile.start ();
  let sfs = match Options.cfg.prelude with
    | "" -> !source_files
    | f -> f :: !source_files in
  process_files sfs;
  Logfile.stop ()
with
  Parser.Error ->
    let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
    let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
    eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
    flush stderr; exit 1
| Lexer.Lexical_error(errcode, pos1, pos2) ->
    let l = Loc(!input_name,pos1, pos2) in
    begin match errcode with
      Lexer.Illegal_character ->
        eprintf "%aIllegal character.\n" output_location l
    | Lexer.Unterminated_string ->
        eprintf "%aUnterminated string.\n" output_location l
    | Lexer.Unterminated_comment ->
        eprintf "%aUnterminated comment.\n" output_location l
    end;
    flush stderr; exit 2
(* | Preesm.Error msg ->
 *     eprintf "Error in the Preesm backend: %s.\n" msg;
 *     flush stderr; exit 4
 * | Systemc.Error msg ->
 *     eprintf "Error in the SystemC backend: %s.\n" msg;
 *     flush stderr; exit 4 *)
| End_of_file -> exit 0
| Misc.Error -> exit 1
| Sys.Break -> flush stderr; exit 5
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg;
    flush stderr; exit 6
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 7

let _ = Printexc.print main ()
