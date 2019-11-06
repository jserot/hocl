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

(* SystemC backend *)

open Printf
open Types
open Static

exception Error of string
                 
type sc_config = {
  mutable sc_act_headers: string list;
  mutable sc_top_headers: string list;
  mutable sc_bcasters_suffix: string;
  mutable sc_trace: bool;
  (* mutable sc_dump_fifos: bool;
   * mutable sc_trace_fifos: bool;
   * mutable sc_dump_fifo_stats: bool;
   * mutable sc_fifo_stats_file: string; *)
  mutable sc_act_suffix: string;
  mutable sc_param_suffix: string;
  mutable sc_mod_clock: string;
  mutable sc_clock_period_ns: int;
  mutable sc_data_fifo_capacity: int;
  mutable sc_param_fifo_capacity: int;
  mutable sc_default_io_rate: int;
  mutable sc_stop_time: int;
  (* mutable sc_stop_idle_time: int;
   * mutable sc_tmp_prefix: string;  (\* Prefix for temporary variables in rule actions *\)
   * mutable sc_type_prefix: string;  (\* Prefix for globally defined types *\) *)
  }

let cfg = {
  sc_act_headers = [
    "<systemc.h>";
    "\"hocl.h\"" ];
  sc_top_headers = [
    "<systemc.h>";
    "<iostream>";
    "\"bcast.h\"" ];
  sc_bcasters_suffix = "_bcasters";
  sc_trace = false;
  (* sc_dump_fifos = false;
   * sc_trace_fifos = false;
   * sc_dump_fifo_stats = false;
   * sc_fifo_stats_file = "fifo_stats.dat"; *)
  sc_act_suffix = "_act";
  sc_param_suffix = "_param";
  sc_mod_clock = "clk";
  sc_clock_period_ns = 10;
  sc_data_fifo_capacity = 256;
  sc_param_fifo_capacity = 4;  (* TO FIX: 1 *)
  sc_default_io_rate = 1;
  sc_stop_time = 0;
  (* sc_stop_idle_time = 0;
   * sc_tmp_prefix = "_";
   * sc_type_prefix = "t_"; *)
}

let dump_banner oc = Misc.dump_banner "//" oc

let localize_id s = "_" ^ s

(* Types *)

let rec string_of_type t  = match real_type t with (* TO REFINE *)
  | TyConstr("nat", []) -> "int"
  | TyConstr(name, []) -> name
  (* | Tconstr({tc_name="array"}, [ty], [sz]) -> 
   *     "std::array" ^ "<" ^ string_of_type ty ^  "," ^ string_of_size sz ^ ">" *)
  | ty -> Misc.not_implemented ("Systemc translation of type " ^ Pr_type.string_of_type t) 

(* let rec string_of_val t v = match v, real_type t with
 *     Expr.Val_int (i,_), _ -> string_of_int i
 *   | Expr.Val_bool b, _ -> string_of_bool b
 *   | Expr.Val_float v, _ -> string_of_float v
 *      failwith ("Systemc.string_of_val: no sensible SystemC representation for value " ^ (Expr.string_of_val v)) *)

(* Param and rate expressions *)

let rec string_of_core_expr ?(localize_ids=false) e =
  match e.Syntax.ce_desc with
    Syntax.EConst n -> string_of_int n
  | Syntax.EVar v -> if localize_ids then localize_id v else v
  | Syntax.EBinop (op, e1, e2) -> string_of_core_expr' ~localize_ids e1 ^ op ^ string_of_core_expr' ~localize_ids e2

and string_of_core_expr' ?(localize_ids=false) e =
  if Syntax.is_simple_core_expr e
  then string_of_core_expr ~localize_ids e
  else "(" ^ string_of_core_expr ~localize_ids e ^ ")"

(* Dumping actor interface and implementation *)

let is_actual_actor_io (id,ty,_,_) = not (is_unit_type ty)

let string_of_io_rate rate = match rate with
  | None -> "1"
  | Some e -> Syntax.string_of_rate_expr e

let get_pragma_fns sp id =
    match get_pragma_desc "code" id sp with 
    | [incl_file; loop_fn] -> incl_file, loop_fn, ""
    | [incl_file; loop_fn; init_fn] -> incl_file, loop_fn, init_fn
    | _ -> Error.no_pragma_desc id; id ^ ".h", id, ""

let get_delay_io a =
  let open Ssval in 
  match a.sa_kind with
  | Syntax.A_Delay ->
     let ival_param =
       match a.sa_params, List.find_opt (fun (id, _, _) -> id = "ival") a.sa_params with
       | [], _ -> Error.missing_param "delay" a.sa_id " (should be at least one with name \"ival\")"
       | _, None -> Error.missing_ival_param a.sa_id; List.hd (List.rev a.sa_params)
       | _, Some p -> p in
     let outp = match a.sa_outs with
     | [o] -> o
     | _ -> Error.illegal_interface "delay" a.sa_id " (should have exactly one output)" in
     ival_param, outp 
  | _ -> Misc.fatal_error "Systemc.get_delay_io"
             
let rec dump_actor_intf sp prefix oc modname a =
  let open Ssval in
  let inps = List.filter is_actual_actor_io a.sa_ins in
  let outps = List.filter is_actual_actor_io a.sa_outs in
  (* let _, _, init_fn =
   *   match a.sa_kind with
   *   | A_Delay -> "", "", ""
   *   | _ -> get_pragma_fns sp a.sa_id in *)
  (* let clocked = inps = [] && a.sa_params = [] || a.sa_kind = A_Delay in  (\* parameter-less, source actors or delay *\) *)
  fprintf oc "#ifndef _%s_h\n" modname;
  fprintf oc "#define _%s_h\n" modname;
  fprintf oc "\n";
  List.iter (function h -> fprintf oc "#include %s\n" h) cfg.sc_act_headers;
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s) {\n" modname;
  fprintf oc "  sc_in<bool> %s;\n" cfg.sc_mod_clock;
  List.iter
    (function (id,ty,_) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    a.sa_params;
  List.iter
    (function (id,ty,_,_) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    inps;
  List.iter
    (function (id,ty,_,_) -> fprintf oc "  sc_fifo_out<%s > %s;\n" (string_of_type ty) id)
    outps;
  fprintf oc "\n";
  fprintf oc "  void main(void);\n";
  fprintf oc "\n";
  fprintf oc "  SC_HAS_PROCESS(%s);\n" modname;
  fprintf oc "\n";
  fprintf oc "  %s(sc_module_name name_" modname;
  fprintf oc ", bool trace_=%b " cfg.sc_trace;
  fprintf oc " ) :\n";
  fprintf oc "    modname(name_), sc_module(name_)";
  fprintf oc ", trace(trace_) ";
  fprintf oc "\n";
  fprintf oc "  {\n";
  fprintf oc "    SC_THREAD(main);\n";
  (* if need_clk then
   *   fprintf oc "    sensitive << %s.pos();\n" cfg.sc_mod_clock; *)
  fprintf oc "  }\n";
  fprintf oc "\n";
  fprintf oc "  ~%s() { }\n" modname;
  fprintf oc "\n";
  fprintf oc "  private:\n";
  fprintf oc "    // Local variables\n";
  let dump_local_var (id,ty,_) =
    fprintf oc "    %s %s;\n" (string_of_type ty) (localize_id id) in
  let dump_local_var' (id,ty,rate,ann) =
    let open Syntax in
    match rate with
    | None -> (* No rate specified *)
       fprintf oc "    %s %s[%d];\n" (string_of_type ty) (localize_id id) cfg.sc_default_io_rate
    | Some {ce_desc=EConst n} -> 
       fprintf oc "    %s %s[%d];\n" (string_of_type ty) (localize_id id) n (* Static allocation *)
    | Some _ ->
       fprintf oc "    %s *%s;\n" (string_of_type ty) (localize_id id) in (* Dynamic allocation *)
  List.iter dump_local_var a.sa_params;
  List.iter dump_local_var' (List.filter is_actual_actor_io a.sa_ins);
  List.iter dump_local_var' (List.filter is_actual_actor_io a.sa_outs);
  fprintf oc "    // Service\n";
  fprintf oc "    bool trace;\n";
  fprintf oc "    sc_module_name modname;\n";
  fprintf oc "};\n";
  fprintf oc "#endif\n"

type io_kind = ParamIn | DataIn | DataOut
                                           
let string_of_io (id,ty,kind) = localize_id id
  (* match kind with
   * | ParamIn -> localize_id id
   * | DataIn -> "&" ^ localize_id id
   * | DataOut -> "&" ^ localize_id id *)

let rec dump_actor_impl sp prefix oc name a =
  let open Ssval in
  let modname = name in 
  let incl_file, loop_fn, init_fn =
    match a.sa_kind with
    | A_Delay -> "", "", ""  (* The delay actor is builtin *)
    | _ -> get_pragma_fns sp a.sa_id in
  let inps = List.filter is_actual_actor_io a.sa_ins in
  let outps = List.filter is_actual_actor_io a.sa_outs in
  let params = List.map (function (id,ty,_) -> id,ty,ParamIn) a.sa_params in
  let dinps = List.map (function (id,ty,_,_) -> id,ty,DataIn) inps in
  let doutps = List.map (function (id,ty,_,_) -> id,ty,DataOut) outps in
  let clocked = inps = [] && params = [] in (* true for parameter-less, source actors *)
  let local_params = List.map (function (id,_,_) -> id, localize_id id) params in
  let localize_rate_expr locals re =
    match re with
    | None -> None
    | Some e -> Some (Syntax.subst_rate_expr locals e) in
  let string_of_io_rate' re = string_of_io_rate (localize_rate_expr local_params re) in
  fprintf oc "#include \"%s.h\"\n" modname;
  if incl_file <> "" then fprintf oc "#include \"%s\"\n" incl_file;
  fprintf oc "\n" ;
  fprintf oc "void %s::main(void) {\n" modname;
  if init_fn <> "" || a.sa_kind = A_Delay then begin
      if  params <> [] then begin (* We need to wait for one clk event so that parameter values are available.. *)
        fprintf oc "    wait(%s.posedge_event());\n" cfg.sc_mod_clock;
        List.iter
          (fun (id,_,_) -> fprintf oc "    %s = %s.read();\n" (localize_id id) id)
          params
        end;
      if a.sa_kind = A_Delay then begin
        let ((id,_,_) as ival_inp), ((id',_,rate,_) as outp) = get_delay_io a in
        fprintf oc "    for ( int __k=0; __k<%s; __k++ ) %s.write(%s); // Initial tokens\n"
          (string_of_io_rate' rate) id' (localize_id id)
        end
      else
        fprintf oc "    %s(%s);\n" init_fn (Misc.string_of_list string_of_io ", " params);
    end;
  fprintf oc "    while ( 1 ) { \n";
  if clocked then
    fprintf oc "      wait(%s.posedge_event());\n" cfg.sc_mod_clock;
  List.iter
    (fun (id,_,_) -> fprintf oc "      %s = %s.read();\n" (localize_id id) id)
    params;
  List.iter  (* Dynamically allocate buffers for non-constant sized IOs *)
    (fun (id,ty,rate,ann) ->
      let open Syntax in
      match rate with 
     | Some e when not (is_constant_rate_expr e) ->
        fprintf oc "      %s = new %s[%s];\n" (localize_id id) (string_of_type ty) (string_of_io_rate' rate)
      | _ -> ())
    (if a.sa_kind = Syntax.A_Delay then inps else inps @ outps); 
  List.iter
    (fun (id,_,rate,ann) ->
      fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s[__k] = %s.read();\n"
        (string_of_io_rate' rate) (localize_id id) id)
    inps;
  if a.sa_kind = Syntax.A_Delay then begin
      let iid, oid, orate = match inps, outps with
        | [id,_,_,_], [id',_,rate,_] -> id, id', rate
        | _ -> Error.illegal_interface "delay" a.sa_id " (should have exactly one input and output)" in
      fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s.write(%s[__k]);\n"
            (string_of_io_rate' orate) oid (localize_id iid)
    end
  else begin
    fprintf oc "      %s(%s);\n" loop_fn (Misc.string_of_list string_of_io ", " (params @ dinps @ doutps));
    List.iter
      (fun (id,_,rate,ann) ->
        fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s.write(%s[__k]);\n"
          (string_of_io_rate' rate) id (localize_id id))
      outps
    end;
  List.iter  (* Dynamically de-allocate buffers for non-constant sized IOs *)
    (fun (id,ty,rate,ann) ->
      let open Syntax in
      match rate with 
     | Some e  when not (is_constant_rate_expr e) ->
        fprintf oc "      delete [] %s;\n" (localize_id id)
     | _ -> ())
    (if a.sa_kind = Syntax.A_Delay then inps else inps @ outps); 
  fprintf oc "    }\n";
  fprintf oc "}\n"

let dump_component dir f modname fname m =
  let fname' = dir ^ "/" ^ fname in
  let oc = open_out fname' in
  dump_banner oc;
  f oc modname m;
  Logfile.write fname';
  close_out oc

let dump_actor dir prefix sp (id,act) =
  let a = act.sa_desc in 
  let modname = a.Ssval.sa_id ^ cfg.sc_act_suffix in
  dump_component dir (dump_actor_intf sp prefix) modname (modname ^ ".h") a;
  dump_component dir (dump_actor_impl sp prefix) modname (modname ^ ".cpp") a
          
(* Dumping parameters *)

let is_actual_param_io (_,(_,ty,_,_)) = not (is_unit_type ty)

let rec dump_param_intf prefix oc modname b =
  fprintf oc "#ifndef _%s_h\n" modname;
  fprintf oc "#define _%s_h\n" modname;
  fprintf oc "\n";
  List.iter (function h -> fprintf oc "#include %s\n" h) cfg.sc_act_headers;
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s) {\n" modname;
  let inps = List.filter is_actual_param_io b.b_ins in
  let outps = List.filter is_actual_param_io b.b_outs in
  if inps = [] then (* Initial (non dep) parameter *)
    fprintf oc "  sc_in<bool> %s;\n" cfg.sc_mod_clock;
  List.iter
    (function (id,(_,ty,rate,ann)) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    inps;
  List.iter
    (function (id,(_,ty,rate,ann)) -> fprintf oc "  sc_fifo_out<%s > %s;\n" (string_of_type ty) id)
    outps;
  fprintf oc "\n";
  fprintf oc "  void main(void);\n";
  fprintf oc "\n";
  fprintf oc "  SC_HAS_PROCESS(%s);\n" modname;
  fprintf oc "\n";
  fprintf oc "  %s(sc_module_name name_" modname;
  fprintf oc " ) :\n";
  fprintf oc "    modname(name_), sc_module(name_)";
  fprintf oc "  {\n";
  fprintf oc "    SC_THREAD(main);\n";
  (* if inps = [] then (\* Initial (non dep) parameter *\)
   *     fprintf oc "    sensitive << %s.pos();\n" cfg.sc_mod_clock
   *   end
   * else
   *   begin
   *     fprintf oc "    SC_METHOD(main);\n";
   *     fprintf oc "    sensitive << %s;\n" (Misc.string_of_list (fun (id, _) -> id) " << " inps)
   *   end; *)
  fprintf oc "  }\n";
  fprintf oc "\n";
  fprintf oc "  ~%s() { }\n" modname;
  fprintf oc "\n";
  fprintf oc "  private:\n";
  fprintf oc "    // Local variables\n";
  let dump_local_var (id,(_,ty,_,_)) = fprintf oc "    %s %s;\n" (string_of_type ty) (localize_id id) in
  List.iter dump_local_var inps;
  List.iter dump_local_var outps;
  fprintf oc "    // Service\n";
  fprintf oc "    sc_module_name modname;\n";
  fprintf oc "};\n";
  fprintf oc "#endif\n"

let rec dump_param_impl prefix oc name b =
  let modname = name in 
  let inps = List.filter is_actual_param_io b.b_ins in
  fprintf oc "#include \"%s.h\"\n" modname;
  fprintf oc "\n" ;
  fprintf oc "void %s::main(void) {\n" modname;
  fprintf oc "    while ( 1 ) { \n";
  if inps = [] then (* Initial (non dep) parameter *)
    fprintf oc "      wait(%s.posedge_event());\n" cfg.sc_mod_clock
  else
    List.iter (fun (id,_) -> fprintf oc "      %s = %s.read();\n" (localize_id id) id) b.b_ins;
  fprintf oc "      _o = %s;\n" (string_of_core_expr ~localize_ids:true b.b_val.bv_sub);
  List.iter (fun (id,_) -> fprintf oc "      %s.write(%s);\n" id (localize_id id)) b.b_outs;
  fprintf oc "    }\n";
  fprintf oc "}\n"

let dump_param dir prefix sp (id,b) =
  let modname = b.b_name ^ cfg.sc_param_suffix in
  dump_component dir (dump_param_intf prefix) modname (modname ^ ".h") b;
  dump_component dir (dump_param_impl prefix) modname (modname ^ ".cpp") b
  
(* Printing splitters interface and implementation *)

(* let dump_split_actor oc fanout =
 *   fprintf oc "template <class T>\n";
 *   fprintf oc "SC_MODULE(split%d) { ;\n" fanout;
 *   fprintf oc "  sc_in<bool> clk;\n";
 *   fprintf oc "  sc_port<fifo_in_if<T> > i;\n";
 *   for i=1 to fanout do
 *     fprintf oc "  sc_port<fifo_out_if<T> > o_%d;\n" i
 *   done;
 *   fprintf oc "\n";
 *   fprintf oc "  void main(void);\n\n";
 *   fprintf oc "  SC_HAS_PROCESS(split%d);\n\n" fanout;
 *   fprintf oc "  split%d(sc_module_name name_, bool trace_=false  ) :\n" fanout;
 *   fprintf oc "   modname(name_), sc_module(name_), trace(trace_)\n";
 *   fprintf oc "  {\n";
 *   fprintf oc "    SC_THREAD(main);\n";
 *   fprintf oc "    sensitive << clk.pos();\n";
 *   fprintf oc "  }\n\n";
 *   fprintf oc "  ~split%d() { };\n\n" fanout;
 *   fprintf oc "  private:\n";
 *   fprintf oc "    char *fname;\n";
 *   fprintf oc "    bool trace;\n";
 *   fprintf oc "    sc_module_name modname;\n";
 *   fprintf oc "};\n\n";
 *   fprintf oc "template <class T>\n";
 *   fprintf oc "void split%d<T>::main(void) {\n" fanout;
 *   fprintf oc "    T d;\n";
 *   fprintf oc "    while(1) {\n";
 *   fprintf oc "      wait(); // clk\n";
 *   fprintf oc "      if ( i->rd_rdy() && %s ) {\n"
 *     (Misc.string_of_list
 *        (function i -> "o_" ^ i ^ "->wr_rdy()") " && " (Misc.list_make 1 fanout string_of_int));
 *   fprintf oc "          d = i->read();\n";
 *   fprintf oc "          if ( trace ) cout << modname << \" read \" << d << \" at \" << sc_time_stamp() << endl;\n";
 *   for i=1 to fanout do
 *     fprintf oc "          o_%d->write(d);\n" i
 *   done;
 *   fprintf oc "          if ( trace ) cout << modname << \" wrote \" << d << \" at \" << sc_time_stamp() << endl;\n";
 *   fprintf oc "          };\n";
 *   fprintf oc "      };\n";
 *   fprintf oc "};\n\n"
 * 
 * let dump_split_actors fname fanouts =
 *   let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
 *   let oc = Misc.open_out fname' in
 *   Misc.dump_banner "//" oc;
 *   fprintf oc "#ifndef %s_h\n" cfg.sc_splitters_suffix;
 *   fprintf oc "#define %s_h\n\n" cfg.sc_splitters_suffix;
 *   fprintf oc "#include \"fifo.h\"\n";
 *   fprintf oc "#include <systemc.h>\n\n";
 *   List.iter (dump_split_actor oc) fanouts;
 *   fprintf oc "#endif\n";
 *   close_out oc;
 *   Logfile.write fname' *)

(* Printing of top level .cpp file *)

let rec dump_top_module prefix fname sp =
  (* let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in *)
  (* let oc = Misc.open_out fname' in *)
  let oc = open_out fname in
  let header_name suff (id,_) = "\"" ^ id ^ suff ^ ".h\"" in
  let headers =
      List.map (header_name cfg.sc_act_suffix) sp.gacts
    @ List.map (header_name cfg.sc_param_suffix) sp.gparams in
  let bcasters = [] in
  (* let bcasters = Static.extract_bcast_boxes sp in *)
  (* TO FIX : we have to distinguish "implicit" bcasts, used for parameters and inserted automatically
     with the [-insert_bcasts] option, from the "explicit" bcasts inserted by the programmer btw _actors_.
     Eventually merge the two cases ? *)
  dump_banner oc;
  List.iter (function h -> fprintf oc "#include %s\n" h) (cfg.sc_top_headers @ headers);
  if bcasters <> [] then fprintf oc "#include \"%s\"\n"  (prefix ^ cfg.sc_bcasters_suffix ^ ".h");
  fprintf oc "\n";
  fprintf oc "int sc_main(int argc, char* argv[]) {\n";
  List.iter (dump_wire oc sp.boxes) sp.wires;
  fprintf oc "\n";
  fprintf oc "  sc_clock %s(\"%s\", %d, SC_NS, 0.5);\n" cfg.sc_mod_clock cfg.sc_mod_clock cfg.sc_clock_period_ns;
  fprintf oc "\n";
  (* if cfg.sc_trace_fifos then begin
   *   fprintf oc "  sc_trace_file *fifo_trace_file;\n";
   *   fprintf oc "  fifo_trace_file = sc_create_vcd_trace_file (\"%s_fifos\");\n" prefix; 
   *   fprintf oc "  sc_trace(fifo_trace_file, %s, \"%s\");\n" cfg.sc_mod_clock cfg.sc_mod_clock;
   *   List.iter
   *     (function (i,_) -> fprintf oc "  w%d.trace(fifo_trace_file);\n" i)
   *     (List.filter (is_fifo_wire sp.boxes) sp.wires);
   *   end; *)
  List.iter (dump_box sp oc) sp.boxes;
  fprintf oc "\n";
  if cfg.sc_stop_time > 0 then begin
    fprintf oc "  sc_start(%d, SC_NS);\n" cfg.sc_stop_time;
    fprintf oc "  cout << \"Simulation stopped at t=\" << sc_time_stamp() << \"\\n\";\n";
    end
  else
    fprintf oc "  sc_start();\n";
  (* if cfg.sc_dump_fifo_stats then begin
   *   fprintf oc "  ofstream fifo_stat_file (\"%s\");\n" cfg.sc_fifo_stats_file;
   *   List.iter
   *     (function (i,_) -> fprintf oc "  w%d.dump_stat(fifo_stat_file,%d);\n" i 0)
   *     (List.filter (is_fifo_wire sp.boxes) sp.wires);
   *   fprintf oc "  fifo_stat_file.close();\n";
   *   fprintf oc "  cout << \"Wrote file %s\" << endl;\n" cfg.sc_fifo_stats_file
   *   end; *)
  (* if cfg.sc_trace_fifos then begin
   *   fprintf oc "  sc_close_vcd_trace_file (fifo_trace_file);\n";
   *   fprintf oc "  cout << \"Wrote file %s_fifos.vcd\" << endl;\n" prefix
   *   end; *)
  fprintf oc "  return EXIT_SUCCESS;\n";
  fprintf oc "}\n" ;
  Logfile.write fname;
  close_out oc

and is_fifo_wire boxes (wid,(_,_,kind)) = kind=RegularW

and dump_wire oc boxes (wid,(((src,_),(dst,_)),ty,kind)) =
  (* if is_dep_wire then
   *     fprintf oc "  sc_signal<%s > w%d(\"w%d\");\n" (string_of_type ty) wid wid
   * else *)
  fprintf oc "  sc_fifo<%s > w%d(\"w%d\", %d);\n"
    (string_of_type ty)
    wid
    wid
    (match kind with ParamW -> cfg.sc_param_fifo_capacity | _ -> cfg.sc_data_fifo_capacity)
      (* fprintf oc "  sc_fifo<%s > w%d(\"w%d\", %d, %b, %b);\n"
       *   (string_of_type ty) wid wid cfg.sc_fifo_capacity cfg.sc_dump_fifos cfg.sc_dump_fifo_stats;  *)

and dump_box sp oc (i,b) =
  let bname = box_name sp (i,b) in
  let type_of ios = match ios with 
      | (_,(_,ty,_,_))::_ -> ty
      | _ -> failwith "Systemc.dump_box" (* should not happen *) in
  match b.b_tag with
  | BcastB ->  (* Implicit bcast (for parameters, for now) *)
      let ty = type_of b.b_ins in
      fprintf oc "  %s<%s > %s(\"%s\");\n"
        ("bcast" ^ string_of_int (List.length b.b_outs))
        (string_of_type ty)
        bname
        bname;
      List.iter (dump_box_input oc bname) b.b_ins;
      List.iter (dump_box_output oc bname) b.b_outs
  | ActorB when is_bcast_box sp.boxes i -> (* Explicit bcast (for actors, for now) *) (* TO BE FIXED ? *)
      let ty = type_of b.b_ins in
      fprintf oc "  %s<%s > %s(\"%s\", %b);\n"
        ("bcast" ^ string_of_int (List.length b.b_outs))
        (string_of_type ty)
        bname
        bname
        (* (string_of_param_values ir bname b.ib_params) *)
        cfg.sc_trace;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc bname) b.b_ins;
      List.iter (dump_box_output oc bname) b.b_outs
  | ActorB
  | DelayB ->
      let modname = bname ^ cfg.sc_act_suffix in
      fprintf oc "  %s %s(\"%s\", %b);\n"
        modname
        bname
        bname
        (* (string_of_param_values ir bname b.ib_params)
         * (string_of_var_init_values bname b.ib_vars) *)
        cfg.sc_trace;
      (* if b.b_ins = [] then (\* Source actor *\) *)
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc bname) b.b_ins;
      List.iter (dump_box_output oc bname) b.b_outs
  | LocalParamB ->
      let modname = bname ^ cfg.sc_param_suffix in
      fprintf oc "  %s %s(\"%s\");\n"
        modname
        bname
        bname;
        (* (string_of_param_values ir bname b.ib_params) *)
      if b.b_ins = [] then (* Initial (non dep) parameter *)
        fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc bname) b.b_ins;
      List.iter (dump_box_output oc bname) b.b_outs
  | DummyB ->  Misc.fatal_error "Systemc.dump_box: dummy box"
  | _ ->  Misc.fatal_error "Systemc.dump_box: not implemented box kind"

and dump_box_input oc bname (id,(wid,ty,rate,ann)) =
     fprintf oc "  %s.%s(w%d);\n" bname id wid

and dump_box_output oc bname (id,(wids,ty,rate,ann)) =
   List.iter (function wid -> fprintf oc "  %s.%s(w%d);\n" bname id wid) wids
