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
open Typing
open Static

exception Error of string
                 
type sc_config = {
  mutable sc_act_headers: string list;
  mutable sc_top_headers: string list;
  mutable sc_bcasters_suffix: string;
  mutable sc_trace: bool;
  mutable sc_dump_fifos: bool;
  mutable sc_trace_fifos: bool;
  mutable sc_dump_fifo_stats: bool;
  mutable sc_fifo_stats_file: string;
  mutable sc_graph_suffix: string;
  mutable sc_actor_suffix: string;
  mutable sc_param_suffix: string;
  mutable sc_data_input_module_name: string;
  mutable sc_data_output_module_name: string;
  mutable sc_param_input_module_name: string;
  mutable sc_mod_clock: string;
  mutable sc_clock_period_ns: int;
  mutable sc_data_fifo_capacity: int;
  mutable sc_param_fifo_capacity: int;
  mutable sc_data_file_suffix: string;
  mutable sc_default_io_rate: int;
  mutable sc_stop_time: int;
  (* mutable sc_stop_idle_time: int; (\* TO BE IMPLEMENTED (cf corresp. module in Caph *\) *)
  }

let cfg = {
  sc_act_headers = [
    "<systemc.h>";
    "\"hocl.h\"" ];
  sc_top_headers = [
    "<systemc.h>";
    "<iostream>";
    "\"stream_in.h\"";
    "\"stream_out.h\"";
    "\"param_in.h\"";
    "\"bcast.h\"" ];
  sc_bcasters_suffix = "_bcasters";
  sc_trace = false;
  sc_dump_fifos = false;
  sc_trace_fifos = false;
  sc_dump_fifo_stats = false;
  sc_fifo_stats_file = "fifo_stats.dat";
  sc_actor_suffix = "_act";
  sc_graph_suffix = "_gph";
  sc_param_suffix = "_prm";
  sc_mod_clock = "clk";
  sc_data_input_module_name = "stream_in";
  sc_data_output_module_name = "stream_out";
  sc_param_input_module_name = "param_in";
  sc_clock_period_ns = 10;
  sc_data_fifo_capacity = 16;
  sc_param_fifo_capacity = 1;
  sc_data_file_suffix = ".dat";
  sc_default_io_rate = 1;
  sc_stop_time = 0;
  (* sc_stop_idle_time = 0; *)
}

let dump_banner oc = Misc.dump_banner "//" oc

let localize_id s = "_" ^ s

(* Types *)

let rec string_of_type t  = match real_type t with (* TO REFINE *)
  | TyConstr("int", []) -> "int"
  | TyConstr(name, []) -> name
  (* | Tconstr({tc_name="array"}, [ty], [sz]) -> 
   *     "std::array" ^ "<" ^ string_of_type ty ^  "," ^ string_of_size sz ^ ">" *)
  | ty -> Misc.not_implemented ("Systemc translation of type " ^ Pr_type.string_of_type t) 

(* let rec string_of_val t v = match v, real_type t with
 *     Expr.Val_int (i,_), _ -> string_of_int i
 *   | Expr.Val_bool b, _ -> string_of_bool b
 *   | Expr.Val_float v, _ -> string_of_float v
 *      failwith ("Systemc.string_of_val: no sensible SystemC representation for value " ^ (Expr.string_of_val v)) *)


let mod_name b =
  match b.b_tag with
  | ActorB | GraphB | EBcastB ->
     String.capitalize_ascii b.b_name
  | LocalParamB | InParamB ->
     "Param" ^ string_of_int b.b_id
  | IBcastB ->
     "Bcast" ^ string_of_int (List.length b.b_outs) ^ "<" ^ string_of_type b.b_typ ^">"
  | _ ->
     Misc.not_implemented "Systemc.mod_name"

let box_name bid b =
  match b.b_tag with
  | ActorB | GraphB | EBcastB | IBcastB | SourceB | SinkB ->
     b.b_name ^ string_of_int bid
  | LocalParamB | InParamB ->
     "param" ^ string_of_int bid
  | _ ->
     Misc.not_implemented "Systemc.box_name"

(* Param and rate expressions *)

let rec string_of_core_expr ?(localize_ids=false) e =
  match e.Syntax.ce_desc with
  | Syntax.EInt n -> string_of_int n
  | Syntax.EBool b -> string_of_bool b
  | Syntax.EVar v -> if localize_ids then localize_id v else v
  | Syntax.EBinop (op, e1, e2) -> string_of_core_expr' ~localize_ids e1 ^ op ^ string_of_core_expr' ~localize_ids e2

and string_of_core_expr' ?(localize_ids=false) e =
  if Syntax.is_simple_core_expr e
  then string_of_core_expr ~localize_ids e
  else "(" ^ string_of_core_expr ~localize_ids e ^ ")"

(* Dumping actor interface and implementation *)

let is_actual_actor_io (id,(ty,_)) = not (is_unit_type ty)

let string_of_io_rate rate = match rate with
  | None -> "1"
  | Some e -> Syntax.string_of_rate_expr e

(* let get_delay_io a =
 *   let open Ssval in 
 *   match a.sa_kind with
 *   | Syntax.A_Delay ->
 *      let ival_param =
 *        match a.sa_params, List.find_opt (fun (id, _, _) -> id = "ival") a.sa_params with
 *        | [], _ -> Error.missing_param "delay" a.sa_id " (should be at least one with name \"ival\")"
 *        | _, None -> Error.missing_ival_param a.sa_id; List.hd (List.rev a.sa_params)
 *        | _, Some p -> p in
 *      let outp = match a.sa_outs with
 *      | [o] -> o
 *      | _ -> Error.illegal_interface "delay" a.sa_id " (should have exactly one output)" in
 *      ival_param, outp 
 *   | _ -> Misc.fatal_error "Systemc.get_delay_io"               *)

let dump_module_intf oc name intf =
  let modname = String.capitalize_ascii name in
  fprintf oc "SC_MODULE(%s) {\n" modname;
  fprintf oc "  sc_in<bool> %s;\n" cfg.sc_mod_clock;
  List.iter
    (function (id,ty) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    intf.t_params;
  List.iter
    (function (id,(ty,_)) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    intf.t_real_ins;
  List.iter
    (function (id,(ty,_)) -> fprintf oc "  sc_fifo_out<%s > %s;\n" (string_of_type ty) id)
    intf.t_real_outs

let get_rate_annot anns = 
  List.find_opt (function Syntax.IA_Rate _ -> true | _ -> false) anns

let get_rate_expr anns =
  match get_rate_annot anns with
  | Some (IA_Rate e) -> Some e
  | _ -> None

let rec dump_actor_intf prefix oc name intf attrs =
  let modname = String.capitalize_ascii name in
  (* let _, _, init_fn =
   *   match a.sa_kind with
   *   | A_Delay -> "", "", ""
   *   | _ -> get_pragma_fns sp a.sa_id in *)
  (* let clocked = inps = [] && a.sa_params = [] || a.sa_kind = A_Delay in  (\* parameter-less, source actors or delay *\) *)
  fprintf oc "#ifndef _%s%s_h\n" name cfg.sc_actor_suffix;
  fprintf oc "#define _%s%s_h\n" name cfg.sc_actor_suffix;
  fprintf oc "\n";
  List.iter (function h -> fprintf oc "#include %s\n" h) cfg.sc_act_headers;
  fprintf oc "\n";
  dump_module_intf oc name intf;
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
  let dump_local_var (id,ty) =
    fprintf oc "    %s %s;\n" (string_of_type ty) (localize_id id) in
  let dump_local_var' (id,(ty,anns)) =
    let open Syntax in
    match get_rate_expr anns with
    | Some {ce_desc=EInt n} -> 
       fprintf oc "    %s %s[%d];\n" (string_of_type ty) (localize_id id) n (* Static allocation *)
    | Some _ ->
       fprintf oc "    %s *%s;\n" (string_of_type ty) (localize_id id) (* Dynamic allocation *)
    | None -> (* No rate specified *)
       fprintf oc "    %s %s[%d];\n" (string_of_type ty) (localize_id id) cfg.sc_default_io_rate in
  List.iter dump_local_var intf.t_params;
  List.iter dump_local_var' intf.t_real_ins;
  List.iter dump_local_var' intf.t_real_outs;
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

let get_impl_fns name attrs =
  match List.assoc_opt "incl_file" attrs, List.assoc_opt "loop_fn" attrs, List.assoc_opt "init_fn" attrs with
  | Some f, Some f', Some f'' -> f, f', f''
  | Some f, Some f', None -> f, f', ""
  | _, _, _ -> Error.incomplete_actor_impl "systemc" name

let rec dump_actor_impl prefix oc name intf attrs =
  let modname = String.capitalize_ascii name in
  let incl_file, loop_fn, init_fn = get_impl_fns name attrs in
    (* match a.sa_kind with
     * | A_Delay -> "", "", ""  (\* The delay actor is builtin *\)
     * | _ -> get_pragma_fns sp a.sa_id in *)
  let params = List.map (function (id,ty) -> id,ty,ParamIn) intf.t_params in
  let dinps = List.map (function (id,(ty,_)) -> id,ty,DataIn) intf.t_real_ins in
  let doutps = List.map (function (id,(ty,_)) -> id,ty,DataOut) intf.t_real_outs in
  let clocked = intf.t_real_ins = [] && params = [] in (* true for parameter-less, source actors *)
  let local_params = List.map (function (id,_,_) -> id, localize_id id) params in
  let localize_rate_expr locals re =
    match re with
    | None -> None
    | Some e -> Some (Syntax.subst_core_expr locals e) in
  let string_of_io_rate' re = string_of_io_rate (localize_rate_expr local_params re) in
  fprintf oc "#include \"%s%s.h\"\n" name cfg.sc_actor_suffix;
  if incl_file <> "" then fprintf oc "#include \"%s\"\n" incl_file;
  fprintf oc "\n" ;
  fprintf oc "void %s::main(void) {\n" modname;
  if init_fn <> "" (* || a.sa_kind = A_Delay *) then begin
      if  params <> [] then begin (* We need to wait for one clk event so that parameter values are available.. *)
        fprintf oc "    wait(%s.posedge_event());\n" cfg.sc_mod_clock;
        List.iter
          (fun (id,_,_) -> fprintf oc "    %s = %s.read();\n" (localize_id id) id)
          params
        end;
      (* if a.sa_kind = A_Delay then begin
       *   let ((id,_,_) as ival_inp), ((id',_,rate,_) as outp) = get_delay_io a in
       *   fprintf oc "    for ( int __k=0; __k<%s; __k++ ) %s.write(%s); // Initial tokens\n"
       *     (string_of_io_rate' rate) id' (localize_id id)
       *   end
       * else *)
        fprintf oc "    %s(%s);\n" init_fn (Misc.string_of_list string_of_io ", " params);
    end;
  fprintf oc "    while ( 1 ) { \n";
  if clocked then
    fprintf oc "      wait(%s.posedge_event());\n" cfg.sc_mod_clock;
  List.iter
    (fun (id,_,_) -> fprintf oc "      %s = %s.read();\n" (localize_id id) id)
    params;
  List.iter  (* Dynamically allocate buffers for non-constant sized IOs *)
    (fun (id,(ty,anns)) ->
      match get_rate_expr anns with
     | Some e as e' when not (Syntax.is_constant_core_expr e) ->
        fprintf oc "      %s = new %s[%s];\n" (localize_id id) (string_of_type ty) (string_of_io_rate' e')
      | _ -> ())
    ((*if a.sa_kind = Syntax.A_Delay then inps else*) intf.t_real_ins @ intf.t_real_outs); 
  List.iter
    (fun (id,(ty,anns)) ->
      let rate = get_rate_expr anns in
      fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s[__k] = %s.read();\n"
        (string_of_io_rate' rate) (localize_id id) id)
    intf.t_real_ins;
  (* if a.sa_kind = Syntax.A_Delay then begin
   *     let iid, oid, orate = match intf.t_real_ins, intf.t_real_outs with
   *       | [id,_,_,_], [id',_,rate,_] -> id, id', rate
   *       | _ -> Error.illegal_interface "delay" a.sa_id " (should have exactly one input and output)" in
   *     fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s.write(%s[__k]);\n"
   *           (string_of_io_rate' orate) oid (localize_id iid)
   *   end
   * else *)
   begin
    fprintf oc "      %s(%s);\n" loop_fn (Misc.string_of_list string_of_io ", " (params @ dinps @ doutps));
    List.iter
      (fun (id,(ty,anns)) ->
        let rate = get_rate_expr anns in
        fprintf oc "      for ( int __k=0; __k<%s; __k++ ) %s.write(%s[__k]);\n"
          (string_of_io_rate' rate) id (localize_id id))
      intf.t_real_outs
    end;
  List.iter  (* Dynamically de-allocate buffers for non-constant sized IOs *)
    (fun (id,(ty,anns)) ->
      let open Syntax in
      match get_rate_expr anns with 
     | Some e  when not (is_constant_core_expr e) ->
        fprintf oc "      delete [] %s;\n" (localize_id id)
     | _ -> ())
    ((*if a.sa_kind = Syntax.A_Delay then intf.t_real_ins else *)intf.t_real_ins @ intf.t_real_outs); 
  fprintf oc "    }\n";
  fprintf oc "}\n"

let dump_actor_component path f modname fname intf attrs =
  let fname' = path ^ fname in
  let oc = open_out fname' in
  dump_banner oc;
  f oc modname intf attrs;
  Logfile.write fname';
  close_out oc

let dump_actor path prefix name intf impl =
  let attrs =
    try List.assoc "systemc" impl
    with Not_found -> Error.missing_actor_impl "systemc" name in
  dump_actor_component path (dump_actor_intf prefix) name (name ^ cfg.sc_actor_suffix ^ ".h") intf attrs;
  dump_actor_component path (dump_actor_impl prefix) name (name ^ cfg.sc_actor_suffix ^ ".cpp") intf attrs
          
(* Dumping parameters *)

let is_actual_param_io (_,(_,ty,_)) = not (is_unit_type ty)

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
    (function (id,(_,ty,ann)) -> fprintf oc "  sc_fifo_in<%s > %s;\n" (string_of_type ty) id)
    inps;
  List.iter
    (function (id,(_,ty,ann)) -> fprintf oc "  sc_fifo_out<%s > %s;\n" (string_of_type ty) id)
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
  let dump_local_var (id,(_,ty,_)) = fprintf oc "    %s %s;\n" (string_of_type ty) (localize_id id) in
  List.iter dump_local_var inps;
  List.iter dump_local_var outps;
  fprintf oc "    // Service\n";
  fprintf oc "    bool trace;\n";
  fprintf oc "    sc_module_name modname;\n";
  fprintf oc "};\n";
  fprintf oc "#endif\n"

let rec dump_param_impl prefix oc modname b =
  let incl_name = b.b_name ^ cfg.sc_param_suffix in
  let inps = List.filter is_actual_param_io b.b_ins in
  fprintf oc "#include \"%s.h\"\n" incl_name;
  fprintf oc "\n" ;
  fprintf oc "void %s::main(void) {\n" modname;
  fprintf oc "    while ( 1 ) { \n";
  if inps = [] then begin (* Constant/source/non dep parameter *)
    fprintf oc "      _o = %s;\n" (string_of_core_expr ~localize_ids:true b.b_val.bv_lit);
    List.iter (fun (id,_) -> fprintf oc "      %s.write(%s);\n" id (localize_id id)) b.b_outs; (* Constant *)
    fprintf oc "      wait(%s.posedge_event());\n" cfg.sc_mod_clock
    end
  else begin
      List.iter
        (fun (id,_) ->
          let id' = localize_id id in
          fprintf oc "      %s = %s.read();\n" id' id;
          fprintf oc "      if ( trace ) cout << modname << \" read \" << %s << \" at \" << sc_time_stamp() << endl;\n" id')
        b.b_ins;
    fprintf oc "      _o = %s;\n" (string_of_core_expr ~localize_ids:true b.b_val.bv_lit);
    List.iter
      (fun (id,_) ->
        let id' = localize_id id in
        fprintf oc "      %s.write(%s);\n" id (localize_id id);
        fprintf oc "      if ( trace ) cout << modname << \" wrote \" << %s << \" at \" << sc_time_stamp() << endl;\n" id')
      b.b_outs
    end;
  fprintf oc "    }\n";
  fprintf oc "}\n" 

let dump_param_component dir f modname fname m =
  let fname' = dir ^ "/" ^ fname in
  let oc = open_out fname' in
  dump_banner oc;
  f oc modname m;
  Logfile.write fname';
  close_out oc

let dump_parameter dir prefix gid (bid,b) =
  match b.b_tag with
  | LocalParamB -> 
     let file_pfx = b.b_name ^ cfg.sc_param_suffix in
     let modname = "Param" ^ string_of_int bid in
     dump_param_component dir (dump_param_intf prefix) modname (file_pfx ^ ".h") b;
     dump_param_component dir (dump_param_impl prefix) modname (file_pfx ^ ".cpp") b
  | _ ->
     ()
  
let dump_parameters path prefix gid g =
  List.iter (dump_parameter path prefix gid) g.sg_boxes

let dump_top_parameters path prefix (id,g) = dump_parameters path prefix id g.tg_impl

let dump_node_parameters path prefix (id,n) =
  match n.sn_impl with
  | NI_Graph g -> dump_parameters path prefix id (*n.sn_intf*) g
  | NI_Actor _ -> ()

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

(* Dumping graphs *)

let rec dump_graph ~toplevel path prefix id intf g =
  let modname = String.capitalize_ascii id in
  let fname = path ^ id ^ cfg.sc_graph_suffix ^ ".h" in
  let oc = open_out fname in
  let f_name b =
    match b.b_tag with
    | ActorB | EBcastB -> "\"" ^ b.b_name ^ cfg.sc_actor_suffix ^ ".h" ^ "\""
    | LocalParamB -> "\"" ^ b.b_name ^ cfg.sc_param_suffix ^ ".h" ^ "\""
    | GraphB -> "\"" ^ b.b_name ^ cfg.sc_graph_suffix ^ ".h" ^ "\"" 
    | _ -> "" in
  let headers =
    let add acc f = if f <> "" && not (List.mem f acc) then f::acc else acc in
    List.fold_left (fun acc (bid,b) -> add acc (f_name b)) [] g.sg_boxes in
  let bcasters = [] in
  (* let bcasters = Static.extract_bcast_boxes sp in *)
  (* TO FIX : we have to distinguish "implicit" bcasts, used for parameters and inserted automatically
     with the [-insert_bcasts] option, from the "explicit" bcasts inserted by the programmer btw _actors_.
     Eventually merge the two cases ? *)
  dump_banner oc;
  List.iter (function h -> fprintf oc "#include %s\n" h) (cfg.sc_top_headers @ headers);
  (* if bcasters <> [] then fprintf oc "#include \"%s\"\n"  (prefix ^ cfg.sc_bcasters_suffix ^ ".h"); *)
  fprintf oc "\n";
  dump_module_intf oc modname intf;  
  fprintf oc "\n";
  let sub_boxes = List.filter (fun (_,b) -> not (is_io_box toplevel b)) g.sg_boxes in
  let sub_wires = List.filter (fun (_,w) -> not (is_io_wire g.sg_boxes w)) g.sg_wires in
  List.iter (dump_wire_decl oc) sub_wires;
  fprintf oc "\n";
  List.iter (dump_box_decl oc) sub_boxes;
  fprintf oc "\n";
  fprintf oc "  SC_CTOR(%s) : %s {\n"
    modname
    (Misc.string_of_two_lists string_of_box_inst string_of_wire_inst ", " sub_boxes sub_wires); 
  List.iter (dump_box_inst oc g) sub_boxes;
  fprintf oc "  }\n" ;
  if cfg.sc_trace_fifos then begin
    List.iter
      (function (i,_) -> fprintf oc "  w%d.trace(fifo_trace_file);\n" i)
      (List.filter is_fifo_wire sub_wires);
    end;
  fprintf oc "};\n" ;
  Logfile.write fname;
  close_out oc

and is_fifo_wire (wid,(_,_,kind)) = kind=DataW

and is_io_wire boxes w =  is_src_wire boxes w || is_dst_wire boxes w
and is_src_wire boxes w = match (get_src_box boxes w).b_tag with SourceB | InParamB -> true | _ -> false
and is_dst_wire boxes w = (get_dst_box boxes w).b_tag = SinkB
and is_io_box toplevel b =
  match b.b_tag with
  | SourceB | SinkB -> true
  | InParamB -> not toplevel
  | _ -> false

and dump_wire_decl oc ((wid,(((src,_),(dst,_)),ty,kind)) as w) =
  (* if is_dep_wire then
   *     fprintf oc "  sc_signal<%s > w%d(\"w%d\");\n" (string_of_type ty) wid wid
   * else *)
    fprintf oc "  sc_fifo<%s > w%d;\n" (string_of_type ty) wid

and dump_box_decl oc (bid,b) =
  match b.b_tag with
  | ActorB | GraphB | EBcastB | IBcastB | LocalParamB | InParamB ->
     fprintf oc "  %s %s;\n" (mod_name b) (box_name bid b)
  | _ ->
     ()

and string_of_box_inst (i,b) = 
  (* match bx.b_tag with
   * | LocalParamB ->
   *    sprintf "%s(\"%s\")" name name (string_of_core_expr bx.b_val.bv_lit);
   * | _ -> *)
  let name = box_name i b in
  sprintf "%s(\"%s\")" name name
  
and dump_box_inst oc g (i,b) =
  let name = box_name i b in
  match b.b_tag with
  | ActorB | GraphB | EBcastB ->
      fprintf oc "    %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc g name) b.b_ins;
      List.iter (dump_box_output oc g name) b.b_outs
  | IBcastB ->
      List.iter (dump_box_input oc g name) b.b_ins;
      List.iter (dump_box_output oc g name) b.b_outs
  | LocalParamB | InParamB ->
      if b.b_ins = [] then (* Initial (non dep) parameter *)
        fprintf oc "    %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock;
     List.iter (dump_box_input oc g name) b.b_ins;
     List.iter (dump_box_output oc g name) b.b_outs
  | SourceB | SinkB ->
     () 
  | DummyB ->
     Misc.fatal_error "Systemc.dump_box_inst: dummy box"

and dump_box_input oc g bname ((id,(wid,ty,ann) as w)) =
  fprintf oc "    %s.%s(%s);\n" bname id (iwire_name g wid)

and dump_box_output oc g bname (id,(wids,ty,ann)) =
   List.iter (function wid -> fprintf oc "    %s.%s(%s);\n" bname id (owire_name g wid)) wids

(* and iwire_name g wid =
 *   let w = find_wire g.sg_wires wid in
 *   match get_src_box g.sg_boxes w with
 *   | { b_tag=SourceB; b_name=id } -> id
 *   | _ -> "w" ^ string_of_int wid
 * 
 * and owire_name g wid =
 *   let w = find_wire g.sg_wires wid in
 *   match get_dst_box g.sg_boxes w with
 *   | { b_tag=SinkB; b_name=id } -> id
 *   | _ -> "w" ^ string_of_int wid *)

and wire_name get tags g wid = 
    let w = find_wire g.sg_wires wid in
    match get g.sg_boxes w with
    | { b_tag=tag'; b_name=id } when List.mem tag' tags -> id
    | _ -> "w" ^ string_of_int wid
and iwire_name g wid = wire_name get_src_box [SourceB;InParamB] g wid
and owire_name g wid = wire_name get_dst_box [SinkB] g wid

and string_of_wire_inst (wid,(_,_,kind)) = 
  let cap = match kind with DataW -> cfg.sc_data_fifo_capacity | ParamW -> cfg.sc_param_fifo_capacity in
  sprintf "w%d(\"w%d\",%d)" wid wid cap
    
(* and dump_box oc (i,b) =
 *   let bname = b.b_name in
 *   let type_of ios = match ios with 
 *       | (_,(_,ty,_))::_ -> ty
 *       | _ -> failwith "Systemc.dump_box" (\* should not happen *\) in
 *   match b.b_tag with
 *   | IBcastB ->  (\* Implicit bcast *\)
 *       let ty = type_of b.b_ins in
 *       fprintf oc "  %s<%s > %s(\"%s\");\n"
 *         ("bcast" ^ string_of_int (List.length b.b_outs))
 *         (string_of_type ty)
 *         bname
 *         bname;
 *       List.iter (dump_box_input oc bname) b.b_ins;
 *       List.iter (dump_box_output oc bname) b.b_outs
 *   (\* | ActorB when is_bcast_box sp.boxes i -> (\\* Explicit bcast (for actors, for now) *\\) (\\* TO BE FIXED ? *\\)
 *    *     let ty = type_of b.b_ins in
 *    *     fprintf oc "  %s<%s > %s(\"%s\", %b);\n"
 *    *       ("bcast" ^ string_of_int (List.length b.b_outs))
 *    *       (string_of_type ty)
 *    *       bname
 *    *       bname
 *    *       (\\* (string_of_param_values ir bname b.ib_params) *\\)
 *    *       cfg.sc_trace;
 *    *     fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
 *    *     List.iter (dump_box_input oc bname) b.b_ins;
 *    *     List.iter (dump_box_output oc bname) b.b_outs *\)
 *   | ActorB
 *   | GraphB
 *   | EBcastB ->
 *     (\* | DelayB -> *\)
 *       (\* let modname = bname ^ cfg.sc_actor_suffix in *\)
 *      let modname = mod_name bname in
 *      fprintf oc "  %s %s(\"%s\", %b);\n"
 *         modname
 *         bname
 *         bname
 *         (\* (string_of_param_values ir bname b.ib_params)
 *          * (string_of_var_init_values bname b.ib_vars) *\)
 *         cfg.sc_trace;
 *       (\* if b.b_ins = [] then (\\* Source actor *\\) *\)
 *       fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
 *       List.iter (dump_box_input oc bname) b.b_ins;
 *       List.iter (dump_box_output oc bname) b.b_outs
 *   | LocalParamB ->
 *       let modname = bname ^ cfg.sc_param_suffix in
 *       fprintf oc "  %s %s(\"%s\");\n"
 *         modname
 *         bname
 *         bname;
 *         (\* (string_of_param_values ir bname b.ib_params) *\)
 *       if b.b_ins = [] then (\* Initial (non dep) parameter *\)
 *         fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
 *       List.iter (dump_box_input oc bname) b.b_ins;
 *       List.iter (dump_box_output oc bname) b.b_outs
 *   | DummyB ->  Misc.fatal_error "Systemc.dump_box: dummy box"
 *   | _ ->  Misc.fatal_error "Systemc.dump_box: not implemented box kind" *)

(* Printing node description file(s) *)

let dump_node path prefix (id,n) =
  match n.sn_impl with
  | NI_Actor a -> dump_actor path prefix id n.sn_intf a
  | NI_Graph g -> dump_graph ~toplevel:false path prefix id n.sn_intf g
                  
(* Printing top level .cpp file *)

let rec dump_main_ios oc (id,g) = 
  List.iter
    (fun (bid,b) ->
      match b.b_tag with
      | SourceB -> dump_main_input oc id b
      | SinkB -> dump_main_output oc id b
      | InParamB -> dump_main_param oc id b
      | _ -> ())
    g.tg_impl.sg_boxes

and dump_main_param oc pfx b = 
  let name, wid, ty = match b.b_outs with
      [id,([w],ty,_)] -> pfx ^ "_" ^ b.b_name, w, string_of_type ty
    | _ -> Misc.not_implemented ("Systemc backend: multiply connected input parameter: " ^ pfx ^ "." ^ b.b_name) in
  fprintf oc "  sc_fifo<%s> w%d(\"w%d\",%d);\n" ty wid wid cfg.sc_param_fifo_capacity;
  fprintf oc "  %s<%s> %s(\"%s\",%s,%b);\n"
    cfg.sc_param_input_module_name ty name name (string_of_core_expr b.b_val.bv_lit) cfg.sc_trace;
  fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock;
  fprintf oc "  %s.o(w%d);\n\n" name wid

and dump_main_input oc pfx b = 
  let name, wid, ty = match b.b_outs with
      [id,([w],ty,_)] -> pfx ^ "_" ^ b.b_name, w, string_of_type ty
    | _ -> Misc.fatal_error ("Systemc.dump_main_input: multiply connected input: " ^ pfx ^ "." ^ b.b_name) in
  fprintf oc "  sc_fifo<%s> w%d(\"w%d\",%d);\n" ty wid wid cfg.sc_data_fifo_capacity;
  fprintf oc "  %s<%s> %s(\"%s\", \"%s%s\", %b);\n"
    cfg.sc_data_input_module_name ty name name name cfg.sc_data_file_suffix cfg.sc_trace;
  fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock;
  fprintf oc "  %s.out(w%d);\n\n" name wid

and dump_main_output oc pfx b = 
  let name, wid, ty = match b.b_ins with
      [id,(w,ty,_)] -> pfx ^ "_" ^ b.b_name, w, string_of_type ty
    | _ -> Misc.fatal_error ("Systemc.dump_main_output: multiply connected output: " ^ pfx ^ "." ^ b.b_name) in
  fprintf oc "  sc_fifo<%s> w%d(\"w%d\",%d);\n" ty wid wid cfg.sc_data_fifo_capacity;
  fprintf oc "  %s<%s> %s(\"%s\", \"%s%s\", %b);\n"
    cfg.sc_data_output_module_name ty name name name cfg.sc_data_file_suffix cfg.sc_trace;
  fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock;
  fprintf oc "  %s.inp(w%d);\n\n" name wid

let rec dump_main_ios' oc (id,g) = 
  List.iter
    (fun (bid,b) ->
      match b.b_tag with
      | SourceB -> dump_main_input' oc id b
      | SinkB -> dump_main_output' oc id b
      | InParamB -> dump_main_param' oc id b
      | _ -> ())
    g.tg_impl.sg_boxes

and dump_main_param' oc pfx b = 
  let name, id, wid = match b.b_outs with
      [id,([w],ty,_)] -> pfx, b.b_name, w
    | _ -> Misc.not_implemented ("Systemc backend: multiply connected input parameter: " ^ pfx ^ "." ^ b.b_name) in
  (* fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock; *)
  fprintf oc "  %s.%s(w%d);\n" name id wid

and dump_main_input' oc pfx b = 
  let name, id, wid = match b.b_outs with
      [id,([w],ty,_)] -> pfx, id, w
    | _ -> Misc.fatal_error ("Systemc.dump_main_input': multiply connected input: " ^ pfx ^ "." ^ b.b_name) in
  (* fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock; *)
  fprintf oc "  %s.%s(w%d);\n" name id wid

and dump_main_output' oc pfx b = 
  let name, id, wid = match b.b_ins with
      [id,(w,ty,_)] -> pfx, id, w
    | _ -> Misc.fatal_error ("Systemc.dump_main_output: multiply connected output: " ^ pfx ^ "." ^ b.b_name) in
  (* fprintf oc "  %s.%s(%s);\n" name cfg.sc_mod_clock cfg.sc_mod_clock; *)
  fprintf oc "  %s.%s(w%d);\n" name id wid

let dump_main_module path prefix sp =
  let fname = path ^ prefix ^ ".cpp" in
  let oc = open_out fname in
  let header_name (id,_) = "\"" ^ id ^ cfg.sc_graph_suffix ^ ".h\"" in
  let headers = List.map header_name sp.sp_graphs in
  dump_banner oc;
  List.iter (function h -> fprintf oc "#include %s\n" h) (cfg.sc_top_headers @ headers);
  fprintf oc "\n";
  fprintf oc "int sc_main(int argc, char* argv[]) {\n";
  fprintf oc "  sc_clock %s(\"%s\", %d, SC_NS, 0.5);\n" cfg.sc_mod_clock cfg.sc_mod_clock cfg.sc_clock_period_ns;
  fprintf oc "\n";
  (* Dummping toplevel IOs *)
  List.iter (dump_main_ios oc) sp.sp_graphs; 
  (* Dummping subgraphs *)
  List.iter
    (fun (id, g) ->
      fprintf oc "  %s %s(\"%s\");\n" (String.capitalize_ascii id) id id;
      fprintf oc "  %s.%s(%s);\n" id cfg.sc_mod_clock cfg.sc_mod_clock;
      dump_main_ios' oc (id,g))
    sp.sp_graphs;
  fprintf oc "\n";
  if cfg.sc_dump_fifo_stats then
    fprintf oc "  ofstream fifo_stat_file (\"%s\");\n" cfg.sc_fifo_stats_file;
  if cfg.sc_stop_time > 0 then begin
    fprintf oc "  sc_start(%d, SC_NS);\n" cfg.sc_stop_time;
    fprintf oc "  cout << \"Simulation stopped at t=\" << sc_time_stamp() << \"\\n\";\n";
    end
  else
    fprintf oc "  sc_start();\n";
  if cfg.sc_dump_fifo_stats then begin
    fprintf oc "  fifo_stat_file.close();\n";
    fprintf oc "  cout << \"Wrote file %s\" << endl;\n" cfg.sc_fifo_stats_file
    end;
  if cfg.sc_trace_fifos then begin
    fprintf oc "  sc_close_vcd_trace_file (fifo_trace_file);\n";
    fprintf oc "  cout << \"Wrote file %s_fifos.vcd\" << endl;\n" prefix
    end;
  fprintf oc "  return EXIT_SUCCESS;\n";
  fprintf oc "}\n" ;
  Logfile.write fname;
  close_out oc

(* Dumping toplevel graph(s) *)

let dump_top_graph path prefix (id,g) = dump_graph ~toplevel:false path prefix id g.tg_intf g.tg_impl

(* Dumping program *)
  
let dump path prefix sp =
  (* The SystemC backend writes 
     - a file [<prefix>.cpp] containing an instance of each toplevel graph (declared as "graph ...")
       and the global simulation clock
     - for each node toplevel graph ("graph ... end"), a file [<prefix>_<name>.h]
       containing the (hierarchical) graph description  
     - for each node implemented as a subgraph ("node ... struct ... end"), a file [<prefix>_<name>.h]
       containing the (hierarchical) graph description  
     - for each node implemented as an actor ("node ... actor ... end"), a pair of files [<prefix>_<name>.h]
       and [<prefix>_<name>.cpp] containing resp. the interface and the (behavorial) description of the
       actor (with the provided implementation fns embedded in a [SC_THREAD])
     - for each local parameter, a file [<prefix>_<name>.h] containing the module implementing the 
       corresponding behavior (either as a constant generator or a module reading input parameter values
       and computing the resulting parameter value (ex: "k+1", for input "k")  *)
  dump_main_module path prefix sp;
  List.iter (dump_top_graph path prefix) sp.sp_graphs;
  List.iter (dump_node path prefix) sp.sp_nodes;
  List.iter (dump_top_parameters path prefix) sp.sp_graphs;
  List.iter (dump_node_parameters path prefix) sp.sp_nodes
