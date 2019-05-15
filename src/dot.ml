
open Misc
open Printf
open Ssval
open Static

type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable param_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_annots: bool;
    mutable rank_dir: string
  }

let cfg = {
  labeled_edges = true;
  show_indexes = false;
  param_box_shape = "hexagon";
  slotted_boxes = true;
  show_wire_annots = false;
  rank_dir = "LR"
}

(* and string_of_val v =
 *   let clip s = if String.length s > !max_array_disp_size then "..." else s in
 *   match v with  (\* Special version for DOT since node labels do not support any chars :( *\)
 *     Expr.Val_int (i,_) -> string_of_int i
 *   | Expr.Val_bool b -> string_of_bool b
 *   | Expr.Val_float b -> string_of_float b
 *   | Expr.Val_array1 (n,vs) -> clip (Array1.to_string string_of_val vs)
 *   | Expr.Val_array2 (n,vs) -> clip (Array2.to_string string_of_val vs)
 *   | Expr.Val_array3 (n,vs) -> clip (Array3.to_string string_of_val vs)
 *   | Expr.Val_prim _
 *   | Expr.Val_fun (_,_,_)
 *   | Expr.Val_extern _ -> "(fun)"
 *   | _ -> "?" *)

let output_box ch (i,b) =
  let ioslots c n =
    let rec h = function
        0 -> ""
      | 1 -> "<" ^ c ^ string_of_int (n-1) ^ ">"
      | i -> "<" ^ c ^ string_of_int (n-i) ^ ">|" ^ h (i-1) in
    h n in
  let bid =
    if cfg.show_indexes
    then string_of_int i ^ ":" ^ b.b_name
    else b.b_name in
  match b.b_tag with
  | ActorB ->
      if cfg.slotted_boxes then
        fprintf ch "n%d [shape=record,style=rounded,label=\"<id>%s|{{%s}|{%s}}\"];\n"
          i
          bid
          (ioslots "e" (List.length b.b_ins))
          (ioslots "s" (List.length b.b_outs))
      else
        fprintf ch "n%d [shape=box,style=rounded,label=\"%s\"];\n" i  bid
  | ParamB -> 
     fprintf ch "n%d [shape=%s,label=\"%s\n(%s)\"];\n"
       i
       cfg.param_box_shape
       bid
       (string_of_ssval b.b_val)
  | DummyB ->  (* Should not occur *)
      fprintf ch "n%d [shape=box,style=dotted,label=\"%s\"];\n" i "dummy"

let string_of_wtype ty = ":" ^ Pr_type.string_of_type ty

let output_wire ch (wid,(((s,ss),(d,ds)),ty,is_param_dep))=
  let wire_annot wid =
    (* try Interm.string_of_wire_annot (List.assoc wid wire_annots)
    with Not_found -> *) "" in
  let style = if is_param_dep then "dashed" else "plain" in
  match cfg.labeled_edges, cfg.show_indexes, cfg.show_wire_annots with
  | true, _, true ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" w%d:%s\"; style=%s];\n" s ss d ds wid (wire_annot wid) style
  | true, true, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds ("w" ^ string_of_int wid ^ string_of_wtype ty) style
  | true, false, false ->
     fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"; style=%s];\n" s ss d ds (string_of_wtype ty) style
  | false, _, _ ->
     fprintf ch "n%d:s%d -> n%d:e%d [style=%s];\n" s ss d ds style

let output ch boxes wires = 
  fprintf ch "digraph g {\n";
  fprintf ch "rankdir=%s;\n" cfg.rank_dir;
  List.iter (output_box ch) boxes;
  List.iter (output_wire ch) wires;
  fprintf ch "}\n"

let dump fname sp =
    let oc = open_out fname  in
    output oc sp.boxes sp.wires;
    Logfile.write fname;
    close_out oc
