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

exception Internal of string
exception Error

let source_files = ref ([] : string list)

let fatal_error msg = raise (Internal msg)

let id x = x
         
let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x

let snd4 (_,x,_,_) = x
                 
let not_implemented what = 
  Printf.eprintf "Not implemented: %s.\n" what;
  raise Error

let rec list_iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; list_iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "list_iter3"

let rec list_split3 = function
    [] -> ([], [], [])
  | (x,y,z)::l ->
      let (rx, ry, rz) = list_split3 l in (x::rx, y::ry, z::rz)

let rec foldl_index f acc l =
  let rec h i z = function
    [] -> z
  | x::xs -> let z' = f i z x in h (i+1) z' xs in
  h 0 acc l

let list_pos e l =
  let rec h i l = match l with
    | [] -> None
    | x::xs -> if e = x then Some i else h (i+1) xs in
  h 0 l

let string_of_list f sep l =
  let rec h = function [] -> "" | [x] -> f x | x::xs -> f x ^ sep ^ h xs in
  h l

let string_of_ilist f sep l =
  let rec h = function _,[] -> "" | k,[x] -> f k x | k,x::xs -> f k x ^ sep ^ h (k+1,xs) in
  h (0,l)

let string_of_two_lists f1 f2 sep l1 l2 = 
  match string_of_list f1 sep l1, string_of_list f2 sep l2 with
    "", "" -> ""
  | "", s2 -> s2
  | s1, "" -> s1
  | s1, s2 -> s1 ^ sep ^ s2

let list_make_index n f =
  let rec h i = if i >= n then [] else f i :: h (i+1) in
  h 0

let list_make n v = list_make_index n (fun _ -> v)

let list_repl n x =
  let rec h = function 0 -> [] | n -> x :: h (n-1) in
  h n

let rec output_list f oc sep = function
    [] -> ()
  | [v] -> f oc v
  | v::vs -> f oc v; output_string oc sep; output_list f oc sep vs


exception Toplevel

let opt f = function None -> None | Some v -> Some (f v)

let rec opt_list = function 
    [] -> []
  | Some x :: xs -> x :: opt_list xs
  | None :: xs -> opt_list xs

(* [filter_map p f l] is the list built by applying [f] to each element [x] of [l] such that [p x] is true *)

let rec filter_map p f l = match l with 
   [] -> []
| x::xs -> if p x then f x :: filter_map p f xs else filter_map p f xs

(* [opt_map f l] is the list built by applying [f] to each element of [l] and retaining only results [Some _].
   Ex: [opt_map (function x -> if x mod 2 then Some (x*10) else None) [1;2;3;4]] gives [[20;40]] *)
         
let opt_map f l = List.map f l |> opt_list

(* [iter_while_new f x] computes [f x], [f (f x)], ... until one result is equal to a previously computed one
   and returns the last computed result *)

let iter_while_new f x =
   let rec iter prevs x =
     if List.mem x prevs then x
     else iter (x::prevs) (f x) in
   iter [x] x

(* [map_succ f [x1;x2;x3;...]] returns [[f x1 x2; f x2 x3; ...]] *)

let rec map_succ f = function
    [] -> failwith "map_succ"
  | [x] -> failwith "map_succ"
  | [x;y] -> [f x y]
  | x::y::ys -> (f x y) :: map_succ f (y::ys)

let assoc_pos k l =
  let rec find pos l = match l with
      [] -> 0
    | (k',_)::xs -> if compare k' k = 0 then pos else find (pos+1) xs in
  find 1 l
  
(* [loopback [x1;x2;...] is [x1;x2;...;x1] *)

let loopback = function [] -> [] | x::xs -> (x::xs)@[x]

let file_prefix f = Filename.remove_extension (Filename.basename f)

let replace_suffix ?(sep='.') s f =
    let i = String.rindex f sep in
    String.sub f 0 (i+1) ^ s 
                  
let check_dir name = 
      if not (Sys.file_exists name && Sys.is_directory name) then begin
        Printf.printf "Creating directory %s\n" name;
        Unix.mkdir name 0o777
        end

let assoc_replace k f = List.map (fun (k',v) -> k', if k'=k then f v else v)
let assoc_update k v = assoc_replace k (fun _ -> v)

let compiler_version = "the HoCL compiler (version " ^ Version.version ^ ")"

let time_of_day () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "on %04d-%02d-%02d at %02d:%02d:%02d"
    (t.Unix.tm_year+1900) (t.Unix.tm_mon+1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let get_username () = try Sys.getenv "USER" with Not_found -> "<unknown>"

let dump_banner ?(source="") ?(generator="") comment oc =
  Printf.fprintf oc "%s -------------------------------------------------------------------------------\n" comment;
  Printf.fprintf oc "%s This file has been automatically generated by %s\n"
    comment (if generator <> "" then generator else compiler_version) ;
  Printf.fprintf oc "%s from file(s) %s, %s, by %s\n"
    comment
    (if source <> "" then source else string_of_list id "," !source_files)
    (time_of_day ())
    (get_username());
  Printf.fprintf oc "%s -------------------------------------------------------------------------------\n\n" comment
