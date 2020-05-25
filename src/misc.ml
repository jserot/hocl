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

exception Error

let source_files = ref ([] : string list)

let fatal_error msg = failwith msg
                    
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

let string_of_array f sep a = string_of_list f sep (Array.to_list a)
let string_of_iarray f sep a = string_of_ilist f sep (Array.to_list a)
                            
let fold_lefti f z l =
  let rec h i z l = match l with
      [] -> z
    | x::xs -> h (i+1) (f i z x) xs in
  h 0 z l

let fold_left1 f l =
  match l with
    [] -> invalid_arg "Misc.fold_left1"
  | x::xs -> List.fold_left f x xs 

let rec list_split3 = function
    [] -> ([], [], [])
  | (x,y,z)::l ->
      let (rx, ry, rz) = list_split3 l in (x::rx, y::ry, z::rz)

let flat_map f l = List.flatten @@ List.map f l

let fold_left_map f accu l =  (* Waiting for 4.11 *)
  let rec aux accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l in
  aux accu [] l

let map2i f l1 l2 =
  List.fold_left2
    (fun (i,acc) x1 x2 -> (i+1, f i x1 x2 :: acc))
    (0,[]) l1 l2
  |> snd
  |> List.rev

let array_replace f i a =
  let a' = Array.copy a in
  a'.(i) <- f a.(i);
  a'

let array_foldli f z a = Array.to_list a |> fold_lefti f z
                 
let list_merge l1 l2 =
  (* [list_merge l1 l2] merges two lists [l1] and [l2], merging any element occuring in both lists *)
    l1
  @ List.filter (fun x -> not (List.mem x l1)) l2
  
let assoc_merge f l1 l2 =
  (* [assoc_merge f l1 l2] merges two association lists [l1] and [l2], using function [f]
     to merge values occuring in both lists with the same key *)
  List.map
    (fun (k1,v1) ->
      match List.assoc_opt k1 l2 with
      | None -> k1, v1
      | Some v2 -> k1, f v1 v2)
    l1
  @ List.filter
      (fun (k2,_) -> not (List.mem_assoc k2 l1))
      l2

let update_assoc l f k =
  let rec h l = match l with
    | [] -> []
    | (k',v)::rest -> if k=k' then (k, f v)::rest else (k',v)::h rest in
  h l

let replace_assoc l k v' = update_assoc l (fun _ -> v') k

let assoc_pos k l =
  let rec find pos l = match l with
      [] -> 0
    | (k',_)::xs -> if compare k' k = 0 then pos else find (pos+1) xs in
  find 1 l

let fst3 (x,_,_) = x
let fst4 (x,_,_,_) = x

let file_prefix f = Filename.remove_extension (Filename.basename f)

let check_dir name = 
      if not (Sys.file_exists name && Sys.is_directory name) then begin
        Printf.printf "Creating directory %s\n" name;
        Unix.mkdir name 0o777
        end

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
    (if source <> "" then source else string_of_list Fun.id "," !source_files)
    (time_of_day ())
    (get_username());
  Printf.fprintf oc "%s -------------------------------------------------------------------------------\n\n" comment

let list_extract p l =
  (* If [l] contains an element [x] such as [p x] is true, then [list_extract p l] returns a pair [(Some x,l')]
     where [l'] is the list obtained by removing [x] from [l].
     Otherwise, [list_extract p l] returns the pair [(None,l)].
     If [l] contains several elements [x] such as [p x] is true, only the first element is returned (resp. removed) *)
  let rec scan acc l = match l with
    | [] -> None, acc
    | x::xs -> if p x then Some x, acc@xs else scan (acc@[x]) xs in
  scan [] l
