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

let fst3 (x,_,_) = x
let fst4 (x,_,_,_) = x

let file_prefix f = Filename.remove_extension (Filename.basename f)

let check_dir name = 
      if not (Sys.file_exists name && Sys.is_directory name) then begin
        Printf.printf "Creating directory %s\n" name;
        Unix.mkdir name 0o777
        end
