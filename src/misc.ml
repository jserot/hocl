exception Internal of string
exception Error

let fatal_error msg = raise (Internal msg)

let not_implemented what = 
  Printf.eprintf "Not implemented: %s.\n" what;
  raise Error

let rec list_map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> f a1 a2 :: list_map2 f l1 l2
  | (_, _) -> invalid_arg "list_map2"

let rec list_iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; list_iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "list_iter3"

let list_iter_index f l = 
  let rec h i = function
    [] -> ()
  | x::xs -> f i x ; h (i+1) xs in
  h 0 l

let list_map_index f l = 
  let rec h i = function
    [] -> []
  | x::xs -> f i x :: h (i+1) xs in
  h 0 l

let rec list_split3 = function
    [] -> ([], [], [])
  | (x,y,z)::l ->
      let (rx, ry, rz) = list_split3 l in (x::rx, y::ry, z::rz)

let rec foldl_index f acc l =
  let rec h i z = function
    [] -> z
  | x::xs -> let z' = f i z x in h (i+1) z' xs in
  h 0 acc l

let string_of_list f sep l =
  let rec h = function [] -> "" | [x] -> f x | x::xs -> f x ^ sep ^ h xs in
  h l

let list_make_index n f =
  let rec h i = if i >= n then [] else f i :: h (i+1) in
  h 0

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

(* [loopback [x1;x2;...] is [x1;x2;...;x1] *)

let loopback = function [] -> [] | x::xs -> (x::xs)@[x]

let file_prefix f = Filename.remove_extension (Filename.basename f)
                  
let check_dir name = 
      if not (Sys.file_exists name && Sys.is_directory name) then begin
        Printf.printf "Creating directory %s\n" name;
        Unix.mkdir name 0o777
        end
