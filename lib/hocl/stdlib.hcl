-- HOCL standard library
-- v1.2 - May 4, 2020 - JS

-- [|>] is the reverse application operator : [x |> f] is [f x]
-- It allows expressions such as [f1 (f2 (f3 x))], for ex, to be written as [x |> f1 |> f2 |> f3]

val (|>) x f = f x;

-- [|->] is a variant of [|>] : [i |-> f] is [() |> i |> f]

val (|->) i f = f (i ());

-- [@@] is the classical function composition operator

val ( @@ ) g f x = g (f x);

-- The [repl] higher-order function
-- has type [int -> $t -> $t list]
-- and can be defined as : [repl n x = [x, ..., x]]
--                                      \---v---/
--                                       n times

val rec repl n x =
  if n=0 then []
  else x :: repl (n-1) x
;

-- The [iter] higher-order function
-- It has type [int -> ($t -> $t) -> $t -> $t]
-- and can be defined as : [iter n f x = f^n x = f (... f (f x) ...)]
--                                               \--------v-------/
--                                                  n applications

val rec iter n f x =
  if n=0 then x
  else iter (n-1) f (f x)
;

-- The [miter] higher-order function is a variant of [iter] where the results of
-- the intermediate applications are output
-- It has type [int -> ($t -> $t) -> $t -> $t list]
-- and can be defined as : [miter n f x = [f x, f (f x), ..., f^n x]]

val rec miter n f x =
  if n=0 then [] 
  else let y = f x in y :: (miter (n-1) f y)
;

-- The [pipe] higher-order function
-- has type [($t -> $t) list -> $t -> $t]
-- and can be defined as : [pipe [f1, ..., fn] x = fn (... f2 (f1 x) ...)]

val rec pipe fs x = match fs with
    [] -> x
  | f::fs -> pipe fs (f x)
;

-- The [map] higher-order function
-- has type [($t1 -> $t2) -> $t1 list -> $t2 list]
-- and can be defined as : [map f [x1, ..., xn] = [f x1, ..., f xn]]

val rec map f xs =
  match xs with 
  [] -> []
| x::xs -> f x :: map f xs
;

-- The [mapf] higher-order function
-- has type [($t1 -> $t2) list -> $t1 -> $t2 list]
-- and can be defined as : [mapf [f1, ..., fn] = [f1 x, ..., fn x]]

val rec mapf fs x = match fs with
    [] -> []
  | f::fs ->  f x :: mapf fs x
;

-- The [map2f] higher-order function
-- has type [($t1 -> $t2) list -> $t1 list -> $t2 list]
-- and can be defined as : [mapf [f1, ..., fn] [x1, ..., xn] = [f1 x2, ..., fn x2]]

val rec map2f fs xs = match (fs,xs) with
    ([],[]) -> []
  | (f::fs,x::xs) ->  f x :: map2f fs xs
;

-- The [foldl] higher-order wiring function
-- has type [($t1 * $t2 -> $t1) -> $t1 -> $t2 list -> $t1]
-- and can be defined as : [foldl f z [x1, ..., xn] = f (... (f (z,x1), x2), ..., xn)
  
val rec foldl f z xs =
  match xs with 
  [] -> z
| x::xs -> foldl f (f (z,x)) xs
;

-- [foldl1] is a variant of [foldl] operating on non empty lists and not requiring
-- an initial [z] value
-- It has type [($t * $t -> $t) -> $t list -> $t]
-- and can be defined as : [foldl1 f [x1, x2, ..., xn] = foldl f x1 [x2, ..., xn]]
  
val rec foldl1 f l =
  match l with 
    x::xs -> foldl f x xs
;

-- The [foldr] higher-order wiring function
-- has type [($t1 * $t2 -> $t2) -> $t2 -> $t1 list -> $t2]
-- and can be defined as : [foldr f [x1, ..., xn] z = f (x1, ... (f (xn,z), xn-1), ...)]
  
val rec foldr f z xs =
  match xs with
    [] -> z
  | x::xs -> f (x, foldr f z xs)
;
