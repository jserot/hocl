-- HOCL standard prelude
-- v0.1 - May 14, 2019 - JS

-- [>>] is the reverse application operator : [x >> f] is [f x]
-- It allows expressions such as [f1 (f2 (f3 x))], for ex, to be written as [x >> f1 >> f2 >> f3]

net (>>) x f = f x;

-- [|>] is a variant of [>>] : [i >> f] is [() >> i >> f]

net (|>) i f = f (i ());

-- The [repl] higher-order function
-- has type [nat -> 'a -> 'a list]
-- and can be defined as : [repl n x = [x, ..., x]]
--                                      \---v---/
--                                       n times

net rec repl n x =
  if n=0 then []
  else x :: repl (n-1) x
;

-- The [iter] higher-order function
-- It has type [nat -> ('a -> 'a) -> 'a -> 'a]
-- and can be defined as : [iter n f x = f^n x = f (... f (f x) ...)]
--                                               \--------v-------/
--                                                  n applications

net rec iter n f x =
  if n=0 then x
  else iter (n-1) f (f x)
;

-- The [miter] higher-order function is a variant of [iter] where the results of
-- the intermediate applications are output
-- It has type [nat -> ('a -> 'a) -> 'a -> 'a list]
-- and can be defined as : [miter n f x = [f x, f (f x), ..., f^n x]]

net rec miter n f x =
  if n=0 then [] 
  else let y = f x in y :: (miter (n-1) f y)
;

-- The [pipe] higher-order function
-- has type [('a -> 'a) list -> 'a -> 'a]
-- and can be defined as : [pipe [f1, ..., fn] x = fn (... f2 (f1 x) ...)]

net rec pipe fs x = match fs with
    [] -> x
  | f::fs' -> pipe fs' (f x)
;

-- The [map] higher-order function
-- has type [('a -> 'b) -> 'a list -> 'b list]
-- and can be defined as : [map f [x1, ..., xn] = [f x1, ..., f xn]]

net rec map f xs =
  match xs with 
  [] -> []
| x::xs' -> f x :: map f xs'
;

-- The [mapf] higher-order function
-- has type [('a -> 'b) list -> 'a -> 'b list]
-- and can be defined as : [mapf [f1, ..., fn] = [f1 x, ..., fn x]]

net rec mapf fs x = match fs with
    [] -> []
  | f::fs' ->  f x :: mapf fs' x
;

-- The [map2f] higher-order function
-- has type [('a -> 'b) list -> 'a list -> 'b list]
-- and can be defined as : [mapf [f1, ..., fn] [x1, ..., xn] = [f1 x2, ..., fn x2]]

net rec map2f fs xs = match (fs,xs) with
    ([],[]) -> []
  | (f::fs',x::xs') ->  f x :: map2f fs' xs'
;

-- The [foldl] higher-order wiring function
-- has type [('a * 'b -> 'a) -> 'a -> 'b list -> 'a]
-- and can be defined as : [foldl f z [x1, ..., xn] = f (... (f (z,x1), x2), ..., xn)
  
net rec foldl f z xs =
  match xs with 
  [] -> z
| x::xs' -> foldl f (f (z,x)) xs'
;

-- [foldl1] is a variant of [foldl] operating on non empty lists and not requiring
-- an initial [z] value
-- It has type [('a * 'a -> 'a) -> 'a list -> 'a]
-- and can be defined as : [foldl1 f [x1, x2, ..., xn] = foldl f x1 [x2, ..., xn]]
  
net rec foldl1 f l =
  match l with 
    x::xs -> foldl f x xs
;

-- The [foldr] higher-order wiring function
-- has type [('a * 'b -> 'b) -> 'b -> 'a list -> 'b]
-- and can be defined as : [foldr f [x1, ..., xn] z = f (x1, ... (f (xn,z), xn-1), ...)]
  
net rec foldr f z xs =
  match xs with
    [] -> z
  | x::xs' -> f (x, foldr f z xs')
;
