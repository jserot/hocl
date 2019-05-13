-- [foldl1] is a variant of [foldl] operating on non empty lists and not requiring
-- an initial [z] value
-- It has type [('a * 'a -> 'a) -> 'a list -> 'a]
-- and can be defined as : [foldl1 f [x1, x2, ..., xn] = foldl f x1 [x2, ..., xn]]
  
net rec foldl1 f l =
  match l with 
    x::xs -> foldl f x xs
;
