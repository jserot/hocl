-- The [map] higher-order function
-- It has type [('a -> 'b) -> 'a list -> 'b list]
-- and can be defined as : [map f [x1, ..., xn] = [f x1, ..., f xn]]

net rec map f xs =
  match xs with 
  [] -> []
| x::xs' -> f x :: map f xs'
;
