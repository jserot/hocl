-- The [map2f] higher-order function
-- It has type [('a -> 'b) list -> 'a list -> 'b list]
-- and can be defined as : [mapf [f1, ..., fn] [x1, ..., xn] = [f1 x2, ..., fn x2]]

net rec map2f fs xs = match (fs,xs) with
    ([],[]) -> []
  | (f::fs',x::xs') ->  f x :: map2f fs' xs'
;
