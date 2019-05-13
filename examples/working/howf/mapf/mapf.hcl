-- The [mapf] higher-order function
-- It has type [('a -> 'b) list -> 'a -> 'b list]
-- and can be defined as : [mapf [f1, ..., fn] = [f1 x, ..., fn x]]

net rec mapf fs x = match fs with
    [] -> []
  | f::fs' ->  f x :: mapf fs' x
;
