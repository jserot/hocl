-- The [pipe] higher-order function
-- It has type [('a -> 'a) list -> 'a -> 'a]
-- and can be defined as : [pipe [f1, ..., fn] x = fn (... f2 (f1 x) ...)]

net rec pipe fs x = match fs with
    [] -> x
  | f::fs' -> pipe fs' (f x);
